{- IdrisWeb Session System
   Makes use of effects library to allow for persistent sessions.

   Exploits to prevent against:
    - Fixation
      - Session ID *MUST* be regenerated when the user logs in.
    - Brute force
      - Big numbers, cryptographically secure and random
    - Sniffing (also todo: SSL)
      - Only allow sessions to be sent over an SSL connection
-}
module IdrisWeb.Session.Session
import IdrisWeb.DB.SQLite.SQLiteNew
import Effects
import IdrisWeb.Common.Random.RandC
import SimpleParser
%access public

-- SessionID should be some long-ish random string (hash?)
SessionID : Type
SessionID = String

private
DB_NAME : String
DB_NAME = "/tmp/sessions.db"

-- I think in this circumstance, tagged data types
-- would be better, since we're not passing directly
-- to a function, more just providing other functions
-- with data.
data SessionDataType = SInt Int
                     | SString String
                     | SBool Bool
                     | SNull 
total
showSerialisedVal : SessionDataType -> (String, String)
showSerialisedVal (SInt i) = ("SInt", show i)
showSerialisedVal (SString s) = ("SString", s)
showSerialisedVal (SBool b) = ("SBool", show b)
showSerialisedVal (SNull) = ("SNull", "")

-- Given a serialised value from the DB, construct
-- the appropriate data type.
-- TODO: Probably a better way of doing it than storing the
-- type as a string in the DB: an enum would likely be better
--total
deserialiseVal : String -> String -> Maybe SessionDataType
deserialiseVal tystr s =
  if tystr == "SInt" then case parse int s of
                               Left err => Nothing
                               Right (i, _) => Just $ SInt i
  else if tystr == "SString" then Just $ SString s
  else if tystr == "SBool" then case parse bool s of
                                     Left err => Nothing
                                     Right (b, _) => Just $ SBool b
  else if tystr == "SNull" then Just SNull
  else Nothing

-- SerialisedSession is a list of 3-tuples of <Key, Value, Type>.
SerialisedSessionEntry : Type
SerialisedSessionEntry = (String, String, String)

public -- this really shouldn't be public, TODO: change
SerialisedSession : Type
SerialisedSession = List SerialisedSessionEntry

-- SessionData is the user-facing data type, containing the session names and variables 
public
SessionData : Type
SessionData = List (String, SessionDataType)

{-
deserialiseSession : SerialisedSession -> Maybe SessionData
deserialiseSession ss = sequence $ map (\(key, val, ty) => case (deserialiseVal ty val) of
  Just dat => Just (key, dat)
  Nothing => Nothing) ss
-}

deserialiseSession : SerialisedSession -> Maybe SessionData
deserialiseSession ss = traverse (\(key, val, ty) => case (deserialiseVal ty val) of
                                                       Just dat => Just (key, dat)
                                                       Nothing => Nothing) ss


-- showSerialisedVal : (String, String)
serialiseSession : SessionData -> SerialisedSession
serialiseSession sd = map (\(key, sdt) => let (tystr, valstr) = showSerialisedVal sdt in 
                                              (key, valstr, tystr)) sd




-- Retrieves session data as a list of (String, String) k-v pairs.
-- We marshal this back to the required types in a later function.
collectResults : EffM IO [SQLITE (Either (SQLiteExecuting InvalidRow) (SQLiteExecuting ValidRow))] 
                         [SQLITE (SQLiteExecuting InvalidRow)] 
                 SerialisedSession
collectResults = 
  if_valid then do
    key <- getColumnText 0
    val <- getColumnText 1
    ty <- getColumnText 2
    step_result <- nextRow
    xs <- collectResults
    Effects.pure $ (key, val, ty) :: xs
  else Effects.pure [] 


retrieveSessionData : SessionID -> Eff IO [SQLITE ()] (Either QueryError SerialisedSession)
retrieveSessionData s_id = do
  conn_res <- openDB DB_NAME
  if_valid then do
    let sql = "SELECT key, val, ty FROM `sessiondata` WHERE `session_key` = ?"
    ps_res <- prepareStatement sql
    if_valid then do
      bindText 1 s_id
      bind_res <- finishBind
      if_valid then do 
        executeStatement
        results <- collectResults
        finaliseInvalid
        closeDB
        Effects.pure $ Right results
      else do
        let be = getBindError bind_res
        cleanupBindFail
        Effects.pure $ Left be
    else do
      cleanupPSFail
      Effects.pure . Left $ getQueryError ps_res
  else 
    Effects.pure . Left $ getQueryError conn_res

--removeSessionData : SessionID -> Eff IO [SQLITE ()] 

getInsertArg : SerialisedSession -> String
getInsertArg [] = ""
-- no comma needed at the end
getInsertArg ((key, val, ty) :: []) = "(\"" ++ key ++ "\", \"" ++ val ++ "\", \"" ++ ty ++ "\")" 
getInsertArg ((key, val, ty) :: xs) = "(\"" ++ key ++ "\", \"" ++ val ++ "\", \"" ++ ty ++ "\")" ++ ", " ++ (getInsertArg xs)
 

storeSessionRow : SessionID -> SerialisedSessionEntry -> Eff IO [SQLITE ()] (Either QueryError ())
storeSessionRow s_id (key, val, ty) = do
  conn_res <- openDB DB_NAME
  if_valid then do
    let insert_sql = "INSERT INTO `sessiondata` (`session_key`, `key`, `val`, `ty`) VALUES (?, ?, ?, ?)"
    ps_res <- prepareStatement insert_sql
    if_valid then do
      -- Bind the arguments to the prepared statement
      bindText 1 s_id
      bindText 2 key
      bindText 3 val
      bindText 4 ty
      bind_res <- finishBind
      if_valid then do
        executeStatement
        finalise
        closeDB
        Effects.pure $ Right ()
      else do
        let be = getBindError bind_res
        cleanupBindFail
        Effects.pure $ Left be
    else do
      cleanupPSFail
      Effects.pure . Left $ getQueryError ps_res
  else 
    Effects.pure . Left $ getQueryError conn_res
  
storeSessionData : SessionID -> SerialisedSession -> Eff IO [SQLITE ()] (Either QueryError ())
storeSessionData s_id [] = Effects.pure $ Right ()
storeSessionData s_id (sr :: srs) = do res <- storeSessionRow s_id sr
                                       case res of
                                            Left err => Effects.pure $ Left err
                                            Right () => storeSessionRow s_id srs

removeSession: SessionID -> Eff IO [SQLITE ()] (Either QueryError ())
removeSession s_id = do
  conn_res <- openDB DB_NAME
  if_valid then do
    let delete_sql = "DELETE FROM `sessiondata` WHERE `session_key` = ?"
    ps_res <- prepareStatement delete_sql
    if_valid then do
      bindText 1 s_id
      bind_res <- finishBind
      if_valid then do
        executeStatement
        finalise
        closeDB
        Effects.pure $ Right ()
      else do
        let be = getBindError bind_res
        cleanupBindFail
        Effects.pure $ Left be
    else do
      cleanupPSFail
      Effects.pure . Left $ getQueryError ps_res
  else 
    Effects.pure . Left $ getQueryError conn_res
    
-- Remove then store
updateSessionData : SessionID -> SessionData -> Eff IO [SQLITE ()] (Either QueryError ())
updateSessionData s_id sd = do
  del_res <- removeSession s_id
  case del_res of
       Left err => Effects.pure $ Left err
       Right () => do store_res <- storeSessionData s_id (serialiseSession sd)
                      case store_res of
                        Left err' => Effects.pure $ Left err'
                        Right () => Effects.pure $ Right ()


getSession : SessionID -> IO (Maybe SessionData)
getSession s_id = do db_res <- run [()] (retrieveSessionData s_id)
                     case db_res of
                          Left err => pure Nothing
                          Right ss => pure $ deserialiseSession ss

{- Session effect:
   We should be able to create, update and delete sessions.
   We should only be able to update and delete valid sessions.
   We should only be able to create sessions when we don't have an active session.
   We really should only be able to populate a session after authentication 
      if we generate a new session (in order to prevent session fixation attacks (but how... hmmmm)
-} 

data SessionStep = SessionUninitialised
                 | SessionInitialised

public
data SessionRes : SessionStep -> Type where
  InvalidSession : SessionRes s
  ValidSession : SessionID -> SessionData -> SessionRes s


data Session : Effect where
  -- Load a session from the database, given a session ID.
  LoadSession : SessionID -> Session (SessionRes SessionUninitialised) (SessionRes SessionInitialised) (Maybe SessionData)
  -- Updates the in-memory representation of the session
  UpdateSession : SessionData -> Session (SessionRes SessionInitialised) (SessionRes SessionInitialised) ()
  -- Given a session data set, creates a new session
  CreateSession : SessionData -> Session (SessionRes SessionUninitialised) (SessionRes SessionInitialised) (Maybe SessionID)
  -- Delete the current session
  DeleteSession : Session (SessionRes SessionInitialised) (SessionRes SessionUninitialised) Bool -- Hmmm... Error handling? How?
  -- Updates the DB with the new session data, discards the in-memory resources
  WriteToDB : Session (SessionRes SessionInitialised) (SessionRes SessionUninitialised) Bool
  -- Discards changes to the current session, disposes of resources
  DiscardSessionChanges : Session (SessionRes SessionInitialised) (SessionRes SessionUninitialised) ()

  GetSessionID : Session (SessionRes SessionInitialised) (SessionRes SessionInitialised) (Maybe SessionID)
  
  GetSessionData : Session (SessionRes SessionInitialised) (SessionRes SessionInitialised) (Maybe SessionData)

SESSION : Type -> EFFECT
SESSION t = MkEff t Session

loadSession : SessionID -> EffM m [SESSION (SessionRes SessionUninitialised)]
                                  [SESSION (SessionRes SessionInitialised)] 
                                  (Maybe SessionData)
loadSession s_id = (LoadSession s_id)

updateSession : SessionData -> Eff m [SESSION (SessionRes SessionInitialised)] ()
updateSession sd = (UpdateSession sd)

createSession : SessionData -> EffM m [SESSION (SessionRes SessionUninitialised)]
                                      [SESSION (SessionRes SessionInitialised)]
                                      (Maybe SessionID)
createSession sd = (CreateSession sd)

deleteSession : EffM m [SESSION (SessionRes SessionInitialised)] 
                       [SESSION (SessionRes SessionUninitialised)]
                       Bool
deleteSession = DeleteSession 

writeSessionToDB : EffM m [SESSION (SessionRes SessionInitialised)] 
                          [SESSION (SessionRes SessionUninitialised)]
                          Bool
writeSessionToDB = WriteToDB

discardSession : EffM m [SESSION (SessionRes SessionInitialised)] 
                        [SESSION (SessionRes SessionUninitialised)]
                        ()
discardSession = DiscardSessionChanges

getSessionID : Eff m [SESSION (SessionRes SessionInitialised)] 
                     (Maybe SessionID)
getSessionID = GetSessionID

getSessionData : Eff m [SESSION (SessionRes SessionInitialised)]
                       (Maybe SessionData)
getSessionData = GetSessionData

instance Handler Session IO where
  -- Grab the session from the DB given the session key.
  -- If it exists, construct the resource and return the data.
  -- If not, return nothing, and reflect the invalidity in the resource.

  -- This should never happen
  handle (ValidSession _ _) (LoadSession _) k = k InvalidSession Nothing

  handle InvalidSession (LoadSession s_id) k = do
    maybe_session <- getSession s_id
    case maybe_session of
         Just s_data => k (ValidSession s_id s_data) (Just s_data)
         Nothing => k InvalidSession Nothing

  -- Update the in-memory representation of the session.
  handle (ValidSession s_id s_dat) (UpdateSession s_dat') k = 
    k (ValidSession s_id s_dat') ()

  -- If we're trying to update an invalid session, just let it fall
  -- through.
  handle (InvalidSession) (UpdateSession _) k =
   k (InvalidSession) () 

  -- Delete a session from the database, and dispose of our resources.
  handle (ValidSession s_id _) DeleteSession k = do
    delete_res <- run [()] (removeSession s_id)
    case delete_res of
         Left err => k InvalidSession False
         Right () => k InvalidSession True

  handle (InvalidSession) DeleteSession k = k InvalidSession False

  -- Writes a session to the DB, and disposes of the in-memory resources
  handle (ValidSession s_id s_dat) WriteToDB k = do
    update_res <- run [()] (updateSessionData s_id s_dat)
    case update_res of
         Left err => do putStrLn (show err) 
                        k InvalidSession False
         Right () => k InvalidSession True
         
  handle InvalidSession WriteToDB k = k InvalidSession False

  -- Simply discard the resource without doing any writes
  handle (ValidSession _ _) DiscardSessionChanges k = k InvalidSession ()
  handle (InvalidSession) DiscardSessionChanges k = k InvalidSession ()

  handle (ValidSession _ _) (CreateSession _) k = k InvalidSession Nothing
  -- Creates a new session.
  -- BIG TODO: This random number gen is extremely rudimentary, and not
  -- secure enough for actual use.
  -- We've also got no guarantees that the IDs generated will be unique... 
  -- This can be fixed by having some sort of property variable in the session
  -- DB, which we increment each time, and hash alongside the random number.
  -- While OK for a quick prototype, this *REALLY* must be fixed.
  handle InvalidSession (CreateSession sd) k = do
    rand_id <- getRandom 1000000000 21474836476 -- FIXME: This is a pathetic level of entropy...
    let s_id = show rand_id -- Some hash function would be here, typically
    store_res <- run [()] (storeSessionData s_id (serialiseSession sd))
    case store_res of
      Left err' => k InvalidSession Nothing
      Right () => k (ValidSession s_id sd) (Just s_id)

  handle (ValidSession s_id s_dat) GetSessionID k = k (ValidSession s_id s_dat) (Just s_id)
  handle (ValidSession s_id s_dat) GetSessionData k = k (ValidSession s_id s_dat) (Just s_dat)
  handle InvalidSession GetSessionID k = k InvalidSession Nothing
  handle InvalidSession GetSessionData k = k InvalidSession Nothing
