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
import SQLite
import Effects
import Parser
import RandC
%access public

-- SessionID should be some long-ish random string (hash?)
SessionID : Type
SessionID = String

DB_NAME : String
DB_NAME = "sessions.db"

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
collectResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] SerialisedSession
collectResults = do
  step_result <- nextRow
  case step_result of
      StepComplete => do sess_key <- getColumnText 1
                         key <- getColumnText 2
                         val <- getColumnText 3
                         ty <- getColumnText 4
                         xs <- collectResults
                         Effects.pure $ (key, val, ty) :: xs
      NoMoreRows => Effects.pure []
      StepFail => Effects.pure []

-- TODO: We should have a separate table for Session ID -> Expiry, and not return the data if
-- the session has expired
retrieveSessionData : SessionID -> Eff IO [SQLITE ()] (Either String SerialisedSession)
retrieveSessionData s_id = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "SELECT ses_key, key, val, ty FROM `sessiondata` WHERE `session_id` = ?"
    sql_prep_res <- prepareStatement sql
    if sql_prep_res then do
      startBind
      bindText 1 s_id
      bind_res <- finishBind
      if bind_res then do 
        beginExecution
        results <- collectResults
        finaliseStatement
        closeDB
        Effects.pure $ Right results
      else do 
        err <- bindFail
        Effects.pure $ Left err
    else do 
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err

--removeSessionData : SessionID -> Eff IO [SQLITE ()] 

getInsertArg : SerialisedSession -> String
getInsertArg [] = ""
-- no comma needed at the end
getInsertArg ((key, val, ty) :: []) = "(\"" ++ key ++ "\", \"" ++ val ++ "\", \"" ++ ty ++ "\")" 
getInsertArg ((key, val, ty) :: xs) = "(\"" ++ key ++ "\", \"" ++ val ++ "\", \"" ++ ty ++ "\")" ++ ", " ++ (getInsertArg xs)
 

storeSessionRow : SessionID -> SerialisedSessionEntry -> Eff IO [SQLITE ()] (Either String ())
storeSessionRow s_id (key, val, ty) = do
  open_db <- openDB DB_NAME
  if open_db then do
    let insert_sql = "INSERT INTO `sessiondata` (`session_key`, `key`, `val`, `ty`) VALUES (?, ?, ?)"
    sql_prep_res <- prepareStatement insert_sql
    if sql_prep_res then do
      startBind
      -- Bind the arguments to the prepared statement
      bindText 1 s_id
      bindText 2 key
      bindText 3 val
      bindText 4 ty
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        nextRow
        finaliseStatement
        closeDB
        Effects.pure $ Right ()
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err
  
storeSessionData : SessionID -> SerialisedSession -> Eff IO [SQLITE ()] (Either String ())
-- Possible bug: this isn't playing nice. It should be, but it isn't.
--storeSessionData s_id ss = (map (\sd => storeSessionRow s_id sd) ss) --traverse (\sd => storeSessionRow s_id sd) ss
storeSessionData s_id [] = Effects.pure $ Right ()
storeSessionData s_id (sr :: srs) = do res <- storeSessionRow s_id sr
                                       case res of
                                            Left err => Effects.pure $ Left err
                                            Right () => Effects.pure $ Right ()

deleteSession : SessionID -> Eff IO [SQLITE ()] (Either String ())
deleteSession s_id = do
  open_db <- openDB DB_NAME
  if open_db then do
    let delete_sql = "DELETE FROM `sessiondata` WHERE `session_id` = ?"
    sql_prep_res <- prepareStatement delete_sql
    if sql_prep_res then do
      startBind
      bindText 1 s_id
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        nextRow
        finaliseStatement
        closeDB
        Effects.pure $ Right ()
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err
    
-- Remove then store
updateSessionData : SessionID -> SessionData -> Eff IO [SQLITE ()] (Either String ())
updateSessionData s_id sd = do
  del_res <- deleteSession s_id
  case del_res of
       Left err => Effects.pure $ Left ("Error deleting: " ++ err)
       Right () => do store_res <- storeSessionData s_id (serialiseSession sd)
                      case store_res of
                        Left err' => Effects.pure $ Left ("Error storing: " ++ err')
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

data SessionStep = Uninitialised
                 | Initialised

abstract
data SessionRes : SessionStep -> Type where
  InvalidSession : SessionRes s
  ValidSession : SessionID -> SessionData -> SessionRes s


data Session : Effect where
  -- Load a session from the database, given a session ID.
  LoadSession : SessionID -> Session (SessionRes Uninitialised) (SessionRes Initialised) (Maybe SessionData)
  -- Updates the in-memory representation of the session
  UpdateSession : SessionData -> Session (SessionRes Initialised) (SessionRes Initialised) ()
  -- Given a session data set, creates a new session
  CreateSession : SessionData -> Session (SessionRes Uninitialised) (SessionRes Initialised) (Maybe SessionID)
  -- Delete the current session
  DeleteSession : Session (SessionRes Initialised) (SessionRes Uninitialised) Bool -- Hmmm... Error handling? How?
  -- Updates the DB with the new session data, discards the in-memory resources
  WriteToDB : Session (SessionRes Initialised) (SessionRes Uninitialised) Bool
  -- Discards changes to the current session, disposes of resources
  DiscardSessionChanges : Session (SessionRes Initialised) (SessionRes Uninitialised) ()


SESSION : Type -> EFFECT
SESSION t = MkEff t Session

instance Handler Session IO where
  -- Grab the session from the DB given the session key.
  -- If it exists, construct the resource and return the data.
  -- If not, return nothing, and reflect the invalidity in the resource.
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
    delete_res <- run [()] (deleteSession s_id)
    case delete_res of
         Left err => k InvalidSession False
         Right () => k InvalidSession True

  handle (InvalidSession) DeleteSession k = k InvalidSession False

  -- Writes a session to the DB, and disposes of the in-memory resources
  handle (ValidSession s_id s_dat) WriteToDB k = do
    update_res <- run [()] (updateSessionData s_id s_dat)
    case update_res of
         Left err => k InvalidSession False
         Right () => k InvalidSession True
         
  handle InvalidSession WriteToDB k = k InvalidSession False

  -- Simply discard the resource without doing any writes
  handle (ValidSession _ _) DiscardSessionChanges k = k InvalidSession ()
  handle (InvalidSession) DiscardSessionChanges k = k InvalidSession ()

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
