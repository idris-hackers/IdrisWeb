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
%access public

SessionID : Type
SessionID = Int

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

-- Retrieves session data as a list of (String, String) k-v pairs.
-- We marshal this back to the required types in a later function.
collectResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] SerialisedSession
collectResults = do
  step_result <- nextRow
  case step_result of
      StepComplete => do key <- getColumnText 1
                         val <- getColumnText 2
                         ty <- getColumnText 3
                         xs <- collectResults
                         Effects.pure $ (key, val, ty) :: xs
      NoMoreRows => Effects.pure []
      StepFail => Effects.pure []

retrieveSessionData : SessionID -> Eff IO [SQLITE ()] (Either String SerialisedSession)
retrieveSessionData s_id = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "SELECT key, val, ty FROM `sessiondata` WHERE `session_id` = ?"
    sql_prep_res <- prepareStatement sql
    if sql_prep_res then do
      startBind
      bindInt 1 s_id
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
 

storeSession : SessionID -> SerialisedSession -> Eff IO [SQLITE ConnectionOpened] (Either String ())
storeSession _ [] = Effects.pure $ Right () -- If it's an empty session, we really don't have to do anything
storeSession s_id ss = do
  let insert_sql = "INSERT INTO `sessiondata` (`key`, `val`, `ty`) VALUES ?"
  sql_prep_res <- prepareStatement sql
  if sql_prep_res then do
    startBind
    bindString 1 getInsertArg 

storeSessionData : SessionID -> SerialisedSession -> Eff IO [SQLITE ()] (Either String SerialisedSession)
storeSessionData s_id sd = do
  open_db <- openDB DB_NAME
  if open_db then do
    
  
-- Remove then store
updateSessionData : SessionID -> SessionData -> Eff IO [SQLITE ()] (Either String SerialisedSession)
-}
{-
getSession : (tys : Vect SessionDataType n) -> (names : Vect String n) -> SessionID -> IO (Maybe (interpSerialisedTys tys))
getSession tys names id = do db_res <- run [()] (retrieveSessionData id)
                             case db_res of
                                  Left err => pure Nothing
                                  Right xs => pure Nothing --pure $ 
                                             -}

-- TODO: This should be a common definition somewhere, I think
mapM : Monad m => (a -> m b) -> List a -> m (List b)
mapM fn xs = sequence $ map fn xs

deserialiseSession : SerialisedSession -> Maybe SessionData
deserialiseSession ss = mapM (\(key, val, ty) => case (deserialiseVal ty val) of
                                                      Just dat => Just (key, dat)
                                                      Nothing => Nothing) ss


getSession : SessionID -> IO (Maybe SessionData)
getSession s_id = do db_res <- run [()] (retrieveSessionData s_id)
                     case db_res of
                          Left err => pure Nothing
                          Right ss => pure $ deserialiseSession ss


-- showSerialisedVal : (String, String)
serialiseSession : SessionData -> SerialisedSession
serialiseSession sd = map (\(key, sdt) => let (tystr, valstr) = showSerialisedVal sd in 
                                              (key, valstr, tystr)) sd


{- Session effect:
   We should be able to create, update and delete sessions.
   We should only be able to update and delete valid sessions.
   We should only be able to create sessions when we don't have an active session.
   We should only
-} 
