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
%access public

SessionID : Type
SessionID = Int

data SerialisedSessionType = SInt
                           | SString
                           | SBool
                           | SNull

total
interpSerialisedTy : SerialisedSessionType -> Type
interpSerialisedTy SInt = Int
interpSerialisedTy SString = String
interpSerialisedTy SBool = Bool
interpSerialisedTy SNull = ()


total
showSerialisedVal : (a : SerialisedSessionType) -> (interpSerialisedTy a) -> String
showSerialisedVal SInt i = show i
showSerialisedVal SString s = s
showSerialisedVal SBool b = show b
showSerialisedVal SNull _ = ""

-- TODO: Use the monadic parser instead
total
castSerialisedVal : (a : SerialisedSessionType) -> String -> (interpSerialisedTy a) 
castSerialisedVal SString s = s
castSerialisedVal SInt i = castInt i
  where castInt : String -> Int
        castInt i = cast i
castSerialisedVal SBool b = castBool b
  where castBool : String -> Bool
        castBool "True" = True
        castBool "False" = False
        castBool _ = False -- -_- gonna have to do something about this
castSerialisedVal SNull _ = ()

public
SerialisedSession : Type
SerialisedSession = List (String, String)

data SessionStep = Uninitialised -- before trying to retrieve from the DB
                 | Initialised -- after successfully retrieving session data
                 | Invalid -- if session data is invalid

data SessionData : Type -> Type where
  UninitialisedSession : (session_id : SessionID) -> SessionData a
  ValidInitialisedSession : (session_id : SessionID) -> a -> SessionData a
  InvalidInitialisedSession : SessionData a

  --MkSessionData : (session_id : SessionID) ->
     --             (session_data : a) -> SessionData a

data SessionRes : (s : SessionStep) -> Type -> Type where
  SRes : SessionData a -> SessionRes s a 
  --DisposedSession : 

record MySampleSessionData : Type where
  MySessionData : (username : String) ->
                  (age : Int) -> 
                  (male : Bool) -> MySampleSessionData


-- getSessionData : Eff m [SESSION (ValidSession Initialised a)] a

-- Retrieves session data as a list of (String, String) k-v pairs.
-- We marshal this back to the required types in a later function.
collectResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] (List (String, String))
collectResults = do
  step_result <- nextRow
  case step_result of
      StepComplete => do key <- getColumnText 1
                         val <- getColumnText 2
                         xs <- collectResults
                         Effects.pure $ (key, val) :: xs
      NoMoreRows => Effects.pure []
      StepFail => Effects.pure []

retrieveSessionData : SessionID -> Eff IO [SQLITE ()] (Either String (List (String, String)))
retrieveSessionData s_id = do
  open_db <- openDB "sessions.db"
  if open_db then do
    let sql = "SELECT key, val FROM `sessiondata` WHERE `session_id` = ?"
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

getSession : (tys : Vect SerialisedSessionType n) -> (names : Vect String n) -> SessionID -> IO (Maybe (interpSerialisedTys tys))
getSession tys names id = do db_res <- run [()] (retrieveSessionData id)
                             case db_res of
                                  Left err => pure Nothing
                                  Right xs => pure Nothing --pure $ 


data Session : Effect where
  ---NewSession : NoSession a -> 
  -- Retrieves session information from the database, and marshals it into the given
  -- format.
  RetrieveSessionData : Session (SessionRes Uninitialised a) (SessionRes Initialised a) a
  -- 
  DeleteSession : Session (SessionRes Initialised a) (SessionRes Invalid a) () -- Possibly have a disposed state? Hm
  UpdateSession : a -> Session (SessionRes Initialised a) (SessionRes Initialised a) ()
{-
instance Handler Session IO where
  handle (UninitialisedSession s_id) RetrieveSessionData = do

  handle (ValidInitialisedSession s_id sd) 
  -}
--total
interpSerialisedTys : (tys : Vect SerialisedSessionType n) -> Type
interpSerialisedTys [] = ()
interpSerialisedTys [x] = interpSerialisedTy x
interpSerialisedTys (x :: (y :: xs)) = (interpSerialisedTy x, interpSerialisedTys (y :: xs))

serialise : (tys' : Vect SerialisedSessionType n) -> Vect String n -> interpSerialisedTys tys' -> SerialisedSession
serialise [] _ _ = []
serialise [ty] [name] val = [("name", showSerialisedVal ty val)]
serialise (ty :: (ytys :: tys)) (name :: (ynames :: names)) (x, y) = 
  (name, showSerialisedVal ty x) :: serialise (ytys :: tys) (ynames :: names) y

--deserialise : (tys : Vect SerialisedSessionType n) -> Vect String n -> List (String, String) -> interpSerialisedTys tys
--deserialise [] _ _ 
