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
SerialisedSession : Type
SerialisedSession = List (String, (SerialisedSessionType, String))

data SessionStep = Uninitialised -- before trying to retrieve from the DB
                 | Initialised -- after successfully retrieving session data
                 | Invalid -- if session data is invalid

data SessionData : Type -> Type where
  UninitialisedSession : (session_id : SessionID) -> SessionData a
  InitialisedSession : (session_id : SessionID) -> a -> SessionData a
  NoSession : SessionData a

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

data Session : Effect where
  -- Retrieves session information from the database, and marshals it into the given
  -- format.
  -- The database rows will be of the form [(Key, Value, DBType)].
  -- The user must therefore -- 
  RetrieveSessionData : (SerialisedSession -> a) -> Session (SessionRes Uninitialised a) (SessionRes Initialised a) ()
  -- 
  DeleteSession : Session (SessionRes Initialised a) (SessionRes Invalid a) () -- Possibly have a disposed state? Hm
  UpdateSession : a -> Session (SessionRes Initialised a) (SessionRes Initialised a) ()


serialise : (a : SerialisedSessionType) -> String -> interpSerialisedTy (a) -> (String, (SerialisedSessionType, String))
serialise ty key val = (key, (ty, (showSerialisedVal ty val)))

deserialise : (a : SerialisedSessionType) -> String -> interpSerialisedTy (a)
deserialise ty str = castSerialisedVal ty str

serialiseSampleData : MySampleSessionData -> SerialisedSession 
serialiseSampleData mssd = [serialise SString "username" (username mssd), 
                            serialise SInt "age" (age mssd),
                            serialise SBool "male" (male mssd)]

deserialiseSampleData : SerialisedSession -> Maybe MySampleSessionData
deserialiseSampleData ss = do (_, ss_un) <-  lookup "username" ss
                              (_, ss_age) <- lookup "age" ss
                              (_, ss_male) <- lookup "male" ss
                              pure $ MySessionData (deserialise SString ss_un) 
                                                   (deserialise SInt ss_age) 
                                                   (deserialise SBool ss_male)
                              
sampleSessionData : SessionRes Initialised MySampleSessionData
sampleSessionData = SRes (InitialisedSession 5 (MySessionData "hi" 1 True))

sampleSessionData' : SessionRes Uninitialised MySampleSessionData
sampleSessionData' = SRes (UninitialisedSession 5)
