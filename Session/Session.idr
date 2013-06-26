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


-- TODO: Use the monadic parser instead
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

public
SerialisedSession : Type
SerialisedSession = List (String, String)

data SessionStep = Uninitialised -- before trying to retrieve from the DB
                 | Initialised -- after successfully retrieving session data
                 | Invalid -- if session data is invalid

data SessionData : Type -> Type where
  UninitialisedSession : SessionData a
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

data Session : Effect where
  ---NewSession : NoSession a -> 
  -- Retrieves session information from the database, and marshals it into the given
  -- format.
  RetrieveSessionData : (SerialisedSession -> Maybe a) -> Session (SessionRes Uninitialised a) (SessionRes Initialised a) ()
  -- 
  DeleteSession : Session (SessionRes Initialised a) (SessionRes Invalid a) () -- Possibly have a disposed state? Hm
  UpdateSession : a -> Session (SessionRes Initialised a) (SessionRes Initialised a) ()

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

deserialise : (a : SerialisedSessionType) -> String -> interpSerialisedTy (a)
deserialise ty str = castSerialisedVal ty str

SFunType : (xs : List SerialisedSessionType) -> Type
SFunType (x :: xs) with (length xs)
  | (S k) = (interpSerialisedTy x) -> SFunType xs
  | O = interpSerialisedTy x
-- Given a number of arguments, arg names and arg types, serialises the session

{-
sampleSessionData : SessionRes Initialised MySampleSessionData
sampleSessionData = SRes (ValidInitialisedSession 5 (MySessionData "hi" 1 True))

sampleSessionData' : SessionRes Uninitialised MySampleSessionData
sampleSessionData' = SRes (UninitialisedSession)
-}


