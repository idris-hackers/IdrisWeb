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

{-
total
toSerialisedSession : List SerialisedSessionType -> Type
toSerialisedSession [] = SerialisedSession -- Result type
toSerialisedSession (x :: xs) = interpSerialisedTy x -> toSerialisedSession xs

total
serialise : (tys : List SerialisedSessionType) -> toSerialisedSession tys
serialise 
-}

--total
interpSerialisedTys : (tys : Vect SerialisedSessionType n) -> Type
interpSerialisedTys [] = ()
interpSerialisedTys [x] = interpSerialisedTy x
interpSerialisedTys (x :: (y :: xs)) = (interpSerialisedTy x, interpSerialisedTys (y :: xs))



serialise : (tys' : Vect SerialisedSessionType n) -> (Vect String n) -> interpSerialisedTys tys' -> SerialisedSession
serialise [] () = [] -- ?pines1
serialise [ty] val = [("name", showSerialisedVal ty val)]
serialise (ty :: (ytys :: tys)) (x, y) = ("name", showSerialisedVal ty x) :: serialise (ytys :: tys) y
  -- If (isCons tys) is true, then the args thing will be of form (x, y)
--  | True = let (arg, rest) = args in ("pines", showSerialisedVal ty arg) :: (serialise tys rest)
  -- otherwise, it will be of form x
--  | False = let args = arg in ("pines", showSerialisedVal ty arg)
 

-- web DSL
-- lambda pulls out of form [(String, String)] 

using (G : Vect Ty n)

  data Env : Vect Ty n -> Type where
      Nil  : Env Nil
      (::) : interpTy a -> Env G -> Env (a :: G)

  data HasType : (i : Fin n) -> Vect Ty n -> Ty -> Type where
      stop : HasType fO (t :: G) t
      pop  : HasType k G t -> HasType (fS k) (u :: G) t

  lookup : HasType i G t -> Env G -> interpTy t
  lookup stop    (x :: xs) = x
  lookup (pop k) (x :: xs) = lookup k xs





-- 
{-serialise (ty :: tys) arg with (isCons tys)
 | False ?= Prelude.List.(::) ("pines", showSerialisedVal ty arg) Prelude.List.Nil-- ?pines
 | True ?= Prelude.List.(::) ("pines", showSerialisedVal ty (fst arg)) (serialise tys (snd arg))
 -}
{-
interpSerialisedTys : (tys : List SerialisedSessionType) -> List Type
interpSerialisedTys [] = Prelude.List.Nil
interpSerialisedTys (x :: xs) = interpSerialisedTy x :: interpSerialisedTys xs
-}


{-
using (xs, ys : List SerialisedSessionType)
  data SerialisedArgList : List SerialisedSessionType -> Type where
    sNil : SerialisedArgList Nil
    sCons : interpSerialisedTy x -> SerialisedArgList xs -> SerialisedArgList (x :: xs)


serialise : (tys : List SerialisedSessionType) -> SerialisedArgList tys -> SerialisedSession
serialise [] _ = []
serialise (ty :: tys) (sCons arg args) = (showSerialisedVal ty arg) :: (serialise tys args)
-}

--serialise : (tys : List SerialisedSessionType) -> interpSerialisedTys tys -> SerialisedSession
--serialise [] [] = [("pines", "pines")]
--serialise [] _ = []
--serialise (ty :: tys) args with (isCons tys) 
--  | True = ?truecase
--  | False = ("pines", showSerialisedVal ty args) :: Prelude.List.Nil
-- = ?mv -- (showSerialisedVal ty dat) :: serialise tys rest
--serialise : (tys : List SerialisedSessionType) -> interpSerialisedTys (tys) ->

deserialise : (a : SerialisedSessionType) -> String -> interpSerialisedTy (a)
deserialise ty str = castSerialisedVal ty str
{-
serialiseSampleData : MySampleSessionData -> SerialisedSession 
serialiseSampleData mssd = [serialise SString "username" (username mssd), 
                            serialise SInt "age" (age mssd),
                            serialise SBool "male" (male mssd)]
                            -}
SFunType : (xs : List SerialisedSessionType) -> Type
SFunType (x :: xs) with (length xs)
  | (S k) = (interpSerialisedTy x) -> SFunType xs
  | O = interpSerialisedTy x
-- Given a number of arguments, arg names and arg types, serialises the session
{-
total
serialiseData : Fin n -> Vect String n -> Vect SerialisedSessionType n -> SerialisedSession
serialiseData fO _ _ = []
serialiseData fS (k) (argname :: argnames) (ty :: tys) = 

deserialiseSampleData : SerialisedSession -> Maybe MySampleSessionData
deserialiseSampleData ss = do ss_un <-  lookup "username" ss
                              ss_age <- lookup "age" ss
                              ss_male <- lookup "male" ss
                              pure $ MySessionData (deserialise SString ss_un) 
                                                   (deserialise SInt ss_age) 
                                                   (deserialise SBool ss_male)
                                                   -}                            
sampleSessionData : SessionRes Initialised MySampleSessionData
sampleSessionData = SRes (ValidInitialisedSession 5 (MySessionData "hi" 1 True))

sampleSessionData' : SessionRes Uninitialised MySampleSessionData
sampleSessionData' = SRes (UninitialisedSession)



{-
-- Let's try and get my head around this whole well-typed interpreter thing then...
using (G : Vect SerialisedSessionType n)
  -- Environment: a vector of the types of our argument
  data Env : Vect SerialisedSessionType n -> Type where
    -- No types
    Nil : Env Nil
    -- Given a serialised type tag a, an environment G,
    -- appends the type to the environment
    (::) : interpSerialisedTy a -> Env G -> Env (a :: G)

  -- HasType is a predicate, giving a proof that a variable has a particular type.
  data HasType : (i : Fin n) -> Vect SerialisedSessionType n -> 
                              SerialisedSessionType -> Type where
    -- Index 0 : we've found our type
    stop : HasType fO (t :: G) t
    -- Other indices: it's later in the list somewhere
    pop : HasType k G t -> HasType (fS k) (u :: G) t

  -- Given a proof of list membership and the environment, gets the associated
  -- type.
  lookup : HasType i G t -> Env G -> interpSerialisedTy t
  lookup stop (x :: xs) = x
  lookup (pop k) (x :: xs) = lookup k xs
  -}
