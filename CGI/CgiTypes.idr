module IdrisWeb.CGI.CgiTypes
import Effects
import Decidable.Equality
import SQLite
%access public
-- Types used by the CGI module

-- Simple type synonym for a list of key value pairs
public
Vars : Type
Vars = List (String, String)

FormHandler : List EFFECT -> Type -> Type
FormHandler effs t = EffM IO effs effs t

-- CGI Concrete effect sig
public
CGI : Type -> EFFECT

public
UserForm : Type

-- Information passed by CGI
public
record CGIInfo : Type where
       CGIInf : (GET : Vars) ->
                (POST : Vars) ->
                (Cookies : Vars) ->
                (UserAgent : String) ->
                (Headers : String) ->
                (Output : String) -> CGIInfo

-- Type of user-defined CGI Actions
public
CGIProg : List EFFECT -> Type -> Type

-- States in the state machine
public
data CGIStep  = Initialised 
              | TaskRunning 
              | TaskCompleted 
              | HeadersWritten 
              | ContentWritten 
              -- Perhaps another after any cleanup?



-- Data type representing an initialised CGI script
public
data InitialisedCGI : CGIStep -> Type where
  ICgi : CGIInfo -> InitialisedCGI s

{- Serialisable Web Effects -}

data WebEffect = CgiEffect
               | SqliteEffect

cgiNotSqlite : CgiEffect = SqliteEffect -> _|_
cgiNotSqlite refl impossible

instance DecEq WebEffect where
  decEq CgiEffect CgiEffect = Yes refl
  decEq SqliteEffect SqliteEffect = Yes refl
  decEq CgiEffect SqliteEffect = No cgiNotSqlite
  decEq SqliteEffect CgiEffect = No (negEqSym cgiNotSqlite)


instance Eq WebEffect where
  (==) CgiEffect CgiEffect = True
  (==) SqliteEffect SqliteEffect = True
  (==) _ _ = False

instance Show WebEffect where
  show CgiEffect = "cgi"
  show SqliteEffect = "sqlite"

total
interpWebEffect : WebEffect -> EFFECT
interpWebEffect CgiEffect = (CGI (InitialisedCGI TaskRunning))
interpWebEffect SqliteEffect = (SQLITE ())

interpWebEffects : List WebEffect -> List EFFECT
interpWebEffects [] = []
interpWebEffects (x :: xs) = interpWebEffect x :: interpWebEffects xs



{- Allowed form types -}

data FormTy = FormString
            | FormInt
            | FormBool
            | FormFloat

total
interpFormTy : FormTy -> Type
interpFormTy FormString = String
interpFormTy FormInt = Int
interpFormTy FormBool = Bool
interpFormTy FormFloat = Float

instance Eq FormTy where
  (==) FormString FormString = True
  (==) FormInt FormInt = True
  (==) FormBool FormBool = True
  (==) FormFloat FormFloat = True
  (==) _ _ = False

instance Show FormTy where
  show FormString = "str"
  show FormInt = "int"
  show FormBool = "bool"
  show FormFloat = "float"

formstringNotFormInt : FormString = FormInt -> _|_
formstringNotFormInt refl impossible
formstringNotFormBool : FormString = FormBool -> _|_
formstringNotFormBool refl impossible
formstringNotFormFloat : FormString = FormFloat -> _|_
formstringNotFormFloat refl impossible
formintNotFormBool : FormInt = FormBool -> _|_
formintNotFormBool refl impossible
formintNotFormFloat : FormInt = FormFloat -> _|_
formintNotFormFloat refl impossible
formboolNotFormFloat : FormBool = FormFloat -> _|_
formboolNotFormFloat refl impossible

instance DecEq FormTy where
  decEq FormString FormString = Yes refl
  decEq FormString FormInt = No formstringNotFormInt
  decEq FormString FormBool = No formstringNotFormBool
  decEq FormString FormFloat = No formstringNotFormFloat
  decEq FormInt FormString = No (negEqSym formstringNotFormInt)
  decEq FormInt FormInt = Yes refl
  decEq FormInt FormBool = No formintNotFormBool
  decEq FormInt FormFloat = No formintNotFormFloat
  decEq FormBool FormString = No (negEqSym formstringNotFormBool)
  decEq FormBool FormInt = No (negEqSym formintNotFormBool)
  decEq FormBool FormBool = Yes refl
  decEq FormBool FormFloat = No formboolNotFormFloat
  decEq FormFloat FormString = No (negEqSym formstringNotFormFloat)
  decEq FormFloat FormInt = No (negEqSym formintNotFormFloat)
  decEq FormFloat FormBool = No (negEqSym formboolNotFormFloat)
  decEq FormFloat FormFloat = Yes refl


MkHandlerFnTy : Type
MkHandlerFnTy = (List FormTy, List WebEffect, FormTy)

mkHandlerFn' : List FormTy -> List WebEffect -> FormTy -> Type
mkHandlerFn' [] effs ty = FormHandler (interpWebEffects effs) (interpFormTy ty)
mkHandlerFn' (x :: xs) effs ty = Maybe (interpFormTy x) -> mkHandlerFn' xs effs ty

mkHandlerFn : MkHandlerFnTy -> Type 
mkHandlerFn (tys, effs, ret) = mkHandlerFn' tys effs ret


data RegHandler : Type where
  RH : (ft : MkHandlerFnTy) -> mkHandlerFn ft -> RegHandler


interpCheckedFnTy : Vect FormTy n -> List EFFECT -> Type -> Type
interpCheckedFnTy tys effs t = interpCheckedFnTy' (reverse tys)
  where interpCheckedFnTy' : Vect FormTy n -> Type
        interpCheckedFnTy' [] = FormHandler effs t
        interpCheckedFnTy' (x :: xs) = Maybe (interpFormTy x) -> interpCheckedFnTy' xs


using (G : Vect FormTy n)
  data Env : Vect FormTy n -> Type where
    Nil : Env Nil
    (::) : interpFormTy a -> Env G -> Env (a :: G) 

  data FormRes : Vect FormTy n -> Type where
    FR : Nat -> Env G -> Vect FormTy n -> String -> FormRes G

  
  data Form : Effect where
    AddTextBox : (fty : FormTy) -> 
                 (val_ty : interpFormTy fty) -> 
                 Form (FormRes G) (FormRes (fty :: G)) () 

    Submit : interpCheckedFnTy G effs t -> 
             String -> 
             (effs : List WebEffect) -> 
             (t : FormTy) -> 
             Form (FormRes G) (FormRes []) String

  FORM : Type -> EFFECT
  FORM t = MkEff t Form

  addTextBox : (fty : FormTy) -> 
               (interpFormTy fty) -> 
               EffM m [FORM (FormRes G)] [FORM (FormRes (fty :: G))] ()
  addTextBox ty val = (AddTextBox ty val)

  addSubmit : (interpCheckedFnTy G effs t) -> 
              String -> 
              (effs : List WebEffect) -> 
              (t : FormTy) -> 
              EffM m [FORM (FormRes G)] [FORM (FormRes [])] String
  addSubmit fn name effs t = (Submit fn name effs t)


UserForm = Eff id [FORM (FormRes [])] String -- Making a form is a pure function (atm)



-- CGI Effect
public
data Cgi : Effect where
  -- Individual functions of the effect
  
  -- Action retrieval
  -- ASKEDWIN: HALP! How to bind implicit variable?
  -- GetAction : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (CGIProg a)

  -- State retrival / update
  -- ASKEDWIN: Do we really need this?
  --SetInfo : CGIInfo -> Cgi (InitialisedCGI Initialised) (InitialisedCGI Ini) ()

  GetInfo : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) CGIInfo
  
  -- Output a string
  OutputData : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Retrieve the GET variables
  GETVars : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Vars

  -- Retrieve the POST variables
  POSTVars : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Vars

  -- Retrieve the cookie variables
  CookieVars : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Vars

  -- Lookup a variable in the GET variables
  QueryGetVar : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (Maybe String)

  -- Lookup a variable in the POST variables
  QueryPostVar : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (Maybe String)

  -- Retrieves the current output
  GetOutput : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) String

  -- Retrieves the headers
  GetHeaders : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) String

  -- Flushes the headers to StdOut
  FlushHeaders : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Flushes output to StdOut
  Flush : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Initialise the internal CGI State
  Init : Cgi () (InitialisedCGI Initialised) String

  -- Transition to task started state
  StartRun : Cgi (InitialisedCGI Initialised) (InitialisedCGI TaskRunning) ()

  -- Transition to task completed state
  FinishRun : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskCompleted) ()

  -- Write headers, transition to headers written state
  WriteHeaders : Cgi (InitialisedCGI TaskCompleted) (InitialisedCGI HeadersWritten) ()

  -- Write content, transition to content written state
  WriteContent : String -> Cgi (InitialisedCGI HeadersWritten) (InitialisedCGI ContentWritten) ()

  -- Add cookie
  -- TODO: Add expiry date in here once I've finished the basics
  SetCookie : String -> String -> {- Date -> -} 
              Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Run the user-specified action
  RunAction : Env IO (CGI (InitialisedCGI TaskRunning) :: effs) -> 
              CGIProg effs a -> 
              Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) a

  -- Write a form to the web page
  AddForm : String -> String -> 
            UserForm -> 
            Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Attempts to handle a form, given a list of available handlers
  HandleForm : List (String, RegHandler) -> 
               Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Bool


CGI t = MkEff t Cgi


CGIProg effs a = Eff IO (CGI (InitialisedCGI TaskRunning) :: effs) a









interpFnTy : Vect FormTy n -> Type
interpFnTy tys = interpFnTy' (reverse tys)
  where interpFnTy' : Vect FormTy n -> Type
        interpFnTy' [] = () -- TODO: should be form effect stuff here
        interpFnTy' (x :: xs) = interpFormTy x -> interpFnTy' xs



SerialisedForm : Type
SerialisedForm = String



data PopFn : Type where
  PF : (w_effs : List WebEffect) -> (ret_ty : FormTy) -> 
       (effs : List EFFECT) -> (env : Effects.Env IO effs) -> 
       Eff IO effs (interpFormTy ret_ty) -> PopFn


