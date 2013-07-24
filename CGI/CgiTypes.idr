module IdrisWeb.CGI.CgiTypes
import Effects
import Decidable.Equality
import SQLite
import Session
%access public
-- Types used by the CGI module

-- Simple type synonym for a list of key value pairs
public
Vars : Type
Vars = List (String, String)

FormHandler : List EFFECT -> Type
FormHandler effs = Eff IO effs ()

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
               | SessionEffect

cgiNotSqlite : CgiEffect = SqliteEffect -> _|_
cgiNotSqlite refl impossible

cgiNotSession : CgiEffect = SessionEffect -> _|_
cgiNotSession refl impossible

sqliteNotSession : SqliteEffect = SessionEffect -> _|_
sqliteNotSession refl impossible

instance DecEq WebEffect where
  decEq CgiEffect CgiEffect = Yes refl
  decEq SqliteEffect SqliteEffect = Yes refl
  decEq CgiEffect SqliteEffect = No cgiNotSqlite
  decEq SqliteEffect CgiEffect = No (negEqSym cgiNotSqlite)
  decEq CgiEffect SessionEffect = No cgiNotSession
  decEq SessionEffect CgiEffect = No (negEqSym cgiNotSession)
  decEq SessionEffect SessionEffect = Yes refl
  decEq SqliteEffect SessionEffect = No sqliteNotSession
  decEq SessionEffect SqliteEffect = No (negEqSym sqliteNotSession)

instance Eq WebEffect where
  (==) CgiEffect CgiEffect = True
  (==) SqliteEffect SqliteEffect = True
  (==) SessionEffect SessionEffect = True
  (==) _ _ = False

instance Show WebEffect where
  show CgiEffect = "cgi"
  show SqliteEffect = "sqlite"
  show SessionEffect = "session"

total
interpWebEffect : WebEffect -> EFFECT
interpWebEffect CgiEffect = (CGI (InitialisedCGI TaskRunning))
interpWebEffect SqliteEffect = (SQLITE ())
interpWebEffect SessionEffect = (SESSION (SessionRes SessionUninitialised))

interpWebEffects : List WebEffect -> List EFFECT
interpWebEffects [] = []
interpWebEffects (x :: xs) = interpWebEffect x :: interpWebEffects xs



{- Allowed form types -}

data FormTy = FormString
            | FormInt
            | FormBool
            | FormFloat
            | FormList FormTy 

total
interpFormTy : FormTy -> Type
interpFormTy FormString = String
interpFormTy FormInt = Int
interpFormTy FormBool = Bool
interpFormTy FormFloat = Float
interpFormTy (FormList a) = List (interpFormTy a)

instance Eq FormTy where
  (==) FormString FormString = True
  (==) FormInt FormInt = True
  (==) FormBool FormBool = True
  (==) FormFloat FormFloat = True
  (==) (FormList a) (FormList b) = (a == b) 
  (==) _ _ = False

instance Show FormTy where
  show FormString = "str"
  show FormInt = "int"
  show FormBool = "bool"
  show FormFloat = "float"
  show (FormList a) = "list_" ++ (show a)


formstringNotFormInt : FormString = FormInt -> _|_
formstringNotFormInt refl impossible
formstringNotFormBool : FormString = FormBool -> _|_
formstringNotFormBool refl impossible
formstringNotFormFloat : FormString = FormFloat -> _|_
formstringNotFormFloat refl impossible
formstringNotFormList : FormString = (FormList a) -> _|_
formstringNotFormList refl impossible
formintNotFormBool : FormInt = FormBool -> _|_
formintNotFormBool refl impossible
formintNotFormFloat : FormInt = FormFloat -> _|_
formintNotFormFloat refl impossible
formintNotFormList : FormInt = (FormList a) -> _|_
formintNotFormList refl impossible
formboolNotFormFloat : FormBool = FormFloat -> _|_
formboolNotFormFloat refl impossible
formboolNotFormList : FormBool = (FormList a) -> _|_
formboolNotFormList refl impossible
formfloatNotFormList : FormFloat = (FormList a) -> _|_
formfloatNotFormList refl impossible

lemma_a_not_b : {a : FormTy} -> {b : FormTy} -> ((a = b) -> _|_) -> ((FormList a = FormList b) -> _|_) 
lemma_a_not_b refl impossible

instance DecEq FormTy where
  decEq FormString FormString = Yes refl
  decEq FormString FormInt = No formstringNotFormInt
  decEq FormString FormBool = No formstringNotFormBool
  decEq FormString FormFloat = No formstringNotFormFloat
  decEq FormString (FormList a) = No formstringNotFormList
  decEq FormInt FormString = No (negEqSym formstringNotFormInt)
  decEq FormInt FormInt = Yes refl
  decEq FormInt FormBool = No formintNotFormBool
  decEq FormInt FormFloat = No formintNotFormFloat
  decEq FormInt (FormList a) = No formintNotFormList
  decEq FormBool FormString = No (negEqSym formstringNotFormBool)
  decEq FormBool FormInt = No (negEqSym formintNotFormBool)
  decEq FormBool FormBool = Yes refl
  decEq FormBool FormFloat = No formboolNotFormFloat
  decEq FormBool (FormList a) = No formboolNotFormList
  decEq FormFloat FormString = No (negEqSym formstringNotFormFloat)
  decEq FormFloat FormInt = No (negEqSym formintNotFormFloat)
  decEq FormFloat FormBool = No (negEqSym formboolNotFormFloat)
  decEq FormFloat FormFloat = Yes refl
  decEq FormFloat (FormList a) = No formfloatNotFormList
  decEq (FormList a) FormString = No (negEqSym formstringNotFormList)
  decEq (FormList a) FormInt = No (negEqSym formintNotFormList)
  decEq (FormList a) FormBool = No (negEqSym formboolNotFormList)
  decEq (FormList a) FormFloat = No (negEqSym formfloatNotFormList)
  decEq (FormList a) (FormList b) with (decEq a b) 
    decEq (FormList a) (FormList a) | Yes refl = Yes refl
    decEq (FormList a) (FormList b) | No p = No (lemma_a_not_b p)

MkHandlerFnTy : Type
MkHandlerFnTy = (List FormTy, List WebEffect)

mkHandlerFn' : List FormTy -> List WebEffect -> Type
mkHandlerFn' [] effs = FormHandler (interpWebEffects effs) 
mkHandlerFn' (x :: xs) effs = Maybe (interpFormTy x) -> mkHandlerFn' xs effs 

mkHandlerFn : MkHandlerFnTy -> Type 
mkHandlerFn (tys, effs) = mkHandlerFn' tys effs 

data RegHandler : Type where
  RH : (ft : MkHandlerFnTy) -> mkHandlerFn ft -> RegHandler

interpCheckedFnTy : Vect FormTy n -> List WebEffect -> Type
interpCheckedFnTy tys effs = interpCheckedFnTy' (reverse tys)
  where interpCheckedFnTy' : Vect FormTy n -> Type
        interpCheckedFnTy' [] = FormHandler (interpWebEffects effs)
        interpCheckedFnTy' (x :: xs) = Maybe (interpFormTy x) -> interpCheckedFnTy' xs

public
HandlerFn : Type
HandlerFn = (ft ** (mkHandlerFn ft, String))

public
HandlerList : Type
HandlerList = List HandlerFn


using (G : List FormTy, E : List WebEffect)
  data FormRes : List FormTy -> List WebEffect -> Type where
    FR : Nat -> List FormTy -> List WebEffect -> String -> FormRes G E

  data FnElem : mkHandlerFn ((reverse G), E) -> HandlerList -> Type where 
       FnHere : {xs : HandlerList, f : mkHandlerFn ((reverse G), E)} ->
              FnElem f ((((reverse G), E) ** (f, fStr)) :: xs)
       FnThere : {xs : HandlerList, f : mkHandlerFn ((reverse G), E)} ->
               FnElem f xs -> FnElem f (x :: xs)

  findFn : Nat -> List (TTName, Binder TT) -> TT -> Tactic -- Nat is maximum search depth
  findFn O ctxt goal = Refine "FnHere" `Seq` Solve 
  findFn (S n) ctxt goal = GoalType "FnElem" 
            (Try (Refine "FnHere" `Seq` Solve)
                 (Refine "FnThere" `Seq` (Solve `Seq` findFn n ctxt goal)))


  getString' : (f : mkHandlerFn ((reverse G), E)) ->
               (fs : HandlerList) -> FnElem f fs -> String
  getString' f ((_ ** (_, n)) :: _) FnHere = n
  getString' f (_ :: fs) (FnThere p) = getString' f fs p

  
  data Form : Effect where
    AddTextBox : (label : String) -> 
                 (fty : FormTy) -> 
                 (Maybe (interpFormTy fty)) -> 
                 Form (FormRes G E) (FormRes (fty :: G) E) () 

    AddHidden  : (fty : FormTy) -> 
                 (interpFormTy fty) -> 
                 Form (FormRes G E) (FormRes (fty :: G) E) () 

    AddSelectionBox : (label : String) ->
                      (fty : FormTy) ->
                      (vals : Vect (interpFormTy fty) m) ->
                      (names : Vect String m) ->
                      Form (FormRes G E) (FormRes (fty :: G) E) ()

    AddRadioGroup : (label : String) -> 
                    (fty : FormTy) ->
                    (vals : Vect (interpFormTy fty) m) ->
                    (names : Vect String m) ->
                    (default : Int) ->
                    Form (FormRes G E) (FormRes (fty :: G) E) ()
    
    AddCheckBoxes : (label : String) ->
                    (fty : FormTy) ->
                    (vals : Vect (interpFormTy fty) m) ->
                    (names : Vect String m) ->
                    (checked_boxes : Vect Bool m) ->
                    Form (FormRes G E) (FormRes ((FormList fty) :: G) E) ()

    UseEffects : (effs : List WebEffect) ->
                 Form (FormRes G E) (FormRes G effs) ()

    Submit : -- (interpCheckedFnTy G E) -> 
             (mkHandlerFn ((reverse G), E)) ->
             String -> 
             Form (FormRes G E) (FormRes [] []) String

  FORM : Type -> EFFECT
  FORM t = MkEff t Form

  addTextBox : String ->
               (fty : FormTy) -> -- Data type
               (Maybe (interpFormTy fty)) ->  -- Default data value (optional)
               EffM m [FORM (FormRes G E)] [FORM (FormRes (fty :: G) E)] ()
  addTextBox label ty val = (AddTextBox label ty val)

  addHidden : (fty : FormTy) ->
              (interpFormTy fty) -> -- Default value
              EffM m [FORM (FormRes G E)] [FORM (FormRes (fty :: G) E)] ()
  addHidden ty val = (AddHidden ty val)

  addSelectionBox : String ->
                    (fty : FormTy) ->
                    (vals : Vect (interpFormTy fty) j) ->
                    (names : Vect String j) ->
                    EffM m [FORM (FormRes G E)] [FORM (FormRes (fty :: G) E)] ()
  addSelectionBox label ty vals names = (AddSelectionBox label ty vals names)

  addRadioGroup : String ->
                  (fty : FormTy) ->
                  (vals : Vect (interpFormTy fty) j) ->
                  (names : Vect String j) ->
                  (default : Int) ->
                  EffM m [FORM (FormRes G E)] [FORM (FormRes (fty :: G) E)] ()
  addRadioGroup label ty vals names default = (AddRadioGroup label ty vals names default)

  addCheckBoxes : (label : String) ->
                  (fty : FormTy) ->
                  (vals : Vect (interpFormTy fty) j) ->
                  (names : Vect String j) ->
                  (checked_boxes : Vect Bool j) ->
                  EffM m [FORM (FormRes G E)] [FORM (FormRes ((FormList fty) :: G) E)] ()
  addCheckBoxes label ty vals names checked = (AddCheckBoxes label ty vals names checked) 


{-
  getString : (f : mkHandlerFn ((reverse G), E)) ->
              (fs : HandlerList) -> 
              {default tactics { applyTactic findFn 100; solve; }
                prf : FnElem f fs} -> String
  getString f fs {prf} = getString' f fs prf
-}


-- (ft ** (mkHandlerFn ft, String))
  addSubmit : --(fn : (interpCheckedFnTy G E)) ->
              (f :  mkHandlerFn ((reverse G), E)) ->
              (fns : HandlerList) ->
              {default tactics { applyTactic findFn 100; solve; }
                prf : FnElem f fns} ->
            --  (prf : FnElem fn fns) ->
              EffM m [FORM (FormRes G E)] [FORM (FormRes [] [])] String
  addSubmit f handlers {prf} = (Submit f name)
    where name : String
          name = getString' f handlers prf

  useEffects : (effs : List WebEffect) ->
               EffM m [FORM (FormRes G E)] [FORM (FormRes G effs)] ()
  useEffects effs = (UseEffects effs)

UserForm = Eff id [FORM (FormRes [] [])] String -- Making a form is a pure function (atm)



-- CGI Effect
public
data Cgi : Effect where
  -- Individual functions of the effect
  
  -- Action retrieval
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

  -- Lookup a cookie from the cookie variables
  QueryCookieVar : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (Maybe String)

  -- Retrieves the current output
  GetOutput : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) String

  -- Retrieves the headers
  GetHeaders : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) String

  -- Flushes the headers to StdOut
  FlushHeaders : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Flushes output to StdOut
  Flush : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Initialise the internal CGI State
  Init : Cgi () (InitialisedCGI Initialised) ()

  -- Transition to task started state
  StartRun : Cgi (InitialisedCGI Initialised) (InitialisedCGI TaskRunning) ()

  -- Transition to task completed state
  FinishRun : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskCompleted) ()

  -- Write headers, transition to headers written state
  WriteHeaders : Cgi (InitialisedCGI TaskCompleted) (InitialisedCGI HeadersWritten) ()

  -- Write content, transition to content written state
  WriteContent : Cgi (InitialisedCGI HeadersWritten) (InitialisedCGI ContentWritten) ()

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
  HandleForm : HandlerList -> 
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
  PF : (w_effs : List WebEffect) ->  
       (effs : List EFFECT) -> (env : Effects.Env IO effs) -> 
       Eff IO effs () -> PopFn


{-
test1 : Maybe String -> Maybe Int -> FormHandler [CGI (InitialisedCGI TaskRunning)]
test1 (Just name) (Just age) = do
  --output $ "Your name is " ++ name
  --output $ " and you are " ++ (show age) ++ " years old!"
  pure ()


test2 : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning), SQLITE ()]
test2 (Just name) (Just name2) = do
  --output $ "Your name is " ++ name
  --output $ " and your last name is " ++ name2
  pure ()
-}
{-
fnList : HandlerList
fnList = [(([FormString, FormInt], [CgiEffect]) ** (test1, "test1")),
          (([FormString, FormString], [CgiEffect, SqliteEffect]) ** (test2, "test2"))]
-}

-----
----- END NEW STUFF
-----

