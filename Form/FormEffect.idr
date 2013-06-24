module IdrisWeb.Form.FormEffect
import Effects

%access public -- utter laziness, fixme

data FormTy = FormString
            | FormInt

interpFormTy : FormTy -> Type
interpFormTy FormString = String
interpFormTy FormInt = Int

interpFnTy : Vect FormTy n -> Type
interpFnTy tys = interpFnTy' (reverse tys)
  where interpFnTy' : Vect FormTy n -> Type
        interpFnTy' [] = () -- TODO: should be form effect stuff here
        interpFnTy' (x :: xs) = interpFormTy x -> interpFnTy' xs

FormHandler : List EFFECT -> Type -> Type
FormHandler effs t = Eff IO effs t

interpCheckedFnTy : Vect FormTy n -> List EFFECT -> Type -> Type
interpCheckedFnTy tys effs t = interpCheckedFnTy' (reverse tys)
  where interpCheckedFnTy' : Vect FormTy n -> Type
        interpCheckedFnTy' [] = FormHandler effs t
        interpCheckedFnTy' (x :: xs) = Maybe (interpFormTy x) -> interpCheckedFnTy' xs

showFormVal : (fty : FormTy) -> interpFormTy fty -> String
showFormVal FormString s = s
showFormVal FormInt i = show i
{-
serialiseFnType : Type -> String
serialiseFnType ((Maybe String) -> y) = ?mv1 -- "Maybe String -> " ++ serialiseFnType y
serialiseFnType ((Maybe Int) -> y) = ?mv2 -- "Maybe Int -> " ++ serialiseFnType y
serialiseFnType _ = ?mv3 -- "booo"
-}
-- data Step : 
using (G : Vect FormTy n)
  data Env : Vect FormTy n -> Type where
    Nil : Env Nil
    (::) : interpFormTy a -> Env G -> Env (a :: G) 

  data FormRes : Vect FormTy n -> Type where
    FR : Nat -> Env G -> String -> FormRes G
{- 
Actually not sure we need this
  data HasType : (i : Fin n) -> Vect FormTy n -> FormTy -> Type where
      stop : HasType fO (t :: G) t
      pop  : HasType k G t -> HasType (fS k) (u :: G) t


  lookup : HasType i G t -> Env G -> interpFormTy t
  lookup stop    (x :: xs) = x
  lookup (pop k) (x :: xs) = lookup k xs

  update : HasType i G t -> Env G -> interpFormTy t -> Env G
  update stop (x :: xs) newval = (newval :: xs)
  update (pop k) (x :: xs) newval = x :: (update k xs newval)
  -}
  
  data Form : Effect where
    AddTextBox : (fty : FormTy) -> 
                 (val_ty : interpFormTy fty) -> 
                 Form (FormRes G) (FormRes (fty :: G)) () 
    Submit : interpCheckedFnTy G effs t -> (effs : List EFFECT) -> (t : Type) -> 
             Form (FormRes G) (FormRes []) String

  instance Handler Form m where
    handle (FR len vals ser_str) (Submit fn effs t) k = do
      k (FR O [] ser_str) (ser_str ++ "\n" ++ "serialised closure / submit button stuff here\n</form>")
    handle (FR len vals ser_str) (AddTextBox fty val) k = do
      -- <input type="text" name="inputx" value="val">
      k (FR (S len) (val :: vals) 
        (ser_str ++ "<input name=\"inp1" ++ (show $ len)  ++ "\" value=\"" ++ (showFormVal fty val) ++ "\">\n")) ()

  FORM : Type -> EFFECT
  FORM t = MkEff t Form

  addTextBox : (fty : FormTy) -> (interpFormTy fty) -> EffM m [FORM (FormRes G)] [FORM (FormRes (fty :: G))] ()
  addTextBox ty val = (AddTextBox ty val)

  addSubmit : (interpCheckedFnTy G effs t) -> (effs : List EFFECT) -> (t : Type) -> EffM m [FORM (FormRes G)] [FORM (FormRes [])] String
  addSubmit fn effs t = (Submit fn effs t)

UserForm : Type
UserForm = Eff id [FORM (FormRes [])] String -- Making a form is a pure function (atm)

SerialisedForm : Type
SerialisedForm = String

-- TODO: Action
mkForm : String -> UserForm -> SerialisedForm
mkForm name frm = runPure [FR O [] ("<form name=\"" ++ name ++ "\" action=\"\" method=\"POST\">\n")] frm
