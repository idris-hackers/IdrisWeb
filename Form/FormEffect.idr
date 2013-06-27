module IdrisWeb.Form.FormEffect
import Effects
import Decidable.Equality
--import FormHandler

%access public -- utter laziness, fixme

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


fsNotFi : FormString = FormInt -> _|_
fsNotFi refl impossible

instance DecEq FormTy where
  decEq FormString FormString = Yes refl
  decEq FormString FormInt = No fsNotFi
  decEq FormString FormBool = ?mv2 --No refl impossible
  decEq FormString FormFloat = ?mv3 --No refl impossible
  decEq FormInt FormString = ?mv4 --Yes refl
  decEq FormInt FormInt = ?mv5 --Yes refl
  decEq FormInt FormBool = ?mv6 --No refl impossible
  decEq FormInt FormFloat = ?mv7 -- No refl impossible  
  decEq FormBool FormString = ?mv8 -- No refl impossible
  decEq FormBool FormInt = ?mv9 -- No refl impossible
  decEq FormBool FormBool = ?mv10 -- Yes refl
  decEq FormBool FormFloat = ?mv11 --No refl impossible
  decEq FormFloat FormString = ?mv12 -- No impossible
  decEq FormFloat FormInt = ?mv13 -- No refl impossible
  decEq FormFloat FormBool = ?mv14 --No refl impossible
  decEq FormFloat FormFloat = ?mv15 --Yes refl 
{-
  decEq FormInt FormInt = Yes refl
  decEq FormBool FormBool = Yes refl
  decEq FormFloat FormFloat = Yes refl
  -}

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

using (G : Vect FormTy n)
  private
  data Env : Vect FormTy n -> Type where
    Nil : Env Nil
    (::) : interpFormTy a -> Env G -> Env (a :: G) 

  data FormRes : Vect FormTy n -> Type where
    FR : Nat -> Env G -> String -> FormRes G

  
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
        (ser_str ++ "<input name=\"inp" ++ (show $ len)  ++ "\" value=\"" ++ (showFormVal fty val) ++ "\">\n")) ()

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



{- 
Actually not sure we need this

  -}
