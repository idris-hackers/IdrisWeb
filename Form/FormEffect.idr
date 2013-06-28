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

formstringNotFormInt : FormString = FormInt -> _|_
formstringNotFormInt refl impossible
formstringNotFormBool : FormString = FormBool -> _|_
formstringNotFormBool refl impossible
formstringNotFormFloat : FormString = FormFloat -> _|_
formstringNotFormFloat refl impossible
formintNotFormString : FormInt = FormString -> _|_
formintNotFormString refl impossible
formintNotFormBool : FormInt = FormBool -> _|_
formintNotFormBool refl impossible
formintNotFormFloat : FormInt = FormFloat -> _|_
formintNotFormFloat refl impossible
formboolNotFormString : FormBool = FormString -> _|_
formboolNotFormString refl impossible
formboolNotFormInt : FormBool = FormInt -> _|_
formboolNotFormInt refl impossible
formboolNotFormFloat : FormBool = FormFloat -> _|_
formboolNotFormFloat refl impossible
formfloatNotFormString : FormFloat = FormString -> _|_
formfloatNotFormString refl impossible
formfloatNotFormInt : FormFloat = FormInt -> _|_
formfloatNotFormInt refl impossible
formfloatNotFormBool : FormFloat = FormBool -> _|_
formfloatNotFormBool refl impossible

instance DecEq FormTy where
  decEq FormString FormString = Yes refl
  decEq FormString FormInt = No formstringNotFormInt
  decEq FormString FormBool = No formstringNotFormBool
  decEq FormString FormFloat = No formstringNotFormFloat
  decEq FormInt FormString = No formintNotFormString
  decEq FormInt FormInt = Yes refl
  decEq FormInt FormBool = No formintNotFormBool
  decEq FormInt FormFloat = No formintNotFormFloat
  decEq FormBool FormString = No formboolNotFormString
  decEq FormBool FormInt = No formboolNotFormInt
  decEq FormBool FormBool = Yes refl
  decEq FormBool FormFloat = No formboolNotFormFloat
  decEq FormFloat FormString = No formfloatNotFormString
  decEq FormFloat FormInt = No formfloatNotFormInt
  decEq FormFloat FormBool = No formfloatNotFormBool
  decEq FormFloat FormFloat = Yes refl

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
