module IdrisWeb.Form.FormEffect
import Effects
import Decidable.Equality
import FormTypes

%access public -- utter laziness, fixme


interpCheckedFnTy : Vect FormTy n -> List EFFECT -> Type -> Type
interpCheckedFnTy tys effs t = interpCheckedFnTy' (reverse tys)
  where interpCheckedFnTy' : Vect FormTy n -> Type
        interpCheckedFnTy' [] = FormHandler effs t
        interpCheckedFnTy' (x :: xs) = Maybe (interpFormTy x) -> interpCheckedFnTy' xs


serialiseSubmit : String -> Vect FormTy n -> List WebEffect -> FormTy -> String
serialiseSubmit name tys effs ret = "<input type=\"hidden\" name=\"handler\" value=\"" ++ name ++ ":" ++
                                     serialised_tys ++ ":" ++ serialised_effs ++ ":" ++ serialised_ret ++ ";\"></input>" ++
                                    "<input type=\"submit\"></input></form>"  
  where serialised_tys = foldr (\ty, str => str ++ (show ty) ++ ";") "" tys
        serialised_effs = foldr (\eff, str => str ++ (show eff) ++ ";") "" effs
        serialised_ret =  (show ret) 

using (G : Vect FormTy n)
  private
  data Env : Vect FormTy n -> Type where
    Nil : Env Nil
    (::) : interpFormTy a -> Env G -> Env (a :: G) 

  data FormRes : Vect FormTy n -> Type where
    FR : Nat -> Env G -> Vect FormTy n -> String -> FormRes G

  
  data Form : Effect where
    AddTextBox : (fty : FormTy) -> 
                 (val_ty : interpFormTy fty) -> 
                 Form (FormRes G) (FormRes (fty :: G)) () 
    Submit : interpCheckedFnTy G effs t -> String -> (effs : List WebEffect) -> (t : FormTy) -> 
             Form (FormRes G) (FormRes []) String

  instance Handler Form m where
    handle (FR len vals tys ser_str) (Submit fn name effs t) k = do
      k (FR O [] [] ser_str) (ser_str ++ "\n" ++ (serialiseSubmit name tys effs t))-- "serialised closure / submit button stuff here\n</form>")
    handle (FR len vals tys ser_str) (AddTextBox fty val) k = do
      k (FR (S len) (val :: vals) (fty :: tys) 
        (ser_str ++ "<input name=\"inp" ++ (show $ len)  ++ "\" value=\"" ++ (showFormVal fty val) ++ "\"></input>\n")) ()
      -- <input type="text" name="inpx" value="val">

  FORM : Type -> EFFECT
  FORM t = MkEff t Form

  addTextBox : (fty : FormTy) -> (interpFormTy fty) -> EffM m [FORM (FormRes G)] [FORM (FormRes (fty :: G))] ()
  addTextBox ty val = (AddTextBox ty val)

  addSubmit : (interpCheckedFnTy G effs t) -> String -> (effs : List WebEffect) -> (t : FormTy) -> EffM m [FORM (FormRes G)] [FORM (FormRes [])] String
  addSubmit fn name effs t = (Submit fn name effs t)


public
UserForm : Type
UserForm = Eff id [FORM (FormRes [])] String -- Making a form is a pure function (atm)


SerialisedForm : Type
SerialisedForm = String

-- TODO: Action
mkForm : String -> String -> UserForm -> SerialisedForm
mkForm name action frm = runPure [FR O [] [] ("<form name=\"" ++ name ++ 
                         "\" action=\"" ++ action ++ "\" method=\"POST\">\n")] frm

