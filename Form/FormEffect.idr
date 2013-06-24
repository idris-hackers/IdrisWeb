module IdrisWeb.Form.FormEffect
import Effects

data FormTy = FormString
            | FormInt

interpFormTy : FormTy -> Type
interpFormTy FormString = String
interpFormTy FormInt = Int

interpFnTy : List FormTy -> Type
interpFnTy [] = () -- TODO: should be form effect stuff here
interpFnTy (x :: xs) = interpFormTy x -> interpFnTy xs

interpCheckedFnTy : List FormTy -> Type
interpCheckedFnTy [] = () -- TODO: should be form effect stuff here
interpCheckedFnTy (x :: xs) = Maybe (interpFormTy x) -> interpCheckedFnTy xs
{-
serialiseFnType : Type -> String
serialiseFnType ((Maybe String) -> y) = ?mv1 -- "Maybe String -> " ++ serialiseFnType y
serialiseFnType ((Maybe Int) -> y) = ?mv2 -- "Maybe Int -> " ++ serialiseFnType y
serialiseFnType _ = ?mv3 -- "booo"
-}
-- data Step : 
data FormRes : (elems : List FormTy) -> Type where
  FR : List FormTy -> FormRes elems


data Form : Effect where
  AddTextBox : (fty : FormTy) -> (val_ty : interpFormTy fty) -> Form (FormRes xs) (FormRes (xs ++ [fty])) () -- prepending is better, todo: neaten
  Submit : interpCheckedFnTy xs -> Form (FormRes xs) (FormRes []) String

instance Handler Form IO where
  handle (FR elems) (Submit fn) k = do
    k (FR []) "Serialised stuff"
  handle (FR elems) (AddTextBox fty val) k = do
    k (FR (fty :: elems)) ()

FORM : Type -> EFFECT
FORM t = MkEff t Form

addTextBox : (fty : FormTy) -> (interpFormTy fty) -> EffM m [FORM (FormRes xs)] [FORM (FormRes (xs ++ [fty]))] ()
addTextBox ty val = (AddTextBox ty val)

addSubmit : (interpCheckedFnTy xs) -> EffM m [FORM (FormRes xs)] [FORM (FormRes [])] String
addSubmit fn = (Submit fn)

sampleHandler : Maybe String -> Maybe Int -> ()
sampleHandler name age = ()


-- Effects.>>= : (EffM m xs xs' a) -> (a -> EffM m xs' xs'' b) -> EffM xs xs'' b
-- addTextBox str >>= addTextBox int : EffM m [] [FormString] () -> EffM m [FormString] [FormInt, FormString] () -> EffM m [] [FormInt, FormString]
myForm : Eff m [FORM (FormRes [])] String
myForm = do addTextBox FormString "Simon"
            addTextBox FormInt 21
            addSubmit sampleHandler
  --(addTextBox FormString "Simon") >>= ((\_ => addTextBox FormInt 21) >>= (\_ => addSubmit sampleHandler))
            
