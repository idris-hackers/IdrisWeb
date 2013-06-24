module IdrisWeb.Form.FormEffect
import Effects

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
        
interpCheckedFnTy : Vect FormTy n -> Type
interpCheckedFnTy tys = interpCheckedFnTy' (reverse tys)
  where interpCheckedFnTy' : Vect FormTy n -> Type
        interpCheckedFnTy' [] = () -- TODO: should be form effect stuff here
        interpCheckedFnTy' (x :: xs) = Maybe (interpFormTy x) -> interpCheckedFnTy' xs

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

  data HasType : (i : Fin n) -> Vect FormTy n -> FormTy -> Type where
      stop : HasType fO (t :: G) t
      pop  : HasType k G t -> HasType (fS k) (u :: G) t

  data FormRes : Vect FormTy n -> Type where
    FR : Env G -> FormRes G

  lookup : HasType i G t -> Env G -> interpFormTy t
  lookup stop    (x :: xs) = x
  lookup (pop k) (x :: xs) = lookup k xs

  update : HasType i G t -> Env G -> interpFormTy t -> Env G
  update stop (x :: xs) newval = (newval :: xs)
  update (pop k) (x :: xs) newval = x :: (update k xs newval)



  data Form : Effect where
    AddTextBox : (fty : FormTy) -> (val_ty : interpFormTy fty) -> Form (FormRes G) (FormRes (fty :: G)) () -- prepending is better, todo: neaten
    Submit : interpCheckedFnTy G -> Form (FormRes G) (FormRes []) String

  instance Handler Form IO where
    handle (FR vals) (Submit fn) k = do
      k (FR []) "Serialised stuff"
    handle (FR vals) (AddTextBox fty val) k = do
      k (FR (val :: vals)) ()

  FORM : Type -> EFFECT
  FORM t = MkEff t Form

  addTextBox : (fty : FormTy) -> (interpFormTy fty) -> EffM m [FORM (FormRes G)] [FORM (FormRes (fty :: G))] ()
  addTextBox ty val = (AddTextBox ty val)

  addSubmit : (interpCheckedFnTy G) -> EffM m [FORM (FormRes G)] [FORM (FormRes [])] String
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
