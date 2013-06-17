module IdrisWeb.Form.Form
-- DSL to describe web forms

-- Types allowed by the form handler
data FormTy = TyStr 
            | TyInt 
            | TyBool 
            | TyNone 

-- Submission methods
data Method = POST | GET

total
interpTy : FormTy -> Type
interpTy TyStr  = String
interpTy TyInt  = Int
interpTy TyBool = Bool
interpTy TyNone = ()


-- Each element of the form is associated with some handler type
--data FormElement : FormTy -> Type where
--  FE : FormElement t
{-
data FormElement = Text FormTy
                 | Hidden FormTy
                 | CheckBox
                 | Password
                 | Submit
                 -}
{-
using (ty : FormTy)
  data FormElement : FormTy -> Type where
    Text :  -> 
           -}

data FormElement : Type where
  -- Text and hidden fields are parameterised on type.
  Text : (a : FormTy) -> interpTy a -> FormElement 
  Hidden : (a : FormTy) -> interpTy a -> FormElement
  CheckBox : Bool -> FormElement
  Password : String -> FormElement


instance Eq FormTy where
  (==) TyStr TyStr = True
  (==) TyInt TyInt = True
  (==) TyBool TyBool = True
  (==) TyNone TyNone = True
  (==) _ _ = False

{-
total
elemTy : FormElement -> FormTy
elemTy (Text ty) = ty
elemTy (Hidden ty) = ty
elemTy Password = TyStr 
elemTy CheckBox = TyBool
elemTy Submit = TyNone
-}

record Form : Type where
     MkForm : (name : String) -> 
              (method : Method) ->
              (action : String) ->
              (elems : List (String, FormElement)) -> Form
              --(elems : (k ** Vect (String, FormElement) k)) -> Form

myFormArgs : List (String, FormElement)
myFormArgs = [("name", Text TyStr "hello")
             ,("age", Text TyInt 10)
             ,("receive_offers", CheckBox True)
             ]
             
serialiseFormElement : (String, FormElement) -> String
serialiseFormElement (name, (Text TyStr x)) = "<input name=\"" ++ (show name) ++ "\" type=\"text\" value=\"" ++ (show x) ++ "\">"

