module IdrisWeb.Form.Form
-- DSL to describe web forms

-- Types allowed by the form handler
data FormTy = TyStr
            | TyInt
            | TyBool
            | TyNone

-- Submission methods
data Method = POST | GET


-- Each element of the form is associated with some handler type
--data FormElement : FormTy -> Type where
--  FE : FormElement t

data FormElement = Text FormTy
                 | Hidden FormTy
                 | CheckBox
                 | Password
                 | Submit


total
interpTy : FormTy -> Type
interpTy TyStr = String
interpTy TyInt = Int
interpTy TyBool = Bool
interpTy TyNone = ()

instance Eq FormTy where
  (==) TyStr TyStr = True
  (==) TyInt TyInt = True
  (==) TyBool TyBool = True
  (==) TyNone TyNone = True
  (==) _ _ = False

total
elemTy : FormElement -> FormTy
elemTy (Text ty) = ty
elemTy (Hidden ty) = ty
elemTy Password = TyStr 
elemTy CheckBox = TyBool
elemTy Submit = TyNone


record Form : Type where
     MkForm : (name : String) -> 
              (method : Method) ->
              (action : String) ->
              (elems : List (String, FormElement)) -> Form
              --(elems : (k ** Vect (String, FormElement) k)) -> Form

myFormArgs : List (String, FormElement)
myFormArgs = [("name", Text TyStr)
             ,("age", Text TyInt)
             ,("receive_offers", CheckBox)
             ]


{-
myFormArgs : (k ** Vect (String, FormElement) k)
myFormArgs = (_ ** [("name", Text TyStr)
                   ,("age", Text TyInt)
                   ,("receive_offers", CheckBox)
                   ]
             )
             -}


myForm : Form
myForm = MkForm "myform" POST "form.cgi" myFormArgs

argTy : Nat -> List (String, FormElement) -> FormTy
argTy O ((_, fe) :: fes) = elemTy fe
argTy (S k) [] = TyNone
argTy (S k) (fe :: fes) = argTy k fes

-- Dependently typed stuff now:
unitTestForm : so ( argTy (S (O)) myFormArgs == TyInt)
unitTestForm = oh

-- We want...
--data FormData = FormData (String, Type)

--List (String, FormElement) -> List (String, String) -> List (String, FormData)


{- Example form elements

MyForm : Form 
MyForm = [ ("name", Text TyStr) -- Text, but we expect a string
         , ("age", Text TyInt) --  Text, but we expect an integer
         , ("marital_status", Selection ["Single, Married, Divorced"]) -- We expect one of the given options: membership predicate?
         , ("receive_offers", CheckBox) -- We want this to be TyInt, without specifying it
         ]

-}

