-- File containing all types used in form processing, and associated instances
module IdrisWeb.Form.FormTypes
import Decidable.Equality
import SQLite
import Parser
import Cgi

%access public

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


interpFnTy : Vect FormTy n -> Type
interpFnTy tys = interpFnTy' (reverse tys)
  where interpFnTy' : Vect FormTy n -> Type
        interpFnTy' [] = () -- TODO: should be form effect stuff here
        interpFnTy' (x :: xs) = interpFormTy x -> interpFnTy' xs

FormHandler : List EFFECT -> Type -> Type
FormHandler effs t = EffM IO effs effs t

showFormVal : (fty : FormTy) -> interpFormTy fty -> String
showFormVal FormString s = s
showFormVal FormInt i = show i



{- Serialisable web effects -}
data WebEffect = CgiEffect
               | SqliteEffect

cgiNotSqlite : CgiEffect = SqliteEffect -> _|_
cgiNotSqlite refl impossible

instance DecEq WebEffect where
  decEq CgiEffect CgiEffect = Yes refl
  decEq SqliteEffect SqliteEffect = Yes refl
  decEq CgiEffect SqliteEffect = No cgiNotSqlite
  decEq SqliteEffect CgiEffect = No (negEqSym cgiNotSqlite)

MkHandlerFnTy : Type
MkHandlerFnTy = (List FormTy, List WebEffect, FormTy)

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



mkHandlerFn' : List FormTy -> List WebEffect -> FormTy -> Type
mkHandlerFn' [] effs ty = FormHandler (interpWebEffects effs) (interpFormTy ty)
mkHandlerFn' (x :: xs) effs ty = Maybe (interpFormTy x) -> mkHandlerFn' xs effs ty

mkHandlerFn : MkHandlerFnTy -> Type 
mkHandlerFn (tys, effs, ret) = mkHandlerFn' tys effs ret


