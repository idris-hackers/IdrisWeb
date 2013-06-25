module IdrisWeb.Form.FormHandler 
import Effects
--import Cgi
import FormEffect
import SQLite

--record HandlerState : Type where

--data FormHander : Effect where
  --HandleForm : 

Handler : Type 
Handler = List FormTy -> List EFFECT -> FormTy -> Type

mkHandlerFn : List FormTy -> List EFFECT -> FormTy -> Type
mkHandlerFn [] effs ty = FormHandler effs (interpFormTy ty)
mkHandlerFn (x :: xs) effs ty = Maybe (interpFormTy x) -> mkHandlerFn xs effs ty

using (G : Vect (List FormTy -> List EFFECT -> FormTy -> Type) n)
  data FHEnv : Vect (List FormTy -> List EFFECT -> FormTy -> Type) n -> Type where
    Nil : FHEnv Nil 
    (::) : mkHandlerFn tys effs ret -> FHEnv G -> FHEnv (a :: G) -- hmmm... will this work?

  data HandlerRes : (Fin n) -> Vect (List FormTy -> List EFFECT -> FormTy -> Type) n -> Type where
    FR : (Fin n) -> FHEnv G -> HandlerRes G

  data FormHandler : Effect where
    AddHandler : (ftys : List FormTy) -> (effs : List EFFECT) -> (ret : FormTy) -> FormHandler (FormRes G) (FormRes (a :: G))

