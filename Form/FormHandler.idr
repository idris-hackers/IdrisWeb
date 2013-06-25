module IdrisWeb.Form.FormHandler 
import Effects
--import Cgi
import FormEffect
import SQLite

--record HandlerState : Type where

--data FormHander : Effect where
  --HandleForm : 

{- 
   We index over MkHandlerFnTy, as this is the data we need to construct the type
   of the handling function using mkHandlerFn.
   Cleaner to store it as a tuple.
-}
MkHandlerFnTy : Type
MkHandlerFnTy = (List FormTy, List EFFECT, FormTy)

mkHandlerFn' : List FormTy -> List EFFECT -> FormTy -> Type
mkHandlerFn' [] effs ty = FormHandler effs (interpFormTy ty)
mkHandlerFn' (x :: xs) effs ty = Maybe (interpFormTy x) -> mkHandlerFn' xs effs ty

mkHandlerFn : MkHandlerFnTy -> Type 
mkHandlerFn (tys, effs, ret) = mkHandlerFn' tys effs ret

using (H : Vect MkHandlerFnTy n)
  data FHEnv : Vect MkHandlerFnTy n -> Type where
    Nil : FHEnv Nil 
    (::) : mkHandlerFn a -> FHEnv H -> FHEnv (a :: H)

  data HasType : (i : Fin n) -> Vect MkHandlerFnTy n -> MkHandlerFnTy -> Type where
      stop : HasType fO (t :: H) t
      pop  : HasType k H t -> HasType (fS k) (u :: H) t


  lookup : HasType i H t -> FHEnv H -> mkHandlerFn t
  lookup stop    (x :: xs) = x
  lookup (pop k) (x :: xs) = lookup k xs

  update : HasType i H t -> FHEnv H -> mkHandlerFn t -> FHEnv H
  update stop    (x :: xs) newval = (newval :: xs)
  update (pop k) (x :: xs) newval = x :: (update k xs newval)

  htAsIndex : HasType i H t -> Int
  htAsIndex stop = 0
  htAsIndex (pop k) = 1 + (htAsIndex k)

  indexAsHt' : Nat -> Maybe HasType i H t 
  indexAsHt' O = stop
  indexAsHt' (S k) = pop (indexAsHt' k)

  indexAsHt : Int -> Maybe HasType i H t
  indexAsHt i = indexAsHt' (cast i)


  data HandlerRes : Vect MkHandlerFnTy n -> Type where
    HR : FHEnv H -> HandlerRes H

  data FormHandler : Effect where
    AddHandler : (ftys : List FormTy) -> 
                 (effs : List EFFECT) -> 
                 (ret : FormTy) -> 
                 mkHandlerFn' ftys effs ret -> 
                 FormHandler (HandlerRes H) (HandlerRes ((ftys, effs, ret) :: H)) () -- perhaps int, or some identifier to be returned instead?

    --GetHandler : (name : String) -> (ftys : List FormTy) -> (effs : List EFFECT) -> (ret : FormTy) -> Maybe (mkHandlerFn ftys effs ret)


{- Say we have a handler:

printNameAge : Maybe String -> Maybe Int -> FormHandler [CGI (InitialisedCGI TaskRunning)] ()
printNameAge ms mi = case (ms, mi) of
                          Just (ms', mi') => output $ "Name: " ++ ms' ++ ", age: " ++ show mi'
                          Nothing => output "Error parsing data."


Ideally, we want to add this like:

AddHandler printNameAge

But I'm not sure we can.


One thing we could do instead, though, is do something a bit like this:
AddHandler [FormString, FormInt] [CGI (InitialisedCGI TaskRunning)] FormUnit printNameAge
Ugly as hell, but then AddHandler would be

AddHandler : (ftys : List FormTy) -> (effs : List EFFECT) -> (ret : FormTy) -> mkHandlerFn' ftys effs ret -> FormHandler (HandlerRes G) (HandlerRes (ftys, effs, ret) :: G)

-}
