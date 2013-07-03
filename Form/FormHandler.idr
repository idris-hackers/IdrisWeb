module IdrisWeb.Form.FormHandler 
import Effects
import CgiTypes
import FormEffect
import SQLite -- Probably need to separate this out into SQLiteTypes
import Parser
import Decidable.Equality
--import DecListTup
%access public

--record HandlerState : Type where

--data FormHander : Effect where
  --HandleForm : 

{- 
   We index over MkHandlerFnTy, as this is the data we need to construct the type
   of the handling function using mkHandlerFn.
   Cleaner to store it as a tuple.
-}


data RegHandler : Type where
  RH : (ft : MkHandlerFnTy) -> mkHandlerFn ft -> RegHandler


lengthAsInt : List a -> Int
lengthAsInt xs = fromInteger lengthAsInteger
  where lengthAsInteger : Integer
        lengthAsInteger = cast $ length xs

-- Gets the serialised value as the given type, if it's possible
--total
parseSerialisedValue : (ty : FormTy) -> String -> Maybe (interpFormTy ty)
parseSerialisedValue FormString val = Just val
parseSerialisedValue FormInt val = case parse int val of
                                        Left err => Nothing
                                        Right (i, _) => Just i
-- FIXME: Placeholders for now, todo: improve parser
parseSerialisedValue FormBool val = case parse bool val of
                                         Left err => Nothing
                                         Right (b, _) => Just b
parseSerialisedValue FormFloat val = Just 0.0

getAs : (ty : FormTy) -> Int -> List (String, String) -> Maybe (interpFormTy ty)
getAs ty n args = do val <- lookup ("arg" ++ (show n)) args
                     parseSerialisedValue ty val

-- Disregards args
public
mkFinalHandlerType : MkHandlerFnTy -> Type
mkFinalHandlerType (_, effs, ret) = FormHandler (interpWebEffects effs) (interpFormTy ret)

getWebEnv' : (effs : List WebEffect) -> (InitialisedCGI TaskRunning) -> Effects.Env IO (interpWebEffects effs)
getWebEnv' [] _ = []
getWebEnv' (CgiEffect :: xs) cgi = (cgi) :: getWebEnv' xs cgi
getWebEnv' (SqliteEffect :: xs) cgi = (()) :: getWebEnv' xs cgi

getEffects : (List FormTy, List WebEffect, FormTy) -> List EFFECT
getEffects (_, effs, _) = interpWebEffects effs

--getWebEnv : (frm_ty : MkHandlerFnTy) -> Effects.Env IO (getEffects frm_ty)
--getWebEnv (_, effs, _) = getWebEnv' effs


data PopFn : Type where
  PF : (w_effs : List WebEffect) -> (ret_ty : FormTy) -> 
       (effs : List EFFECT) -> (env : Effects.Env IO effs) -> 
       Eff IO effs (interpFormTy ret_ty) -> PopFn

evalFn : (mkHTy : MkHandlerFnTy) -> 
         (counter : Int) -> -- TODO: ftys would be far better as a Vect over some finite set indexed over n
         (arg_num : Int) ->
         (args : List (String, String)) ->
         (fn : mkHandlerFn mkHTy) ->
         (InitialisedCGI TaskRunning) ->
         Maybe (mkFinalHandlerType mkHTy, PopFn)
evalFn (Prelude.List.Nil, effs, ret) counter argnum args fn cgi = Just (fn, (PF effs ret (interpWebEffects effs) (getWebEnv' effs cgi) fn))-- ?mv -- Just (fn, )
evalFn ((fty :: ftys), effs, ret) counter argnum args fn cgi = let arg = getAs fty (argnum - counter + 1) args in
                                                                   evalFn (ftys, effs, ret) (counter - 1) argnum args (fn arg) cgi

{- Parser functions to grab arguments from a form -}
strFty : List (String, FormTy)
strFty = [("str", FormString), ("int", FormInt), ("bool", FormBool), ("float", FormFloat)]

strEff : List (String, WebEffect)
strEff = [("cgi", CgiEffect), ("sqlite", SqliteEffect)]

arg : Parser FormTy
arg = do a_str <- strToken
         char ';'
         case lookup a_str strFty of
              Just fty => pure fty
              Nothing  => failure $ "Attempted to deserialise nonexistent function type " ++ a_str

webEff : Parser WebEffect
webEff = do e_str <- strToken
            char ';'
            case lookup e_str strEff of
                 Just w_eff => pure w_eff
                 Nothing => failure $ "Attempted to deserialise nonexistent web effect " ++ e_str

parseFormFn' : Parser (String, MkHandlerFnTy)
parseFormFn' = do name <- strToken
                  char ':'
                  args <- many arg
                  char ':' -- List delimiter
                  effs <- many webEff
                  char ':'
                  ret <- arg
                  pure (name, (args, effs, ret))
                  
-- The hidden field "handler" will be of the form:
-- <handler name>:type1;type2...typen;:<effects, eventually>:return type;
parseFormFn : String -> Maybe (String, MkHandlerFnTy)
parseFormFn str = case parse parseFormFn' str of
                       Left err => Nothing
                       Right ((name, parsed_fn), _) => Just (name, parsed_fn)

checkFunctions : (reg_fn_ty : MkHandlerFnTy) -> 
                 (frm_fn_ty : MkHandlerFnTy) -> 
                 mkHandlerFn reg_fn_ty -> 
                 Maybe (mkHandlerFn frm_fn_ty)
--checkFunctions reg_fn_ty frm_fn_ty reg_fn = if reg_fn_ty == frm_fn_ty then Just (RH frm_fn_ty reg_fn) else Nothing
checkFunctions reg_fn_ty frm_fn_ty reg_fn with (decEq reg_fn_ty frm_fn_ty)
  checkFunctions frm_fn_ty frm_fn_ty reg_fn | Yes refl = Just reg_fn
  checkFunctions reg_fn_ty frm_fn_ty reg_fn | No _ = Nothing
                                          

getReturnType : MkHandlerFnTy -> FormTy
getReturnType (_, _, ret) = ret


--getHandlerFnTy : List (String, String) -> List (String, RegHandler) -> Maybe 
-- Takes in a list of form POST / GET vars, a list of available handlers, and returns the appropriate handler
getHandler : List (String, String) -> 
             (handler_name : String) -> 
             (handler_ty : MkHandlerFnTy) -> 
             List (String, RegHandler) -> 
             (InitialisedCGI TaskRunning) -> 
             Maybe (mkFinalHandlerType handler_ty, PopFn)
getHandler vars handler_name handler_type handlers cgi = do 
                              (RH rh_type rh_fn) <- lookup handler_name handlers
                              rh_fn' <- checkFunctions rh_type handler_type rh_fn
                              let f_rh = (RH handler_type rh_fn')
                              let (tys, effs, ret) = handler_type
                              let arg_len = lengthAsInt tys 
                              evalFn handler_type arg_len arg_len vars rh_fn' cgi
                              -- ?mv
                              --pure ((tys, effs, ret), final_handler)
--getEffEnv : (mkty : MkHandlerFnTy) -> Effects.Env IO (

--getEffEnv' : (ws_ty : List WebEffect) -> Effects.Env IO (interpWebEffects ws_ty) 
--getEffEnv' ws = getWebEnv' ws
-- TODO: Return types
-- Check the POST / GET vars for the handler field, the list of registered handlers,
-- and runs the appropriate handler if it exists.
-- If the handler runs successfully, True will be returned. Otherwise, False will be returned.
-- This is an ugly mess; todo: cleanup

getWebEffects : MkHandlerFnTy -> List WebEffect
getWebEffects (_, effs, _) = effs


--Eff IO 
-- TODO: GetEnv on the CGI
executeHandler : List (String, String) -> List (String, RegHandler) -> (InitialisedCGI TaskRunning) -> IO Bool
executeHandler vars handlers cgi = case (lookup "handler" vars) >>= parseFormFn of
                                      Just (name, frm_ty) => case getHandler vars name frm_ty handlers cgi of
                                        Just (fn, (PF effs ret conc_effs env fn')) => do run env fn'
                                                                                         pure True 
                                        Nothing => pure False
                                      Nothing => pure False
                                               
                       --Right (_, _) => Nothing -- Some bits didn't parse
{- TODO. Not sure this will be possible at this early stage. -}
--effect : Parser EFFECT
--effect = do ...




