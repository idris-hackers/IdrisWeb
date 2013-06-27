module IdrisWeb.Form.FormHandler 
import Effects
import Cgi
import FormEffect
import SQLite
import Parser
import Decidable.Equality
--record HandlerState : Type where

--data FormHander : Effect where
  --HandleForm : 

{- 
   We index over MkHandlerFnTy, as this is the data we need to construct the type
   of the handling function using mkHandlerFn.
   Cleaner to store it as a tuple.
-}
data WebEffect = CgiEffect
               | SqliteEffect

MkHandlerFnTy : Type
MkHandlerFnTy = (List FormTy, List WebEffect, FormTy)

instance Eq WebEffect where
  (==) CgiEffect CgiEffect = True
  (==) SqliteEffect SqliteEffect = True
  (==) _ _ = False

total
interpWebEffect : WebEffect -> EFFECT
interpWebEffect CgiEffect = (CGI ())
interpWebEffect SqliteEffect = (SQLITE ())

interpWebEffects : List WebEffect -> List EFFECT
interpWebEffects [] = []
interpWebEffects (x :: xs) = interpWebEffect x :: interpWebEffects xs


mkHandlerFn' : List FormTy -> List WebEffect -> FormTy -> Type
mkHandlerFn' [] effs ty = FormHandler (interpWebEffects effs) (interpFormTy ty)
mkHandlerFn' (x :: xs) effs ty = Maybe (interpFormTy x) -> mkHandlerFn' xs effs ty

mkHandlerFn : MkHandlerFnTy -> Type 
mkHandlerFn (tys, effs, ret) = mkHandlerFn' tys effs ret


data RegHandler : Type where
  RH : (ft : MkHandlerFnTy) -> mkHandlerFn ft -> RegHandler


lengthAsInt : List a -> Int
lengthAsInt xs = fromInteger lengthAsInteger
  where lengthAsInteger : Integer
        lengthAsInteger = cast $ length xs

-- Gets the serialised value as the given type, if it's possible
total
parseSerialisedValue : (ty : FormTy) -> String -> Maybe (interpFormTy ty)
parseSerialisedValue FormString val = Just val
parseSerialisedValue FormInt val = case parse int val of
                                        Left err => Nothing
                                        Right (i, _) => Just i
-- FIXME: Placeholders for now, todo: improve parser
parseSerialisedValue FormBool val = Just False
parseSerialisedValue FormFloat val = Just 0.0

getAs : (ty : FormTy) -> Int -> List (String, String) -> Maybe (interpFormTy ty)
getAs ty n args = do val <- lookup ("arg" ++ (show n)) args
                     parseSerialisedValue ty val

evalFn : (ftys : List FormTy) -> 
         (counter : Int) -> -- TODO: ftys would be far better as a Vect over some finite set indexed over n
         (arg_num : Int) ->
         (effs : List WebEffect) -> 
         (ret : FormTy) -> 
         (args : List (String, String)) ->
         (fn : mkHandlerFn' ftys effs ret) ->
         Maybe (FormHandler (interpWebEffects effs) (interpFormTy ret))
evalFn [] counter argnum effs ret args fn = Just fn
evalFn (fty :: ftys) counter argnum effs ret args fn = let arg = getAs fty (argnum - counter + 1) args in
                                                           evalFn ftys (counter - 1) argnum effs ret args (fn arg)

{- Parser functions to grab arguments from a form -}
strFty : List (String, FormTy)
strFty = [("str", FormString), ("int", FormInt), ("bool", FormBool), ("float", FormFloat)]

arg : Parser FormTy
arg = do a_str <- strToken
         char ';'
         case lookup a_str strFty of
              Just fty => pure fty
              Nothing  => failure $ "Attempted to deserialise nonexistent function type " ++ a_str

parseFormFn' : Parser (String, MkHandlerFnTy)
parseFormFn' = do name <- strToken
                  args <- many arg
                  char ':' -- List delimiter
                  let effs = Prelude.List.Nil -- [] -- for now
                  char ':'
                  ret <- arg
                  pure (name, (args, effs, ret))
                  
-- The hidden field "handler" will be of the form:
-- <handler name>:type1;type2...typen:<effects, eventually>:return type;
parseFormFn : String -> Maybe (String, MkHandlerFnTy)
parseFormFn str = case parse parseFormFn' str of
                       Left err => Nothing
                       Right ((name, parsed_fn), _) => Just (name, parsed_fn)

checkFunctions : (reg_fn : MkHandlerFnTy) -> (frm_fn : MkHandlerFnTy) -> mkHandlerFn reg_fn -> Maybe (mkHandlerFn frm_fn)
checkFunctions reg_fn_ty frm_fn_ty reg_fn with (decEq reg_fn_ty frm_fn_ty)
  | Yes _ = Just reg_fn
  | No _ = Nothing

-- Takes in a list of form POST / GET vars, a list of available handlers, and returns the appropriate handler
getHandler : List (String, String) -> List (String, RegHandler) -> Maybe (FormHandler effs (interpFormTy ret))
                              -- Get the handler field from the form data
getHandler vars handlers = do handler_field <- lookup "handler" vars
                              -- Parse the handler field
                              (handler_name, handler_type) <- parseFormFn handler_field
                              -- Lookup the handler in the list of registered handlers
                              (RH rh_type rh_fn) <- lookup handler_name handlers
                              -- Check if the MkHandlerFnTypes, and therefore the function types, are the same
                              (RH rh_type' rh_fn') <- checkFunctions rh_type handler_type rh_fn
                              -- Get the arguments, and populate the function
                              let (tys, effs, ret) = rh_type'
                              let arg_len = lengthAsInt tys -- we're discounting the hidden field
                              fn <- evalFn tys arg_len arg_len effs ret vars rh_fn'
                              pure fn
                              --else Nothing -- This would really be better as an Either, I think.


                       --Right (_, _) => Nothing -- Some bits didn't parse
{- TODO. Not sure this will be possible at this early stage. -}
--effect : Parser EFFECT
--effect = do ...




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
