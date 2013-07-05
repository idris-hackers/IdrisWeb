module Main
--module IdrisWeb.Form.FormTest
import Cgi
import Effects
--import Debug.Trace

sampleHandler : Maybe String -> 
                Maybe Int -> 
                FormHandler [CGI (InitialisedCGI TaskRunning)] (interpFormTy FormString)
sampleHandler (Just name) (Just age) = do output $ "Your name is: " ++ name ++ ", and you are " ++ (show age) ++ " years old!"
                                          pure "pines"
sampleHandler _ _ = do output "There was an error processing form data."
                       pure "pinesington"

sampleForm : UserForm
sampleForm = do addTextBox FormString "Simon"
                addTextBox FormInt 21
                -- TODO: ideally, we'd have something like just "addSubmit sampleHandler" or grab it from the list of registered handlers
                addSubmit sampleHandler "sampleHandler" [CgiEffect] FormBool 


cgiAction : CGIProg [] ()
cgiAction = do output "<h1>Simon's awesome form stuff!</h1>\n"
               handlervar <- queryPostVar "handler"
               {-post_vars <- getPOSTVars
               let post_vars_str = foldr (\(k, v), str => str ++ k ++ " :-> " ++ v ++ "<br />") "" post_vars
               output "Post vars: <br />"
               output post_vars_str
               -}
               case handlervar of
                    -- If at all poss, this needs to be cleaner. Users shouldn't have to type this
                    -- TODO: Integrate the Maybe into one of the effect fn types, so users don't have to do the extra case switc 
                    Just _ => do 
                      maybe_handler <- getHandler [("sampleHandler", (RH ([FormString, FormInt], [CgiEffect], FormString) sampleHandler))]
                      case maybe_handler of
                                      Just (handler, ty) => do res <- handleForm (Just handler) ty
                                                               case res of
                                                                    Just result => do --output result
                                                                                      pure ()
                                                                    Nothing => do output "Error executing handler"
                                                                                  pure ()
                                      Nothing => do output "Error getting handler"
                                                    pure ()
                    Nothing => do addForm "sampleForm" "formtest" sampleForm 
                                  pure ()

-- The initCgiState thing is a hack for now, hopefully can remove it at some point
main : IO ()
main = do runCGI [initCgiState] cgiAction
          pure ()
