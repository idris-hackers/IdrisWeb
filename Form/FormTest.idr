module Main
--module IdrisWeb.Form.FormTest
import Cgi
import Effects
--import Debug.Trace

total
outputCommaList : List String -> String
outputCommaList [] = ""
outputCommaList [x] = x ++ "."
outputCommaList (x :: y :: []) = x ++ " and " ++ y ++ "."
outputCommaList (x :: xs) = x ++ ", " ++ outputCommaList xs

sampleHandler : Maybe String -> 
                Maybe Int -> 
                Maybe Int -> 
                Maybe String -> 
                Maybe (List String) ->
                FormHandler [CGI (InitialisedCGI TaskRunning)]
sampleHandler (Just name) (Just age) (Just num) (Just p_lang) (Just known_langs) = do 
                                                                   output ("Your name is: " ++ name ++ 
                                                                     ", and you are " ++ (show age) ++ " years old!" ++ 
                                                                     "<br>You selected number: " ++ (show num) ++
                                                                     ", and said your favourite programming language is " ++ p_lang ++
                                                                     ". You said you also knew " ++ (outputCommaList known_langs))
                                                                   pure ()
sampleHandler _ _ _ _ _ = do output "There was an error processing form data."
                             pure ()

sampleForm : UserForm --UserForm
sampleForm = do addTextBox "Name: " FormString (Just "Simon")
                addTextBox "Age: " FormInt Nothing-- 21
                --addTextBox FormInt 21
                addSelectionBox "Pick a number!" FormInt [1,2,3,4] ["One", "Two", "Three", "Four"]
                addRadioGroup "Favourite programming language: " FormString ["java", "haskell", "c"] ["Java", "Haskell", "C"] 2
                addCheckBoxes "Programming languages you know: " FormString ["java", "haskell", "c"] ["Java", "Haskell", "C"] [False, True, False]
                -- TODO: ideally, we'd have something like just "addSubmit sampleHandler" or grab it from the list of registered handlers
                addSubmit sampleHandler "sampleHandler" [CgiEffect] 


cgiAction : CGIProg [] ()
cgiAction = do output "<h1>Simon's awesome form stuff!</h1>\n"
               handlervar <- queryPostVar "handler"
               post_vars <- getPOSTVars
               let post_vars_str = foldr (\(k, v), str => str ++ k ++ " :-> " ++ v ++ "<br />") "" post_vars
               output "Post vars: <br />"
               output post_vars_str
               --sequence (map (\(name, val) => output $ "Name: " ++ name ++ ", " ++ val ++ "<br />") post_vars)
               case handlervar of
                    -- If at all poss, this needs to be cleaner. Users shouldn't have to type this
                    Just _ => do res <- handleForm [("sampleHandler", (RH ([FormString, FormInt, FormInt, FormString, (FormList FormString)], [CgiEffect]) sampleHandler))]
                                 output (show res)
                                 pure ()
                    Nothing => do addForm "sampleForm" "formtest" sampleForm 
                                  pure ()
main : IO ()
main = do runCGI [initCGIState] cgiAction
          pure ()
