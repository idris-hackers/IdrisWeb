module Main
import FormEffect
import Effects
import SQLite


-- We could also have SQLITE, or CGI here perhaps
sampleHandler : Maybe String -> Maybe String -> FormHandler [SQLITE ()] ()
sampleHandler name age = return ()


-- Effects.>>= : (EffM m xs xs' a) -> (a -> EffM m xs' xs'' b) -> EffM xs xs'' b
-- addTextBox str >>= addTextBox int : EffM m [] [FormString] () -> EffM m [FormString] [FormInt, FormString] () -> EffM m [] [FormInt, FormString]
myForm : UserForm
myForm = do addTextBox FormString "Simon"
            addTextBox FormInt 21
            addSubmit sampleHandler [SQLITE ()] ()
  --(addTextBox FormString "Simon") >>= ((\_ => addTextBox FormInt 21) >>= (\_ => addSubmit sampleHandler))




main : IO ()
main = do let ser_form = mkForm "myform" myForm
          putStrLn ser_form

