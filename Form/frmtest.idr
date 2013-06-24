module Main
import FormEffect
import Effects



sampleHandler : Maybe String -> Maybe Int -> ()
sampleHandler name age = ()


-- Effects.>>= : (EffM m xs xs' a) -> (a -> EffM m xs' xs'' b) -> EffM xs xs'' b
-- addTextBox str >>= addTextBox int : EffM m [] [FormString] () -> EffM m [FormString] [FormInt, FormString] () -> EffM m [] [FormInt, FormString]
myForm : UserForm
myForm = do addTextBox FormString "Simon"
            addTextBox FormInt 21
            addSubmit sampleHandler
  --(addTextBox FormString "Simon") >>= ((\_ => addTextBox FormInt 21) >>= (\_ => addSubmit sampleHandler))




main : IO ()
main = do let ser_form = mkForm "myform" myForm
          putStrLn ser_form

