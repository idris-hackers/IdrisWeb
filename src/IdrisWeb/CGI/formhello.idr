module Main
import Cgi
import Effects

sayHello : Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning)]
sayHello (Just name) = output ("Hello, " ++ name ++ "!")

handlers : HandlerList
handlers = [(handler args=[FormString], effects=[CgiEffect], fn=sayHello, name="sayHello")]

showHelloForm : UserForm
showHelloForm = do
  addTextBox "Name" FormString Nothing
  useEffects [CgiEffect]
  addSubmit sayHello handlers

cgiHello : CGIProg [] ()
cgiHello = do
  handler_set <- isHandlerSet
  if handler_set then do
    handleForm handlers
    return ()
  else do
    addForm "nameform" "helloform" showHelloForm
    return ()

main : IO ()
main = runCGI [initCGIState] cgiHello
