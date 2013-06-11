module Main
import Cgi
import Effects

doCGIStuff : ActionTy Int 
doCGIStuff = do output "Hello, world!\n"
                return 0

main : IO ()
main = do
  _ <- runCGI doCGIStuff
  return ()

