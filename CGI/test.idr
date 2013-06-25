module Main
import Cgi
import Effects

doCGIStuff : CGIProg Int 
doCGIStuff = do output "Hello, world!\n"
                pinesvar <- queryPostVar "pines"
                case pinesvar of
                    Just str => do output ("Pines var: " ++ str ++ "\n")
                                   pure 0
                    Nothing =>  do output "No pines var :( \n"
                                   pure 0 

main : IO ()
main = do
  runCGI doCGIStuff
  pure ()

