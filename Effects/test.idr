module Main
import Cgi
import Effects

doCGIStuff : CGIProg Int 
doCGIStuff = do output "Hello, world!\n"
                pinesvar <- queryPostVar "pines"
                case pinesvar of
                    Just str => do output ("Pines var: " ++ str ++ "\n")
                                   return 0
                    Nothing =>  do output "No pines var :( \n"
                                   return 0 

main : IO ()
main = do
  runCGI doCGIStuff
  return ()

