module Main
import Cgi
import Effects
import Effect.StdIO

doCGIStuff : CGIProg [STDIO] Int 
doCGIStuff = do output "Hello, world!\n"
                putStr "IO effect says hi\n"
                pinesvar <- queryPostVar "pines"
                setCookie "pines" "pinesington"
                case pinesvar of
                    Just str => do output ("Pines var: " ++ str ++ "\n")
                                   pure 0
                    Nothing =>  do output "No pines var :( \n"
                                   pure 0 

main : IO ()
main = do
  runCGI [ICgi (CGIInf [] [] [] "" "" "this shouldn't happen"), () ] doCGIStuff
  pure ()

