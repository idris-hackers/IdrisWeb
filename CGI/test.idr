module Main
import Cgi
import Effects
import Session
import SessionUtils



incrementAndGetCount : SessionData -> Eff IO [SESSION (SessionRes SessionInitialised)] Int
incrementAndGetCount sd = case lookup "counter" sd of
                            Just (SInt c) => do updateSession $ updateVar "counter" (SInt (c + 1)) sd
                                                return (c + 1)
                            _ => do updateSession $ updateVar "counter" (SInt 1) sd -- Start a new counter
                                    return 1


-- TODO: Update CGIProg to the EffM definition instead of the Eff definition
useSession : Maybe (SessionID, SessionData) -> EffM IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionInitialised)]
                                          [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised)]
                                          ()
useSession (Just (si, sd)) = do count <- lift (Drop (Keep (SubNil))) (incrementAndGetCount sd)
                                lift (Keep (Drop (SubNil))) (output $ "You have visited this page " ++ (show count) ++ " time(s)!")
                                lift (Drop (Keep (SubNil))) writeSessionToDB
                                Effects.pure ()
useSession Nothing = do output "There was a problem retrieving your session."
                        -- Delete the session for good measure
                        -- whoops, haven't written this yet
                        discardSession
                        Effects.pure ()
                        

doCGIStuff : Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised)] ()
doCGIStuff = do lift (Keep (Drop (SubNil))) (output "Hello, world!\n")
                session <- getOrCreateSession
                useSession session

main : IO ()
main = do
  runCGI [initCGIState, InvalidSession] doCGIStuff
  pure ()


