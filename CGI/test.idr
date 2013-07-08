module Main
import Cgi
import Effects

total
updateVar : String -> SessionDataType -> SessionData -> SessionData
updateVar new_key new_val [] = [(new_key, new_val)]
updateVar new_key new_val ((key, val)::xs) = if (key == new_key) then ((key, new_val):: xs)
                                                                 else ((key, val) :: (updateVar new_key new_val xs))

incrementAndGetCount : SessionData -> Eff IO [SESSION (SessionRes SessionInitialised)] Int
incrementAndGetCount sd = case lookup "counter" sd of
                            Just (SInt c) => do updateSession $ updateVar "counter" (SInt (c + 1)) sd
                                                return (c + 1)
                            _ => do updateSession $ updateVar "counter" (SInt 1) sd -- Start a new counter
                                    return 1


-- TODO: Update CGIProg to the EffM definition instead of the Eff definition
useSession : Maybe SessionData -> EffM IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionInitialised)]
                                          [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised)]
                                          ()
useSession (Just sd) = do count <- incrementAndGetCount sd
                          output $ "You have visited this page " ++ (show count) ++ " time(s)!"
                          writeSessionToDB
                          pure ()
useSession Nothing = do output "There was a problem retrieving your session."
                        -- Delete the session for good measure
                        -- whoops, haven't written this yet
                        discardSession
                        

foundSessionID : SessionID -> Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionInitialised)] ()
foundSessionID s_id = do session_data <- loadSession s_id
                         -- Failure is handled by pattern matching in useSession
                         useSession session_data
                         --discardSession
                         pure ()
 
doCGIStuff : Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised)] ()
doCGIStuff = do output "Hello, world!\n"
                -- TODO: Ideally, we wouldn't deal with this in a raw way like this
                session_var <- queryCookieVar "session_id"
                case session_var of
                    -- We've found a stored session ID in the cookie.
                    Just s_id => foundSessionID
                                 discardSession 
                    Nothing =>  do let sd = [("counter", SInt 0)] ++ Prelude.List.Nil
                                   sess_id <- createSession sd
                                   case sess_id of 
                                     Just s_id => do setCookie "session_id" s_id
                                                     useSession (Just sd) -- hackity hack
                                                     discardSession
                                     Nothing => do output "There was an error creating a session for you :("
                                                   discardSession
                                                   
main : IO ()
main = do
  runCGI [initCGIState, InvalidSession] doCGIStuff
  pure ()

