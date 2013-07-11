import CgiTypes
import Session
import Effects

-- Key for the session id in the cookie
SESSION_VAR : String
SESSION_VAR = "session_id"

getOrCreateSession : EffM IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised)]
                                [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionInitialised)]
                                (Maybe (SessionID, SessionData))
getOrCreateSession = do
  -- Firstly grab the session ID from the cookies, if it exists
  s_var <- lift (Keep (Drop (SubNil))) (queryCookieVar SESSION_VAR)
  case s_var of
       -- If it does exist, then attempt to load the session
       Just s_id => do res <- (lift (Drop (Keep (SubNil))) (loadSession s_id))
                       case res of 
                           Just res' => Effects.pure $ Just (s_id, res')
                           Nothing => Effects.pure $ Nothing 
       -- If it doesn't, create a new, empty session
       Nothing => do res <- lift (Drop (Keep (SubNil))) (createSession [])
                     case res of
                          Just s_id' => Effects.pure $ Just (s_id', [])
                          Nothing => Effects.pure $ Nothing

setSessionCookie : Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionInitialised)] Bool
setSessionCookie = do s_id <- lift (Drop (Keep (SubNil))) getSessionID
                      case s_id of
                           Just s_id => do lift (Keep (Drop (SubNil))) (setCookie "session_id" s_id)
                                           pure True
                           Nothing => pure False


total
updateVar : String -> SessionDataType -> SessionData -> SessionData
updateVar new_key new_val [] = [(new_key, new_val)]
updateVar new_key new_val ((key, val)::xs) = if (key == new_key) then ((key, new_val):: xs)
                                                                 else ((key, val) :: (updateVar new_key new_val xs))
