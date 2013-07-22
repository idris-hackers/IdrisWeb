import CgiTypes
import Session
import Effects
%access public

-- Key for the session id in the cookie
public
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
                           -- TODO: This should create a new session
                           Nothing => Effects.pure $ Nothing 
       -- If it doesn't, create a new, empty session
       Nothing => do res <- lift (Drop (Keep (SubNil))) (createSession [])
                     case res of
                          Just s_id' => Effects.pure $ Just (s_id', [])
                          Nothing => Effects.pure $ Nothing

setSessionCookie : Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionInitialised)] Bool
setSessionCookie = do s_id <- lift (Drop (Keep (SubNil))) getSessionID
                      case s_id of
                           Just s_id => do lift (Keep (Drop (SubNil))) (setCookie SESSION_VAR s_id)
                                           pure True
                           Nothing => pure False


total
updateVar : String -> SessionDataType -> SessionData -> SessionData
updateVar new_key new_val [] = [(new_key, new_val)]
updateVar new_key new_val ((key, val)::xs) = if (key == new_key) then ((key, new_val):: xs)
                                                                 else ((key, val) :: (updateVar new_key new_val xs))



-- Takes in two functions: one to execute if there is a valid, authenticated
-- session cookie, and one to execute if there isn't.
withSession : (SessionData ->
               EffM IO [CGI (InitialisedCGI TaskRunning), 
                        SESSION (SessionRes SessionInitialised), 
                        SQLITE ()] 
                       [CGI (InitialisedCGI TaskRunning), 
                        SESSION (SessionRes SessionUninitialised), 
                        SQLITE ()] ()) ->
               EffM IO [CGI (InitialisedCGI TaskRunning), 
                        SESSION (SessionRes SessionInitialised), 
                        SQLITE ()] 
                       [CGI (InitialisedCGI TaskRunning), 
                        SESSION (SessionRes SessionUninitialised), 
                        SQLITE ()] () -> 
               Eff IO [CGI (InitialisedCGI TaskRunning), 
                       SESSION (SessionRes SessionUninitialised), 
                       SQLITE ()] ()
withSession is_auth_fn not_auth_fn = do
  s_var <- queryCookieVar SESSION_VAR
  case s_var of
       -- If it does exist, then attempt to load the session
       Just s_id => do res <- loadSession s_id--(lift (Drop (Keep (SubNil))) (loadSession s_id))
                       case res of 
                           -- If we've got a valid session, execute the user-specified
                           -- function with the gathered session data
                           Just res' => do is_auth_fn res'
                                           pure ()
                           -- If not, execute the specified failure function
                           Nothing => do not_auth_fn
                                         pure ()
      -- If there's no session variable, execute the failure function (somehow)
      -- HACK: this loadSession won't succeed, yet will transfer into the other state so
      -- that the not_auth_fn can be run. Probably better ways of doing it
       Nothing => do loadSession ""
                     not_auth_fn
                     pure ()




