module Effect.Cgi

{- CGI Module, making use of the Effects DSL. 
   A large amount of credit goes to the author of Network.Cgi.
   SimonJF
-}

import Effects

-- Simple type synonym for a list of key value pairs
public
Vars : Type
Vars = List (String, String)

-- Type of the CGI Eff instance, including dependencies
CGIEff : Type -> Type --Eff IO [STATE CGIState, STDIO, ENVIRONMENT] Type
CGIEff t = Eff IO [STATE CGIState, STDIO, ENVIRONMENT] t

-- CGI Internal State
-- data CGIState : Type where
record CGIState : Type where
       CGISt : (GET : Vars) ->
               (POST : Vars) ->
               (Cookies : Vars) ->
               (UserAgent : String) ->
               (Headers : String) ->
               (Output : String) -> CGIState


-- State mutation functions, allowing for addition of headers / output
addHeaders : String -> CGIInfo -> CGIInfo
addHeaders str st = record { Headers = Headers st ++ str } st

add_Output : String -> CGIInfo -> CGIInfo
add_Output str st = record { Output = Output st ++ str } st


-- To create a CGI Program in Network.Cgi, one imports Network.Cgi,
-- and then creates a function CGIInfo -> IO (a, CGIInfo)) which is passed
-- into MkCGI.

-- This produces a complete CGI program, which can then be executed with
-- the required arguments.

{-
 For the implementation of CGI in the effects language, we'll want users
 to be able to specify some function to be run inside the CGI effect.

 This will take the form Eff IO [CGI] a, or more simply
 CGIEff a.

 Several CGI functions will also require access to the state, and some will require
 access to the environment variables.
-}



-- CGI Effect
data Cgi : Effect where
  -- Individual functions of the effect
  
  -- State retrival / update
  SetInfo : String -> Cgi CGIState CGIState ()
  GetInfo : Cgi CGIState CGIState CGIState 
  
  -- Output a string
  Output : String -> Cgi CGIState CGIState ()

  -- Retrieve the GET variables
  GETVars : Cgi CGIState CGIState Vars

  -- Retrieve the PUT variables
  PUTVars : Cgi CGIState CGIState Vars

  -- Retrieve the cookie variables
  CookieVars : Cgi CGIState CGIState Vars
:
