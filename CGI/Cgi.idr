module IdrisWeb.Effect.Cgi

-- CGI Module, making use of the Effects DSL. 
-- large amount of credit goes to the author of Network.Cgi.
-- SimonJF

import Effects
import CgiUtils


-- Information passed by CGI
record CGIInfo : Type where
       CGIInf : (GET : Vars) ->
                (POST : Vars) ->
                (Cookies : Vars) ->
                (UserAgent : String) ->
                (Headers : String) ->
                (Output : String) -> CGIInfo


-- CGI Concrete effect sig
public
CGI : Type -> EFFECT

-- Type of user-defined CGI Actions
public
CGIProg : Type -> Type

-- States in the state machine
public
data Step   = Initialised 
            | TaskRunning 
            | TaskCompleted 
            | HeadersWritten 
            | ContentWritten 
            -- Perhaps another after any cleanup?



-- Data type representing an initialised CGI script
public
data InitialisedCGI : Step -> Type where
  ICgi : CGIInfo -> InitialisedCGI s


CGIProg a = Eff IO [CGI (InitialisedCGI TaskRunning)] a



-- In handle, then match on (ICGI s) to get access to state
-- State mutation functions, allowing for addition of headers / output
addHeaders : String -> CGIInfo -> CGIInfo
addHeaders str st = record { Headers = Headers st ++ str } st

addOutput : String -> CGIInfo -> CGIInfo
addOutput str st = record { Output = Output st ++ str } st


-- To create a CGI Program in Network.Cgi, one imports Network.Cgi,
-- and then creates a function CGIInfo -> IO (a, CGIInfo)) which is passed
-- into MkCGI.

-- This produces a complete CGI program, which can then be executed with
-- the required arguments.


-- For the implementation of CGI in the effects language, we'll want users
-- to be able to specify some function to be run inside the CGI effect.

-- This will take the form Eff IO [CGI] a, or more simply
-- CGIEff a.

-- Several CGI functions will also require access to the state, and some will require
-- access to the environment variables.




-- CGI Effect
public
data Cgi : Effect where
  -- Individual functions of the effect
  
  -- Action retrieval
  -- ASKEDWIN: HALP! How to bind implicit variable?
  -- GetAction : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (CGIProg a)

  -- State retrival / update
  -- ASKEDWIN: Do we really need this?
  --SetInfo : CGIInfo -> Cgi (InitialisedCGI Initialised) (InitialisedCGI Ini) ()

  GetInfo : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) CGIInfo
  
  -- Output a string
  OutputData : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Retrieve the GET variables
  GETVars : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Vars

  -- Retrieve the POST variables
  POSTVars : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Vars

  -- Retrieve the cookie variables
  CookieVars : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) Vars

  -- Lookup a variable in the GET variables
  QueryGetVar : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (Maybe String)

  -- Lookup a variable in the POST variables
  QueryPostVar : String -> Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) (Maybe String)

  -- Retrieves the current output
  GetOutput : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) String

  -- Retrieves the headers
  GetHeaders : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) String

  -- Flushes the headers to StdOut
  FlushHeaders : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Flushes output to StdOut
  Flush : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

  -- Initialise the internal CGI State
  Init : Cgi () (InitialisedCGI Initialised) ()

  -- Transition to task started state
  StartRun : Cgi (InitialisedCGI Initialised) (InitialisedCGI TaskRunning) ()

  -- Transition to task completed state
  FinishRun : Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskCompleted) ()

  -- Write headers, transition to headers written state
  WriteHeaders : Cgi (InitialisedCGI TaskCompleted) (InitialisedCGI HeadersWritten) ()

  -- Write content, transition to content written state
  WriteContent : Cgi (InitialisedCGI HeadersWritten) (InitialisedCGI ContentWritten) ()

  -- Add cookie
  -- TODO: Add expiry date in here once I've finished the basics
  SetCookie : String -> String -> {- Date -> -} Cgi (InitialisedCGI TaskRunning) (InitialisedCGI TaskRunning) ()

-- Creation of the concrete effect
CGI t = MkEff t Cgi


getInfo : Eff m [CGI (InitialisedCGI TaskRunning)] CGIInfo
getInfo = GetInfo

abstract
getGETVars : Eff m [CGI (InitialisedCGI TaskRunning)] Vars
getGETVars = GETVars

abstract
getPOSTVars : Eff m [CGI (InitialisedCGI TaskRunning)] Vars
getPOSTVars = POSTVars

abstract
getCookieVars : Eff m [CGI (InitialisedCGI TaskRunning)] Vars
getCookieVars = CookieVars

abstract
queryGetVar : String -> Eff m [CGI (InitialisedCGI TaskRunning)] (Maybe String)
queryGetVar query = (QueryGetVar query)


abstract
queryPostVar : String -> Eff m [CGI (InitialisedCGI TaskRunning)] (Maybe String)
queryPostVar query = (QueryPostVar query)

getOutput : Eff m [CGI (InitialisedCGI TaskRunning)] String
getOutput = GetOutput

getHeaders : Eff m [CGI (InitialisedCGI TaskRunning)] String
getHeaders = GetHeaders

abstract
flushHeaders : Eff m [CGI (InitialisedCGI TaskRunning)] ()
flushHeaders = FlushHeaders

abstract
flush : Eff m [CGI (InitialisedCGI TaskRunning)] ()
flush = Flush

initialise : EffM m [CGI ()] [CGI (InitialisedCGI Initialised)] ()
initialise = Init

startTask : EffM m [CGI (InitialisedCGI Initialised)] [CGI (InitialisedCGI TaskRunning)] ()
startTask = StartRun

finishTask : EffM m [CGI (InitialisedCGI TaskRunning)] [CGI (InitialisedCGI TaskCompleted)] ()
finishTask = FinishRun

writeHeaders : EffM m [CGI (InitialisedCGI TaskCompleted)] [CGI (InitialisedCGI HeadersWritten)] ()
writeHeaders = WriteHeaders

writeContent : EffM m [CGI (InitialisedCGI HeadersWritten)] [CGI (InitialisedCGI ContentWritten)] ()
writeContent = WriteContent

abstract
setCookie : String -> String -> Eff m [CGI (InitialisedCGI TaskRunning)] ()
setCookie name val = (SetCookie name val)

abstract
output : String -> Eff m [CGI (InitialisedCGI TaskRunning)] ()
output s = (OutputData s)


-- Handler for CGI in the IO context
instance Handler Cgi IO where
  --handle (ICgi st) GetInfo k = k (ICgi st) (CgiInfo st)

  handle (ICgi st) GETVars k = k (ICgi st) (GET st)

  handle (ICgi st) POSTVars k = k (ICgi st) (POST st)

  handle (ICgi st) CookieVars k = k (ICgi st) (Cookies st)

  handle (ICgi st) (QueryGetVar q) k = k (ICgi st) (lookup q (GET st))

  handle (ICgi st) (QueryPostVar q) k = k (ICgi st) (lookup q (POST st))

  handle (ICgi st) GetOutput k = k (ICgi st) (Output st)

  handle (ICgi st) GetHeaders k = k (ICgi st) (Headers st)

  -- Should this also clear them from the state? It doesn't in the Network.Cgi imp
  -- but seems logical
  handle (ICgi st) FlushHeaders k = do putStrLn (Headers st)
                                       k (ICgi st) ()

  -- These two are purely to transition states, so we don't need to do anything
  handle (ICgi st) StartRun k = k (ICgi st) ()
  handle (ICgi st) FinishRun k = k (ICgi st) ()

  -- Handle writing out headers and content
  handle (ICgi st) WriteHeaders k = do putStrLn (Headers st)
                                       k (ICgi st) ()
                                       
  -- Handle writing out headers and content
  handle (ICgi st) WriteContent k = do putStrLn (Output st)
                                       k (ICgi st) ()

  handle (ICgi st) Flush k = do putStrLn (Output st)
                                k (ICgi st) ()

  -- Get the variables from the environment
  handle () Init k = do 
    query   <- safeGetEnvVar "QUERY_STRING"
    cookie  <- safeGetEnvVar "HTTP_COOKIE"
    agent   <- safeGetEnvVar "HTTP_USER_AGENT"
    content <- getContent
    let get_vars  = getVars ['&',';'] query
    let post_vars = getVars ['&'] content
    let cookies   = getVars [';'] cookie
            
    let cgi_info = (CGIInf get_vars post_vars cookies agent
                    "Content-type: text/html\n" "")
    k (ICgi cgi_info) ()

  handle (ICgi st) (OutputData s) k  = do let new_cgi_info = addOutput s st
                                          k (ICgi new_cgi_info) ()

-- TODO: don't hardcode date :P #lazyintern
  handle (ICgi st) (SetCookie name val) k = do let set_cookie_header = ("Set-Cookie: " ++ 
                                                name ++ "=" ++ val ++ 
                                                "; Expires=Thu, 26 Jun 2014 10:00:00 GMT")
                                               -- putStrLn $ "Debug: Cookie header: " ++ set_cookie_header
                                               let new_info = addHeaders set_cookie_header st
                                               k (ICgi new_info) ()

-- Internal mechanism to run the user-specified action
private
runCGI' : CGIProg a -> EffM IO [CGI ()] [CGI (InitialisedCGI ContentWritten)] a
runCGI' action = do initialise 
                    -- Transition to TaskRunning
                    startTask
                    -- Perform the user-defined action and collect the result
                    res <- action
                    -- Transition to TaskComplete
                    finishTask
                    -- Write out the headers
                    writeHeaders
                    -- Write out the content
                    writeContent
                    -- Finally, return the result
                    pure res 

public
runCGI : CGIProg a -> IO a
runCGI act = do result <- run [()] (runCGI' act)
                pure result


