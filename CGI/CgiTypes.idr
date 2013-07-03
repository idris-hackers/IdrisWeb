module IdrisWeb.CGI.CgiTypes
import Effects

%access public
-- Types used by the CGI module

-- Information passed by CGI
public
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
CGIProg : List EFFECT -> Type -> Type

-- States in the state machine
public
data CGIStep   = Initialised 
            | TaskRunning 
            | TaskCompleted 
            | HeadersWritten 
            | ContentWritten 
            -- Perhaps another after any cleanup?



-- Data type representing an initialised CGI script
public
data InitialisedCGI : CGIStep -> Type where
  ICgi : CGIInfo -> InitialisedCGI s


CGIProg effs a = Eff IO (CGI (InitialisedCGI TaskRunning) :: effs) a


