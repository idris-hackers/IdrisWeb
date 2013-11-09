module GCrypt
import Effects

%link C "gcrypt_idr.o"
%include C "gcrypt_idr.h"
%lib C "gcrypt"

-- Unimplemented. Eventually, this should encapsulate the flags
-- available to message digest contexts, which this library could
-- OR together.
HashFlag : Type
HashFlag = Int

GCryptResult : Type
GCryptResult = Int

data GCryptStep = GCryptLibInitialised -- Lib initialised
                | GCryptMDInitialised -- Message digest context initialised
                | GCryptMDWritten -- Message digest context has written out the digest

data GCryptRes : Step -> Type where
  LibInitialised : GCryptHashRes s
  -- IOExcept would avoid the need for this...
  LibInitFailed : GCryptHashRes s
  HashContextInitialised : Ptr -> GCryptHashRes s
  HashContextInvalid : GCryptHashRes s


data GCrypt : Effect where
  -- Currently, marshalling a null pointer to a string results in a segfault.
  -- For now, we'll treat this as a bool...
  InitialiseGCrypt : Maybe String -> GCrypt () (GCryptRes GCryptLibInitialised) (Bool) -- (Maybe String)
  InitialiseHashContext : HashAlgorithm -> List HashFlag -> GCrypt (GCryptRes GCryptLibInitialised) 
                                                                   (GCryptRes GCryptMDInitialised) 
                                                                   (Bool)
{- Not really needed.
  FinaliseHashContext : GCrypt (GCryptRes GCryptMDInitialised)
                               (GCryptRes GCryptMDFinalised)
                               ()
-}

  DisposeHashContext : GCrypt (GCryptRes GCryptMDWritten)
                              (GCryptRes GCryptLibInitialised)
                              ()

  ResetHashContext : GCrypt (GCryptRes GCryptMDWritten)
                            (GCryptRes GCryptMDInitialised)
                            ()

  -- Strings only for now. It'd be nice to get the full AoB thing going, but that might take a bit
  -- of FFI rewriting... 
  GetStringMessageDigest : String -> GCrypt (GCryptRes GCryptMDInitialised)
                                            (GCryptRes GCryptMDWritten)
                                            (String)

GCRYPT : Type -> EFFECT
GCRYPT t = MkEff t GCrypt

{- Functions -}
initialiseGCrypt : Maybe String -> EffM IO [GCRYPT ()] [GCRYPT (GCryptRes GCryptLibInitialised)] Bool -- (Maybe String)
initialiseGCrypt ver = (InitialiseGCrypt ver)

-- Initialises a hash context within the library, given an algorithm and a list of flags.
initialiseHashContext : HashAlgorithm -> List HashFlag -> EffM [GCRYPT (GCryptRes GCryptLibInitialised)]
                                                               [GCRYPT (GCryptRes GCryptMDInitialised)]
                                                               (Bool)
initialiseHashContext alg flags = (InitialiseHashContext alg flags)

-- Disposes of a hash context, freeing associated resources
disposeHashContext : EffM IO [GCRYPT (GCryptRes GCryptMDWritten)]
                             [GCRYPT (GCryptRes GCryptLibInitialised)]
                             ()
disposeHashContext = DisposeHashContext

-- Resets a hash context
resetHashContext : EffM IO [GCRYPT (GCryptRes GCryptMDWritten)]
                           [GCRYPT (GCryptRes GCryptMDInitialised)]
                           ()
resetHashContext = ResetHashContext

-- Gets the message digest of a string, using one of the 
-- BIG TODO: Tactic to ensure that the given hash algorithm is available
getStringMessageDigest : String -> EffM IO [GCRYPT (GCryptRes GCryptMDInitialised)]
                                           [GCRYPT (GCryptRes GCryptMDWritten)]
                                           (String)
getStringMessageDigest str = (GetStringMessageDigest str)


cleanupResStruct : Ptr -> IO ()
cleanupResStruct ptr = do
  res <- mkForeign (FFun "idris_gcry_dispose_res" [FPtr] FUnit) ptr
  pure ()

instance Handler GCrypt IO where
  handle () (InitialiseGCrypt (Just ver)) k = do 
    res <- mkForeign (FFun "idris_gcry_init" [FString] FPtr) ver
    null_res <- isNull res
    if null_res then
      k (LibInitFailed) False
    else
      k (LibInitialised) True
    
  handle () (InitialiseGCrypt Nothing) k = do
    res <- mkForeign (FFun "idris_gcry_init" [FString] FPtr) ""
    null_res <- isNull res
    if null_res then
      k (LibInitFailed) False
    else
      k (LibInitialised) True

  handle (LibInitialised) (InitialiseHashContext alg flags) = do
    -- TODO: We're ignoring flags for now
    res <- mkForeign (FFun "idris_gcry_init_md" [FInt, FInt] FPtr) alg 0
    -- Res is a structure demonstrating the result code and a pointer
    -- which may, so long as the call was correct, point to a hashing
    -- context. The intermediate library will not return NULL here.
    err <- mkForeign (FFun "idris_gcry_get_struct_err" [FPtr] FInt) res
    -- Once again, I'm quickly hacking around this. The error value returned
    -- by LibGCrypt consists of two distinct parts: the error code, and the
    -- error location. If, however, the operation was successful, then the
    -- whole thing will be 0.
    -- Expect this interface to change when I do it properly.
    if err == 0 then do
      -- Success
      context <- mkForeign (FFun "idris_gcry_get_struct_data" [FPtr] FPtr) res
      -- Prudent to do a null check in case something's gone wrong
      context_null <- isNull context
      cleanupResStruct res
      if context_null then do
        k (HashContextInvalid) Nothing
      else
        k (HashContextInitialised context) 
    else
      k (HashContextInvalid) Nothing
