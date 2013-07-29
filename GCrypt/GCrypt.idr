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


data GCrypt : Effect where
  InitialiseGCrypt : Maybe String -> GCrypt () (GCryptRes GCryptLibInitialised) (Maybe String)
  InitialiseHashContext : HashAlgorithm -> List HashFlag -> GCrypt (GCryptRes GCryptLibInitialised) 
                                                                   (GCryptRes GCryptMDInitialised) 
                                                                   (GCryptResult)
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
initialiseGCrypt : Maybe String -> EffM IO [GCRYPT ()] [GCRYPT (GCryptRes GCryptLibInitialised)] (Maybe String)
initialiseGCrypt ver = (InitialiseGCrypt ver)

-- Initialises a hash context within the library, given an algorithm and a list of flags.
initialiseHashContext : HashAlgorithm -> List HashFlag -> EffM [GCRYPT (GCryptRes GCryptLibInitialised)]
                                                               [GCRYPT (GCryptRes GCryptMDInitialised)]
                                                               (GCryptResult)
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

