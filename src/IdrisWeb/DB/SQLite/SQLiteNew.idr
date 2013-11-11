module IdrisWeb.DB.SQLite.SQLiteNew
import Effects
import IdrisWeb.DB.SQLite.SQLiteCodes

%link C "sqlite3api.o"
%include C "sqlite3api.h"
%lib C "sqlite3"
%access public

data ConnectionPtr = ConnPtr Ptr
data StmtPtr = PSPtr Ptr


DBName : Type 
DBName = String

QueryString : Type
QueryString = String

Column : Type
Column = Int

ArgPos : Type
ArgPos = Int
data BindError = BE ArgPos SQLiteCode

{- Connection-stage resources -}
data SQLiteConnected : Type where
  SQLConnection : ConnectionPtr -> SQLiteConnected

{- PreparedStatement resources -}
data BindStep = Binding | Bound

data SQLitePSSuccess : BindStep -> Type where
  -- We record potential bind failures within the resource,
  -- and branch on the finishBind step. This prevents us from
  -- having to branch on every bind, which would be impractical.
  SQLitePS : ConnectionPtr -> StmtPtr -> SQLitePSSuccess a
  SQLiteBindFail : ConnectionPtr -> StmtPtr -> BindError -> SQLitePSSuccess a


data SQLitePSFail : Type where
  PSFail : ConnectionPtr -> SQLitePSFail

data SQLiteFinishBindFail : Type where
  SQLiteFBFail : ConnectionPtr -> StmtPtr -> SQLiteFinishBindFail

{- Executing Resources -}
-- Tag used to indicate whether another row may be fetched
data ExecutionResult = ValidRow
                     | InvalidRow

data SQLiteExecuting : ExecutionResult -> Type where
  SQLiteE : ConnectionPtr -> StmtPtr -> SQLiteExecuting a


data Sqlite : Effect where
  -- Opens a connection to the database
  OpenDB : DBName -> Sqlite () (Either () SQLiteConnected) (Either SQLiteCode ())
  -- Closes the database handle
  CloseDB : Sqlite (SQLiteConnected) () ()
  -- Prepares a statement, given a basic query string
  PrepareStatement : QueryString -> Sqlite (SQLiteConnected) (Either (SQLitePSFail) (SQLitePSSuccess Binding))
                                              (Either SQLiteCode ())
  -- Binds arguments to the given argument position
  BindInt : ArgPos -> Int -> Sqlite (SQLitePSSuccess Binding) (SQLitePSSuccess Binding) ()
  BindFloat : ArgPos -> Float -> Sqlite (SQLitePSSuccess Binding) (SQLitePSSuccess Binding) ()
  BindText : ArgPos -> String -> Int -> Sqlite (SQLitePSSuccess Binding) (SQLitePSSuccess Binding) ()
  BindNull : ArgPos -> Sqlite (SQLitePSSuccess Binding) (SQLitePSSuccess Binding) ()
 
  -- Checks to see whether all the binds were successful, if not then fails with the bind error
  FinishBind : Sqlite (SQLitePSSuccess Binding) (Either SQLiteFinishBindFail (SQLitePSSuccess Bound)) 
                                                (Either BindError ())

  -- Executes the statement, and fetches the first row
  ExecuteStatement : Sqlite (SQLitePSSuccess Bound) (Either (SQLiteExecuting InvalidRow)
                                                                (SQLiteExecuting ValidRow)) StepResult

  RowStep : Sqlite (SQLiteExecuting ValidRow) (Either (SQLiteExecuting InvalidRow)
                                                      (SQLiteExecuting ValidRow)) StepResult
  
  -- We need two separate effects, but this is entirely non-user-facing due to
  -- if_valid in the wrapper function
  ResetFromEnd : Sqlite (SQLiteExecuting InvalidRow) 
                        (Either (SQLiteExecuting InvalidRow) 
                        (SQLiteExecuting ValidRow)) StepResult 

  Reset : Sqlite (SQLiteExecuting ValidRow) (Either (SQLiteExecuting InvalidRow) 
                                                    (SQLiteExecuting ValidRow)) StepResult
  
  -- Column access functions
  GetColumnName : Column -> Sqlite (SQLiteExecuting ValidRow) (SQLiteExecuting ValidRow) String
  GetColumnDataSize : Column -> Sqlite (SQLiteExecuting ValidRow) (SQLiteExecuting ValidRow) Int
  GetColumnText : Column -> Sqlite (SQLiteExecuting ValidRow) (SQLiteExecuting ValidRow) String
  GetColumnInt : Column -> Sqlite (SQLiteExecuting ValidRow) (SQLiteExecuting ValidRow) Int
  
  -- Finalisation Functions
  FinaliseValid : Sqlite (SQLiteExecuting ValidRow) (SQLiteConnected) ()
  FinaliseInvalid : Sqlite (SQLiteExecuting InvalidRow) (SQLiteConnected) ()

  -- Cleanup functions to handle error states
  CleanupPSFail : Sqlite (SQLitePSFail) () ()
  CleanupBindFail : Sqlite (SQLiteFinishBindFail) () ()

private
foreignGetError : ConnectionPtr -> IO Int
foreignGetError (ConnPtr ptr) = mkForeign (FFun "idr_errcode" [FPtr] FInt) ptr

private
foreignNextRow : ConnectionPtr -> IO StepResult
foreignNextRow (ConnPtr ptr) = 
  map stepResult (mkForeign (FFun "sqlite3_step_idr" [FPtr] FInt) ptr)

private
foreignFinalise : ConnectionPtr -> IO ()
foreignFinalise (ConnPtr c) = do mkForeign (FFun "sqlite3_finalize_idr" [FPtr] FInt) c
                                 return ()

private
foreignClose : ConnectionPtr -> IO ()
foreignClose (ConnPtr c) = do mkForeign (FFun "sqlite3_close_idr" [FPtr] FInt) c
                              return ()
-- That's the painful bit done, since exception branching will allow us to not have to do
-- the ugliness of pass-through handlers
instance Handler Sqlite IO where
  handle () (OpenDB file) k = do
    ff <- mkForeign (FFun "sqlite3_open_idr" [FString] FPtr) file
    is_null <- nullPtr ff
    if (not is_null) then k (Right (SQLConnection (ConnPtr ff))) (Right ())
                     else k (Left ()) (Left sqlite_ERROR)

  handle (SQLConnection (ConnPtr conn) ) CloseDB k = do
    mkForeign (FFun "sqlite3_close_idr" [FPtr] FInt) conn
    k () ()

  handle (SQLConnection (ConnPtr conn)) (PrepareStatement str) k = do
    res <- mkForeign (FFun "sqlite3_prepare_idr" [FPtr, FString] FPtr) conn str
    is_null <- nullPtr res
    if (not is_null) then k (Right (SQLitePS (ConnPtr conn) (PSPtr res))) (Right ())
                     else do err <- foreignGetError (ConnPtr conn)
                             k (Left (PSFail (ConnPtr conn))) (Left err)
 
  handle (SQLitePS (ConnPtr conn) (PSPtr res)) (BindInt pos i) k = do
    res <- mkForeign (FFun "sqlite3_bind_int_idr" [FPtr, FInt, FInt] FPtr) conn pos i
    is_null <- nullPtr res
    if (not is_null) then k (SQLitePS (ConnPtr conn) (PSPtr res)) ()
                     else do err <- foreignGetError (ConnPtr conn)
                             putStrLn $ "BindInt error: " ++ (show err)
                             k (SQLiteBindFail (ConnPtr conn) (PSPtr res) (BE pos err)) ()
 
  handle (SQLitePS (ConnPtr conn) (PSPtr res)) (BindFloat pos f) k = do
    res <- mkForeign (FFun "sqlite3_bind_float_idr" [FPtr, FInt, FFloat] FPtr) conn pos f
    is_null <- nullPtr res
    if (not is_null) then k (SQLitePS (ConnPtr conn) (PSPtr res)) ()
                     else do err <- foreignGetError (ConnPtr conn)
                             k (SQLiteBindFail (ConnPtr conn) (PSPtr res) (BE pos err)) ()

  handle (SQLitePS (ConnPtr conn) (PSPtr res)) (BindText pos str str_len) k = do
    res <- mkForeign (FFun "sqlite3_bind_text_idr" [FPtr, FString, FInt, FInt] FPtr) conn str pos str_len
    is_null <- nullPtr res
    if (not is_null) then k (SQLitePS (ConnPtr conn) (PSPtr res)) ()
                     else do err <- foreignGetError (ConnPtr conn)
                             putStrLn $ "BindStr error: " ++ (show err)
                             k (SQLiteBindFail (ConnPtr conn) (PSPtr res) (BE pos err)) ()

  handle (SQLitePS (ConnPtr conn) (PSPtr res)) (BindNull pos) k = do
    res <- mkForeign (FFun "sqlite3_bind_null_idr" [FPtr, FInt] FPtr) conn pos
    is_null <- nullPtr res
    if (not is_null) then k (SQLitePS (ConnPtr conn) (PSPtr res)) ()
                     else do err <- foreignGetError (ConnPtr conn)
                             k (SQLiteBindFail (ConnPtr conn) (PSPtr res) (BE pos err)) ()

  -- Ok, I lied, we have to do *some* pass-throughs. But they're not terrible.
  handle (SQLiteBindFail conn ps be) (BindInt _ _) k = k (SQLiteBindFail conn ps be) ()
  handle (SQLiteBindFail conn ps be) (BindText _ _ _) k = k (SQLiteBindFail conn ps be) ()
  handle (SQLiteBindFail conn ps be) (BindFloat _ _) k = k (SQLiteBindFail conn ps be) ()
  handle (SQLiteBindFail conn ps be) (BindNull _) k = k (SQLiteBindFail conn ps be) ()

  
  -- Finishing binding, reporting any bind errors if they occurred
  handle (SQLitePS c p) (FinishBind) k = 
    k (Right (SQLitePS c p)) (Right ())

  handle (SQLiteBindFail c ps be) (FinishBind) k = 
    k (Left (SQLiteFBFail c ps)) (Left be)

  handle (SQLitePS (ConnPtr c) (PSPtr p)) (ExecuteStatement) k = do
    step <- foreignNextRow (ConnPtr c)
    case step of
      StepComplete => k (Right (SQLiteE (ConnPtr c) (PSPtr p))) step
      StepFail => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step
      NoMoreRows => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step

  handle (SQLiteE (ConnPtr c) (PSPtr p)) (RowStep) k = do
    step <- foreignNextRow (ConnPtr c)
    case step of
      StepComplete => k (Right (SQLiteE (ConnPtr c) (PSPtr p))) step
      StepFail => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step
      NoMoreRows => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step


  -- Getting values from the current row
  handle (SQLiteE (ConnPtr c) (PSPtr p)) (GetColumnName i) k = do
    res <- mkForeign (FFun "sqlite3_column_name_idr" [FPtr, FInt] FString) c i
    k (SQLiteE (ConnPtr c) (PSPtr p)) res

  handle (SQLiteE (ConnPtr c) (PSPtr p)) (GetColumnDataSize i) k = do
    res <- mkForeign (FFun "sqlite3_column_bytes_idr" [FPtr, FInt] FInt) c i
    k (SQLiteE (ConnPtr c) (PSPtr p)) res

  handle (SQLiteE (ConnPtr c) (PSPtr p)) (GetColumnInt i) k = do
    res <- mkForeign (FFun "sqlite3_column_int_idr" [FPtr, FInt] FInt) c i
    k (SQLiteE (ConnPtr c) (PSPtr p)) res

  handle (SQLiteE (ConnPtr c) (PSPtr p)) (GetColumnText i) k = do
    res <- mkForeign (FFun "sqlite3_column_text_idr" [FPtr, FInt] FString) c i
    k (SQLiteE (ConnPtr c) (PSPtr p)) res

  -- Resetting our position
  handle (SQLiteE (ConnPtr c) (PSPtr p)) (Reset) k = do
    mkForeign (FFun "sqlite3_reset_idr" [FPtr] FInt) c
    step <- foreignNextRow (ConnPtr c)
    case step of
      StepComplete => k (Right (SQLiteE (ConnPtr c) (PSPtr p))) step
      StepFail => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step
      NoMoreRows => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step
 
  handle (SQLiteE (ConnPtr c) (PSPtr p)) (ResetFromEnd) k = do
    mkForeign (FFun "sqlite3_reset_idr" [FPtr] FInt) c
    step <- foreignNextRow (ConnPtr c)
    case step of
      StepComplete => k (Right (SQLiteE (ConnPtr c) (PSPtr p))) step
      StepFail => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step
      NoMoreRows => k (Left (SQLiteE (ConnPtr c) (PSPtr p))) step

      -- Finalising the SQL Statement
  handle (SQLiteE c p) (FinaliseValid) k = do
    foreignFinalise c
    k (SQLConnection c) ()

  handle (SQLiteE c p) (FinaliseInvalid) k = do
    foreignFinalise c
    k (SQLConnection c) ()

  handle (PSFail c) CleanupPSFail k = do
    foreignClose c
    k () ()

  handle (SQLiteFBFail c p) CleanupBindFail k = do
    foreignFinalise c
    foreignClose c
    k () ()
    

SQLITE : Type -> EFFECT
SQLITE t = MkEff t Sqlite 
{- User-facing functions -}
openDB : DBName -> EffM IO [SQLITE ()] [SQLITE (Either () SQLiteConnected)]
                                       (Either SQLiteCode ())
openDB name = (OpenDB name)

closeDB : EffM IO [SQLITE (SQLiteConnected)] [SQLITE ()] ()
closeDB = CloseDB

prepareStatement : QueryString -> EffM IO [SQLITE SQLiteConnected] 
                                          [SQLITE (Either SQLitePSFail 
                                                  (SQLitePSSuccess Binding))]
                                          (Either SQLiteCode ())
prepareStatement stmt = (PrepareStatement stmt)

bindInt : ArgPos -> Int -> Eff IO [SQLITE (SQLitePSSuccess Binding)] ()
bindInt pos i = (BindInt pos i)

bindFloat : ArgPos -> Float -> Eff IO [SQLITE (SQLitePSSuccess Binding)] ()
bindFloat pos f = (BindFloat pos f)

bindText : ArgPos -> String -> Eff IO [SQLITE (SQLitePSSuccess Binding)] ()
bindText pos str = (BindText pos str str_len)
  where natToInt : Nat -> Int
        natToInt Z = 0
        natToInt (S k) = 1 + (natToInt k)

        str_len : Int
        str_len = natToInt (length str)

bindNull : ArgPos -> Eff IO [SQLITE (SQLitePSSuccess Binding)] ()
bindNull pos = (BindNull pos)

finishBind : EffM IO [SQLITE (SQLitePSSuccess Binding)]
                     [SQLITE (Either SQLiteFinishBindFail (SQLitePSSuccess Bound))]
                     (Either BindError ())
finishBind = FinishBind

nextRow : EffM IO [SQLITE (SQLiteExecuting ValidRow)] 
                  [SQLITE (Either (SQLiteExecuting InvalidRow)
                                  (SQLiteExecuting ValidRow))] StepResult
nextRow = RowStep

reset : EffM IO [SQLITE (Either (SQLiteExecuting InvalidRow) (SQLiteExecuting ValidRow))]
                [SQLITE (Either (SQLiteExecuting InvalidRow)
                                (SQLiteExecuting ValidRow))] StepResult
reset = if_left then ResetFromEnd else Reset


getColumnName : Column -> Eff IO [SQLITE (SQLiteExecuting ValidRow)] String
getColumnName col = (GetColumnName col)

getColumnText: Column -> Eff IO [SQLITE (SQLiteExecuting ValidRow)] String
getColumnText col = (GetColumnText col)

getColumnInt : Column -> Eff IO [SQLITE (SQLiteExecuting ValidRow)] Int
getColumnInt col = (GetColumnInt col)

getColumnDataSize : Column -> Eff IO [SQLITE (SQLiteExecuting ValidRow)] Int
getColumnDataSize col = (GetColumnDataSize col)

finaliseValid : EffM IO [SQLITE (SQLiteExecuting ValidRow)] [SQLITE (SQLiteConnected)] ()
finaliseValid = FinaliseValid

finaliseInvalid : EffM IO [SQLITE (SQLiteExecuting InvalidRow)] [SQLITE (SQLiteConnected)] ()
finaliseInvalid = FinaliseInvalid

--isOne : (a : Type) -> Either a b

finalise : EffM IO [SQLITE (Either (SQLiteExecuting InvalidRow) (SQLiteExecuting ValidRow))]
                   [SQLITE (SQLiteConnected)] ()
finalise = if_valid then finaliseValid else finaliseInvalid

cleanupPSFail : EffM IO [SQLITE (SQLitePSFail)] [SQLITE ()] ()
cleanupPSFail = CleanupPSFail

cleanupBindFail : EffM IO [SQLITE (SQLiteFinishBindFail)] [SQLITE ()] ()
cleanupBindFail = CleanupBindFail

-- Just makes it a tad nicer to write
executeStatement : EffM IO [SQLITE (SQLitePSSuccess Bound)]
                        [SQLITE (Either (SQLiteExecuting InvalidRow)
                                        (SQLiteExecuting ValidRow))] StepResult
executeStatement = ExecuteStatement



-- Helper function for when there's no binding needed to the PS
-- noBinds : EffM IO [SQLITE (
