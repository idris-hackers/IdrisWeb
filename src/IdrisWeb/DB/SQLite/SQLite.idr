--Effect implementation of the SQLite3 Library.
-- Major credits go to SQLite3 library authors.
module IdrisWeb.DB.SQLite.SQLite
--import Sqlexpr
import Effects
import IdrisWeb.DB.SQLite.SQLiteCodes

%link C "sqlite3api.o"
%include C "sqlite3api.h"
%lib C "sqlite3"

%access public

-- Pointer to open database
data DBPointer = ValidConn Ptr
               | InvalidConn

-- Pointer to open prepared statement
data StmtPointer = ValidStmt Ptr
                 | InvalidStmt 

-- Surely these can be consolidated into one?
-- Also, I should ideally fix the imports to stop duplicating these defs.
data DBVal = DBInt Int
           | DBText String
           | DBFloat Float
           | DBNull


-- Type synonym for a table
ResultSet : Type
ResultSet = List (List DBVal)

ArgPos : Type
ArgPos = Int


data Step = ConnectionOpened
          | PreparedStatementOpen
          | PreparedStatementBinding
          | PreparedStatementBound
          | PreparedStatementExecuting
--          | PreparedStatementResultsFetched



data SQLiteRes : Step -> Type where
  OpenConn : DBPointer -> SQLiteRes s
  OpenStmt : DBPointer -> StmtPointer -> SQLiteRes s
  ExecutingStmt : DBPointer -> StmtPointer -> StepResult -> SQLiteRes s

data Sqlite : Effect where
  -- Opens a connection to the database
  OpenDB : String -> Sqlite () (SQLiteRes ConnectionOpened) Bool
  -- Closes the database handle
  CloseDB : Sqlite (SQLiteRes ConnectionOpened) () Bool
  -- Creates a new prepared statement given a basic query string
  PrepareStatement : String -> Sqlite (SQLiteRes ConnectionOpened) 
                                      (SQLiteRes PreparedStatementOpen) Bool

  -- Transition to the binding state, allowing for variables to be bound to the statement
  StartBind : Sqlite (SQLiteRes PreparedStatementOpen) (SQLiteRes PreparedStatementBinding) ()

  -- Binds an integer to the given argument
  BindInt : ArgPos -> Int -> Sqlite (SQLiteRes PreparedStatementBinding) 
                                    (SQLiteRes PreparedStatementBinding) Bool

  -- Binds a float to the given argument
  BindFloat : ArgPos -> Float -> Sqlite (SQLiteRes PreparedStatementBinding) 
                                        (SQLiteRes PreparedStatementBinding) Bool 

  -- Binds a string to the given argument
  BindText : ArgPos -> (text : String) -> 
                       (length : Int) -> 
                       Sqlite (SQLiteRes PreparedStatementBinding) 
                              (SQLiteRes PreparedStatementBinding) Bool

  -- Binds a NULL value to the given argument
  BindNull : ArgPos -> Sqlite (SQLiteRes PreparedStatementBinding) 
                              (SQLiteRes PreparedStatementBinding) Bool

{- Handle outside of IO, by calling appropriate effect functions
  -- Binds multiple values at once
  BindMulti : List (ArgPos, Value) -> 
              Sqlite (SQLiteRes PreparedStatementBinding) 
                     (SQLiteRes PreparedStatementBinding) Bool
                     -}

  -- Transitions out of the binding state, allowing for the query to be executed
  FinishBind : Sqlite (SQLiteRes PreparedStatementBinding) (SQLiteRes PreparedStatementBound) Bool

  -- Executes the given prepared statement
  ExecuteStatement : Sqlite (SQLiteRes PreparedStatementBound) (SQLiteRes PreparedStatementExecuting) ()

  -- Fetches the results of the previously-executed query
  {-FetchResults : Sqlite (SQLiteRes PreparedStatementExecuting) 
                        (SQLiteRes PreparedStatementResultsFetched) 
                        Table
                        -}
  -- Disposes of a prepared statement
  Finalise : Sqlite (SQLiteRes PreparedStatementExecuting) (SQLiteRes ConnectionOpened) Bool


  ----- Operations on result sets.
  -- SQLite returns data on a row-by-row basis, which is advanced using the step
  -- function. The position of this pointer can also be reset using the reset function.

  -- These operations are done performed on the current row.
  -- Given a column index, gets the name of the column
  GetColumnName : Int -> Sqlite (SQLiteRes PreparedStatementExecuting) 
                                (SQLiteRes PreparedStatementExecuting)
                                String


  -- Given a column index, returns the size in bytes
  GetColumnDataSize : Int -> Sqlite (SQLiteRes PreparedStatementExecuting)
                                    (SQLiteRes PreparedStatementExecuting)
                                    Int


  -- Not sure how we should handle blobs. Underlying library says string, but I don't
  -- think this entirely makes sense. Will come back to it, omitting for now.

  -- Given a column index, returns the data as an Idris string.
  GetColumnText : Int -> Sqlite (SQLiteRes PreparedStatementExecuting)
                                (SQLiteRes PreparedStatementExecuting)
                                String

  -- Given a column index, returns the data as an Idris integer.
  GetColumnInt : Int -> Sqlite (SQLiteRes PreparedStatementExecuting)
                               (SQLiteRes PreparedStatementExecuting)
                               Int

  -- Advance the row pointer
  -- TODO: We will get a particular status code when we reach the end of the
  --       results. Ideally, this should be encoded as a state to ensure that
  --       step is not called once all rows have been seen.
  RowStep : Sqlite (SQLiteRes PreparedStatementExecuting)
                   (SQLiteRes PreparedStatementExecuting)
                   StepResult

  Reset : Sqlite (SQLiteRes PreparedStatementExecuting)
                 (SQLiteRes PreparedStatementExecuting)
                 Bool



-- TODO: Must also do this in IOExcept, runtime errors are bad, mmkay
instance Handler Sqlite IO where
  handle () (OpenDB file) k = do
    ff <- mkForeign (FFun "sqlite3_open_idr" [FString] FPtr) file
    is_null <- nullPtr ff
    -- We still have to transition to ConnectionOpened, even if the
    -- open operation failed. We can, however, tag the connection resource
    -- as invalid, and pattern match on it so that no further effects occur.
    if (not is_null) then k (OpenConn (ValidConn ff)) True
                     else k (OpenConn InvalidConn) False
    -- k (Valid ff) ()

  -- TODO: Probably best to do some error handling here
  handle (OpenConn (ValidConn conn) ) CloseDB k = do
    mkForeign (FFun "sqlite3_close_idr" [FPtr] FInt) conn
    k () True

  -- If the handle is invalid, just return true without doing anything
  handle (OpenConn (InvalidConn)) CloseDB k = k () True

  -- Compile a prepared statement.
  -- If there's no connection, do nothing and return false, with an invalid statement
  handle (OpenConn (InvalidConn)) (PrepareStatement s) k = k (OpenStmt InvalidConn InvalidStmt) False

  -- Otherwise, try to create a prepared statement
  handle (OpenConn (ValidConn c)) (PrepareStatement s) k = do
    ps_ptr <-  mkForeign (FFun "sqlite3_prepare_idr" [FPtr, FString] FPtr) c s
    is_null <- nullPtr ps_ptr
    if (not is_null) then k (OpenStmt (ValidConn c) (ValidStmt ps_ptr)) True
                     else k (OpenStmt (ValidConn c) (InvalidStmt)) False

  -- Execute a prepared statement. 
  handle (OpenStmt (ValidConn c) (ValidStmt s)) ExecuteStatement k = 
    k (ExecutingStmt (ValidConn c) (ValidStmt s) Unstarted) ()
    {-
    Looks like it doesn't work how I thought it did. The best thing to do
      right now, I think, is to just separate the finished binding stage
      with a 'currently executing' stage. 

    I might remove this stage later though.
    do
    x <- mkForeign (FFun "exec_db" [FPtr] FInt) s
    -- Ideally, an execution failure should debar us from attempting 
    -- to retrieve the results. 
    -- Currently, we can do this, and the call would still go through to 
    -- the library.
    -- TODO: Fix this, possibly by having another type of StmtPointer which
    -- signifies that the results have been tainted by some failure,
    -- but that the pointer is still active and needs to be freed.
    putStrLn ("SQLite status code: " ++ show x)
    if (x == sqlite_OK) then k (OpenStmt (ValidConn c) (ValidStmt s)) True
                        else k (OpenStmt (ValidConn c) (ValidStmt s)) False
                        -}

  handle (OpenStmt (ValidConn c) InvalidStmt) ExecuteStatement k = 
    k (ExecutingStmt (ValidConn c) InvalidStmt StepFail) ()

  handle (OpenStmt InvalidConn x) ExecuteStatement k = 
    k (ExecutingStmt InvalidConn x StepFail) () --ExecuteStatement

  -- Bind state transition functions
  -- TODO: error checking here? should this return true / false (or some 
  --       more expressive error type at that
  handle (OpenStmt (ValidConn c) (ValidStmt s)) StartBind k = do
    k (OpenStmt (ValidConn c) (ValidStmt s)) ()

  handle (OpenStmt (ValidConn c) InvalidStmt) StartBind k = do
    k (OpenStmt (ValidConn c) InvalidStmt) ()

  handle (OpenStmt InvalidConn x) StartBind k = k (OpenStmt InvalidConn x) ()

  handle (OpenStmt (ValidConn c) (ValidStmt s)) FinishBind k = do
    k (OpenStmt (ValidConn c) (ValidStmt s)) True

  handle (OpenStmt (ValidConn c) InvalidStmt) FinishBind k = do
    k (OpenStmt (ValidConn c) InvalidStmt) False

  handle (OpenStmt InvalidConn x) FinishBind k = k (OpenStmt InvalidConn x) False


  -- Bind functions
  handle (OpenStmt (ValidConn c) (ValidStmt s)) (BindInt pos i) k = do
    res <- mkForeign (FFun "sqlite3_bind_int_idr" [FPtr, FInt, FInt] FPtr) c pos i
    is_null <- nullPtr res
    if (not is_null) then k (OpenStmt (ValidConn c) (ValidStmt s)) True
                     else k (OpenStmt (ValidConn c) (ValidStmt s)) False

  handle (OpenStmt (ValidConn c) (ValidStmt s)) (BindFloat pos f) k = do
    res <- mkForeign (FFun "sqlite3_bind_float_idr" [FPtr, FInt, FFloat] FPtr) c pos f
    is_null <- nullPtr res
    if (not is_null) then k (OpenStmt (ValidConn c) (ValidStmt s)) True
                     else k (OpenStmt (ValidConn c) (ValidStmt s)) False


  handle (OpenStmt (ValidConn c) (ValidStmt s)) (BindText pos str len) k = do
    res <- mkForeign (FFun "sqlite3_bind_text_idr" [FPtr, FString, FInt, FInt] FPtr) c str pos len
    is_null <- nullPtr res
    if (not is_null) then k (OpenStmt (ValidConn c) (ValidStmt s)) True
                     else k (OpenStmt (ValidConn c) (ValidStmt s)) False

  handle (OpenStmt (ValidConn c) (ValidStmt s)) (BindNull pos) k = do
    res <- mkForeign (FFun "sqlite3_bind_null_idr" [FPtr, FInt] FPtr) c pos
    is_null <- nullPtr res
    if (not is_null) then k (OpenStmt (ValidConn c) (ValidStmt s)) True
                     else k (OpenStmt (ValidConn c) (ValidStmt s)) False

  -- Binds called with invalid args
  handle (OpenStmt InvalidConn x) (BindInt _ _) k = k (OpenStmt InvalidConn x) False
  handle (OpenStmt (ValidConn c) InvalidStmt) (BindInt _ _) k = 
    k (OpenStmt (ValidConn c) InvalidStmt) False

  handle (OpenStmt InvalidConn x) (BindFloat _ _) k = k (OpenStmt InvalidConn x) False
  handle (OpenStmt (ValidConn c) InvalidStmt) (BindFloat _ _) k = 
    k (OpenStmt (ValidConn c) InvalidStmt) False

  handle (OpenStmt InvalidConn x) (BindText _ _ _) k = k (OpenStmt InvalidConn x) False
  handle (OpenStmt (ValidConn c) InvalidStmt) (BindText _ _ _) k = 
    k (OpenStmt (ValidConn c) InvalidStmt) False

  handle (OpenStmt InvalidConn x) (BindNull _) k = k (OpenStmt InvalidConn x) False
  handle (OpenStmt (ValidConn c) InvalidStmt) (BindNull _) k = 
    k (OpenStmt (ValidConn c) InvalidStmt) False

   
  -- Row operations
  -- These only pass the calls to the underlying library if the ExecutingStmt
  -- is tagged with StepComplete. This means that the resource access protocol
  -- is adhered to.
  handle (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) (GetColumnName i) k = do
    res <- mkForeign (FFun "sqlite3_column_name_idr" [FPtr, FInt] FString) c i 
    k (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) res

  handle (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) (GetColumnDataSize i) k = do
    res <- mkForeign (FFun "sqlite3_column_bytes_idr" [FPtr, FInt] FInt) c i
    k (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) res

  handle (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) (GetColumnInt i) k = do
    res <- mkForeign (FFun "sqlite3_column_int_idr" [FPtr, FInt] FInt) c i
    k (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) res

  handle (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) (GetColumnText i) k = do
    -- This is horrifically hacky in every single way. Fix up!
    res <- mkForeign (FFun "sqlite3_column_text_idr" [FPtr, FInt] FPtr) c i 
    -- FIXME: Make this into a maybe
    is_null <- nullPtr res
    if is_null then do --putStrLn "null ptr in gtc"
                       k (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) ""
               else do res' <- mkForeign (FFun "sqlite3_column_text_idr" [FPtr, FInt] FString) c i
                       k (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) res'

  -- Pass-through handlers
  -- Urgh, perhaps best to encapsulate these failures in a maybe?
  -- In fact, we *really* should. Otherwise these values may be used in further
  -- computations in some circumstances.

  handle (ExecutingStmt x y z) (GetColumnName i) k = 
    k (ExecutingStmt x y z) ""

  handle (ExecutingStmt x y z) (GetColumnDataSize i) k = 
    k (ExecutingStmt x y z) (-1)

  handle (ExecutingStmt x y z) (GetColumnText i) k = 
    k (ExecutingStmt x y z) ""

  handle (ExecutingStmt x y z) (GetColumnInt i) k = 
    k (ExecutingStmt x y z) (-1)

  -- Step and reset
  -- Only actually call the underlying library if in either the Unstarted / StepComplete
  -- states. Calling this in other states should fail without calling the library.
  handle (ExecutingStmt (ValidConn c) (ValidStmt s) Unstarted) RowStep k = do
    res <- mkForeign (FFun "sqlite3_step_idr" [FPtr] FInt) c
{-    -- hacky log
    file <- openFile "/tmp/rowstep_log.log" Write
    fwrite file (show res) 
    closeFile file -}
    --putStrLn $ "Res: " ++ (show res)
    let step_res = stepResult res
    k (ExecutingStmt (ValidConn c) (ValidStmt s) step_res) step_res

  handle (ExecutingStmt (ValidConn c) (ValidStmt s) StepComplete) RowStep k = do
    res <- mkForeign (FFun "sqlite3_step_idr" [FPtr] FInt) c
    --putStrLn $ "Res: " ++ (show res)
    let step_res = stepResult res
    k (ExecutingStmt (ValidConn c) (ValidStmt s) step_res) step_res

  handle (ExecutingStmt (ValidConn c) (ValidStmt s) x) RowStep k = do
    k (ExecutingStmt (ValidConn c) (ValidStmt s) x) StepFail

  handle (ExecutingStmt x y z) RowStep k = 
    k (ExecutingStmt x y z) StepFail

  handle (ExecutingStmt (ValidConn c) (ValidStmt s) x) Reset k = do
    res <- mkForeign (FFun "sqlite3_reset_idr" [FPtr] FInt) c
    k (ExecutingStmt (ValidConn c) (ValidStmt s) Unstarted) (res == sqlite_OK)

  handle (ExecutingStmt (ValidConn c) InvalidStmt x) Reset k = 
    k (ExecutingStmt (ValidConn c) InvalidStmt StepFail) False
  handle (ExecutingStmt InvalidConn x y) Reset k = k (ExecutingStmt InvalidConn x StepFail) False

  -- Finalise statement
  handle (ExecutingStmt (ValidConn c) (ValidStmt s) x) Finalise k = do
    res <- mkForeign (FFun "sqlite3_finalize_idr" [FPtr] FInt) s
    k (OpenConn (ValidConn c)) (res == sqlite_OK)

  handle (ExecutingStmt (ValidConn c) InvalidStmt x) Finalise k = 
    k (OpenConn (ValidConn c)) False

  handle (ExecutingStmt InvalidConn x y) Finalise k = k (OpenConn InvalidConn) False

SQLITE : Type -> EFFECT
SQLITE t = MkEff t Sqlite

-- Effect functions
openDB : String -> EffM m [SQLITE ()] [SQLITE (SQLiteRes ConnectionOpened)] Bool
openDB filename = (OpenDB filename)

closeDB : EffM m [SQLITE (SQLiteRes ConnectionOpened)] [SQLITE ()] Bool
closeDB = CloseDB 

prepareStatement : String -> EffM m [SQLITE (SQLiteRes ConnectionOpened)] 
                                    [SQLITE (SQLiteRes PreparedStatementOpen)] Bool
prepareStatement stmt = (PrepareStatement stmt)

startBind : EffM m [SQLITE (SQLiteRes PreparedStatementOpen)] 
                   [SQLITE (SQLiteRes PreparedStatementBinding)] ()
startBind = StartBind

bindInt : ArgPos -> Int -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] Bool
bindInt pos i = (BindInt pos i)

bindFloat : ArgPos -> Float -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] Bool
bindFloat pos i = (BindFloat pos i)

natToInt : Nat -> Int
natToInt O = 0
natToInt (S k) = 1 + (natToInt k)

bindText : ArgPos -> String -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] Bool
bindText pos str = (BindText pos str str_len)
  where 
        str_len : Int
        str_len = natToInt (length str)

bindNull : ArgPos -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] Bool
bindNull pos = (BindNull pos)

finishBind : EffM m [SQLITE (SQLiteRes PreparedStatementBinding)] 
                    [SQLITE (SQLiteRes PreparedStatementBound)] Bool
finishBind = FinishBind

beginExecution : EffM m [SQLITE (SQLiteRes PreparedStatementBound)] 
                        [SQLITE (SQLiteRes PreparedStatementExecuting)] ()
beginExecution = ExecuteStatement

finaliseStatement : EffM m [SQLITE (SQLiteRes PreparedStatementExecuting)]
                           [SQLITE (SQLiteRes ConnectionOpened)]
                           Bool
finaliseStatement = Finalise

getColumnName : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] String
getColumnName pos = (GetColumnName pos)

getColumnDataSize : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] Int
getColumnDataSize pos = (GetColumnDataSize pos)

getColumnText : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] String
getColumnText pos = (GetColumnText pos)

getColumnInt : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] Int
getColumnInt pos = (GetColumnInt pos)

nextRow : Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] StepResult
nextRow = RowStep
resetPos : Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] Bool
resetPos = Reset

-- Utility functions to help handle failure
-- TODO: These errors should ideally be encoded as ADTs
connFail : EffM m [SQLITE (SQLiteRes ConnectionOpened)] [SQLITE ()] String
connFail = do closeDB
              pure "Error connecting to database."

stmtFail : EffM m [SQLITE (SQLiteRes PreparedStatementOpen)] [SQLITE ()] String
stmtFail = do
  -- These will all fall through without having any effect.
  -- Right Way To Do It?
  startBind
  finishBind
  beginExecution
  finaliseStatement
  closeDB -- Close the connection
  pure "Error preparing statement"

bindFail : EffM m [SQLITE (SQLiteRes PreparedStatementBound)] [SQLITE ()] String
bindFail = do
  beginExecution
  finaliseStatement
  closeDB
  pure "Error binding to statement"

executeFail : EffM m [SQLITE (SQLiteRes PreparedStatementExecuting)] [SQLITE ()] String
executeFail = do
  finaliseStatement
  closeDB
  pure "Error executing statement"


multiBind' : List (Int, DBVal) -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] ()
multiBind' [] = Effects.pure ()
multiBind' ((pos, (DBInt i)) :: xs) = do bindInt pos i
                                         multiBind' xs
multiBind' ((pos, (DBFloat f)) :: xs) = do bindFloat pos f
                                           multiBind' xs
multiBind' ((pos, (DBText t)) :: xs) = do bindText pos t
                                          multiBind' xs
-- Binds multiple values within a query
multiBind : List (Int, DBVal) -> EffM m [SQLITE (SQLiteRes PreparedStatementOpen)] [SQLITE (SQLiteRes PreparedStatementBound)] Bool
multiBind vals = do
  startBind
  multiBind' vals
  finishBind


executeInsert' : EffM m [SQLITE (SQLiteRes PreparedStatementExecuting)] [SQLITE ()] (Either String Int) -- (Maybe Int)
executeInsert' = do
 id_res <- nextRow 
 case id_res of
   StepComplete => do
     last_insert_id <- getColumnInt 0
     finaliseStatement
     closeDB
     Effects.pure $ Right last_insert_id
   NoMoreRows => do finaliseStatement
                    closeDB
                    Effects.pure $ Left "Error getting insert ID! NoMoreRows"
   StepFail => do finaliseStatement
                  closeDB
                  Effects.pure $ Left "Error getting insert ID! StepFail"

-- Execute an insert statement, get the last inserted row ID
-- TODO: Create a binding to the SQLITE API function, instead of getting
--       last row ID by a query
executeInsert : String -> 
                String -> 
                List (Int, DBVal) -> 
                Eff IO [SQLITE ()] (Either String Int)
executeInsert db_name query bind_vals = do
  db_conn <- openDB db_name
  if db_conn then do
    prep_res <- prepareStatement query
    if prep_res then do 
      bind_res <- multiBind bind_vals
      if bind_res then do
        beginExecution
        next_row_res <- nextRow
        case next_row_res of
      -- Error with executing insert statement
          StepFail => do
            finaliseStatement
            closeDB
            Effects.pure $ Left "Error inserting! next_row_res executeFail"
          _ => do
            finaliseStatement
            let insert_id_sql = "SELECT last_insert_rowid();"
            sql_prep_res <- prepareStatement insert_id_sql
            if sql_prep_res then do
              startBind
              finishBind
              beginExecution 
              executeInsert'
            else do
              err <- stmtFail
              Effects.pure $ Left err
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err
    
collectResults : (Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] (List DBVal)) ->
                 Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] (Either String ResultSet)
collectResults fn = do
  step_result <- nextRow
  case step_result of
    StepComplete => do results <- fn
                       xs <- collectResults fn
                       Effects.pure $ [| (Prelude.List.(::)) (Right results) xs |]
                       {-case xs of
                         Left err => pure err
                         Right xs' => pure $ results :: xs'-}
    NoMoreRows => Effects.pure $ Right []
    StepFail => Effects.pure $ Left "StepFail encountered"

-- Convenience function to abstract around some of the boilerplate code.
-- Takes in the DB name, query, a list of (position, variable value) tuples,
-- a function to process the returned data, 
executeSelect : String ->
                String -> 
                List (Int, DBVal) -> 
                (Eff m [SQLITE (SQLiteRes PreparedStatementExecuting)] (List DBVal)) -> 
                Eff m [SQLITE ()] (Either String ResultSet)
executeSelect db_name q bind_vals fn = do
  conn <- openDB db_name
  if conn then do
    prep_sql <- prepareStatement q
    if prep_sql then do
      bind_res <- multiBind bind_vals
      if bind_res then do
        beginExecution
        res <- collectResults fn
        finaliseStatement
        closeDB
        Effects.pure $ res
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err
      
