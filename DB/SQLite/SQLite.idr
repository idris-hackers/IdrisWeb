--Effect implementation of the SQLite3 Library.
-- Major credits go to SQLite3 library authors.
module IdrisWeb.DB.SQLite.SQLite
--import Sqlexpr
import Effects
import SQLiteCodes

%lib "sqlite3"
%link "sqlite3api.o"
%include "sqlite3api.h"

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

data Value = VInt Int
           | VStr String
           | VFloat Float


-- Type synonym for a table
Table : Type
Table = List (List DBVal)

ArgPos : Type
ArgPos = Int


data Step = ConnectionOpened
          | PreparedStatementOpen
          | PreparedStatementBinding
          | PreparedStatementBound
          | PreparedStatementExecuted
--          | PreparedStatementResultsFetched



data SQLiteRes : Step -> Type where
  OpenConn : DBPointer -> SQLiteRes s
  OpenStmt : DBPointer -> StmtPointer -> SQLiteRes s

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
  ExecuteStatement : Sqlite (SQLiteRes PreparedStatementBound) (SQLiteRes PreparedStatementExecuted) Bool

  -- Fetches the results of the previously-executed query
  {-FetchResults : Sqlite (SQLiteRes PreparedStatementExecuted) 
                        (SQLiteRes PreparedStatementResultsFetched) 
                        Table
                        -}
  -- Disposes of a prepared statement
  Finalise : Sqlite (SQLiteRes PreparedStatementExecuted) (SQLiteRes ConnectionOpened) Bool


  ----- Operations on result sets.
  -- SQLite returns data on a row-by-row basis, which is advanced using the step
  -- function. The position of this pointer can also be reset using the reset function.

  -- These operations are done performed on the current row.
  -- Given a column index, gets the name of the column
  GetColumnName : Int -> Sqlite (SQLiteRes PreparedStatementExecuted) 
                                (SQLiteRes PreparedStatementExecuted)
                                String


  -- Given a column index, returns the size in bytes
  GetColumnDataSize : Int -> Sqlite (SQLiteRes PreparedStatementExecuted)
                                    (SQLiteRes PreparedStatementExecuted)
                                    Int


  -- Not sure how we should handle blobs. Underlying library says string, but I don't
  -- think this entirely makes sense. Will come back to it, omitting for now.

  -- Given a column index, returns the data as an Idris string.
  GetColumnText : Int -> Sqlite (SQLiteRes PreparedStatementExecuted)
                                (SQLiteRes PreparedStatementExecuted)
                                String

  -- Given a column index, returns the data as an Idris integer.
  GetColumnInt : Int -> Sqlite (SQLiteRes PreparedStatementExecuted)
                               (SQLiteRes PreparedStatementExecuted)
                               Int

  -- Advance the row pointer
  -- TODO: We will get a particular status code when we reach the end of the
  --       results. Ideally, this should be encoded as a state to ensure that
  --       step is not called once all rows have been seen.
  RowStep : Sqlite (SQLiteRes PreparedStatementExecuted)
                   (SQLiteRes PreparedStatementExecuted)
                   StepResult

  Reset : Sqlite (SQLiteRes PreparedStatementExecuted)
                 (SQLiteRes PreparedStatementExecuted)
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
  handle (OpenStmt (ValidConn c) (ValidStmt s)) ExecuteStatement k = do
    x <- mkForeign (FFun "exec_db" [FPtr] FInt) s
    -- Ideally, an execution failure should debar us from attempting 
    -- to retrieve the results. 
    -- Currently, we can do this, and the call would still go through to 
    -- the library.
    -- TODO: Fix this, possibly by having another type of StmtPointer which
    -- signifies that the results have been tainted by some failure,
    -- but that the pointer is still active and needs to be freed.
    if (x == sqlite_OK) then k (OpenStmt (ValidConn c) (ValidStmt s)) True
                        else k (OpenStmt (ValidConn c) (ValidStmt s)) False


  handle (OpenStmt (ValidConn c) InvalidStmt) ExecuteStatement k = 
    k (OpenStmt (ValidConn c) InvalidStmt) False

  handle (OpenStmt InvalidConn x) ExecuteStatement k = 
    k (OpenStmt InvalidConn x) False --ExecuteStatement

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
  handle (OpenStmt (ValidConn c) (ValidStmt s)) (GetColumnName i) k = do
    res <- mkForeign (FFun "sqlite3_column_name_idr" [FPtr, FInt] FString) c i 
    k (OpenStmt (ValidConn c) (ValidStmt s)) res


  handle (OpenStmt (ValidConn c) (ValidStmt s)) (GetColumnDataSize i) k = do
    res <- mkForeign (FFun "sqlite3_column_bytes_idr" [FPtr, FInt] FInt) c i
    k (OpenStmt (ValidConn c) (ValidStmt s)) res

  handle (OpenStmt (ValidConn c) (ValidStmt s)) (GetColumnInt i) k = do
    res <- mkForeign (FFun "sqlite3_column_int_idr" [FPtr, FInt] FInt) c i
    k (OpenStmt (ValidConn c) (ValidStmt s)) res

  handle (OpenStmt (ValidConn c) (ValidStmt s)) (GetColumnText i) k = do
    res <- mkForeign (FFun "sqlite3_column_text_idr" [FPtr, FInt] FString) c i 
    k (OpenStmt (ValidConn c) (ValidStmt s)) res

  -- Pass-through handlers
  -- Urgh, perhaps best to encapsulate these failures in a maybe?
  -- In fact, we *really* should. Otherwise these values may be used in further
  -- computations in some circumstances.
  handle (OpenStmt InvalidConn x) (GetColumnName i) k = k (OpenStmt InvalidConn x) ""
  handle (OpenStmt (ValidConn c) InvalidStmt) (GetColumnName i) k =
    k (OpenStmt (ValidConn c) InvalidStmt) ""

  handle (OpenStmt InvalidConn x) (GetColumnDataSize i) k = k (OpenStmt InvalidConn x) (-1)
  handle (OpenStmt (ValidConn c) InvalidStmt) (GetColumnDataSize i) k =
    k (OpenStmt (ValidConn c) InvalidStmt) (-1)

  handle (OpenStmt InvalidConn x) (GetColumnText i) k = k (OpenStmt InvalidConn x) ""
  handle (OpenStmt (ValidConn c) InvalidStmt) (GetColumnText i) k =
    k (OpenStmt (ValidConn c) InvalidStmt) ""

  handle (OpenStmt InvalidConn x) (GetColumnInt i) k = k (OpenStmt InvalidConn x) (-1)
  handle (OpenStmt (ValidConn c) InvalidStmt) (GetColumnInt i) k =
    k (OpenStmt (ValidConn c) InvalidStmt) (-1)

  -- Step and reset
  handle (OpenStmt (ValidConn c) (ValidStmt s)) RowStep k = do
    res <- mkForeign (FFun "sqlite3_step_idr" [FPtr] FInt) c
    k (OpenStmt (ValidConn c) (ValidStmt s)) (stepResult res)

  handle (OpenStmt (ValidConn c) InvalidStmt) RowStep k = 
    k (OpenStmt (ValidConn c) InvalidStmt) StepFail
  handle (OpenStmt InvalidConn x) RowStep k = k (OpenStmt InvalidConn x) StepFail

  handle (OpenStmt (ValidConn c) (ValidStmt s)) Reset k = do
    res <- mkForeign (FFun "sqlite3_reset_idr" [FPtr] FInt) c
    k (OpenStmt (ValidConn c) (ValidStmt s)) (res == sqlite_OK)

  handle (OpenStmt (ValidConn c) InvalidStmt) Reset k = k (OpenStmt (ValidConn c) InvalidStmt) False
  handle (OpenStmt InvalidConn x) Reset k = k (OpenStmt InvalidConn x) False

  -- Finalise statement
  handle (OpenStmt (ValidConn c) (ValidStmt s)) Finalise k = do
    res <- mkForeign (FFun "sqlite3_finalize_idr" [FPtr] FInt) s
    k (OpenStmt (ValidConn c) (ValidStmt s)) (res == sqlite_OK)

  handle (OpenStmt (ValidConn c) InvalidStmt) Finalise k = 
    k (OpenStmt (ValidConn c) InvalidStmt) False

  handle (OpenStmt InvalidConn x) Finalise k = k (OpenStmt InvalidConn x) False

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

bindText : ArgPos -> String -> Int -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] Bool
bindText pos str len = (BindText pos str len)

bindNull : ArgPos -> Eff m [SQLITE (SQLiteRes PreparedStatementBinding)] Bool
bindNull pos = (BindNull pos)

finishBind : EffM m [SQLITE (SQLiteRes PreparedStatementBinding)] 
                    [SQLITE (SQLiteRes PreparedStatementBound)] Bool
finishBind = FinishBind

executeStatement : EffM m [SQLITE (SQLiteRes PreparedStatementBound)] 
                          [SQLITE (SQLiteRes PreparedStatementExecuted)] Bool
executeStatement = ExecuteStatement

finaliseStatement : EffM m [SQLITE (SQLiteRes PreparedStatementExecuted)]
                           [SQLITE (SQLiteRes ConnectionOpened)]
                           Bool
finaliseStatement = Finalise

getColumnName : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuted)] String
getColumnName pos = (GetColumnName pos)

getColumnDataSize : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuted)] Int
getColumnDataSize pos = (GetColumnDataSize pos)

getColumnText : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuted)] String
getColumnText pos = (GetColumnText pos)

getColumnInt : Int -> Eff m [SQLITE (SQLiteRes PreparedStatementExecuted)] Int
getColumnInt pos = (GetColumnInt pos)

nextRow : Eff m [SQLITE (SQLiteRes PreparedStatementExecuted)] StepResult
nextRow = RowStep

resetPos : Eff m [SQLITE (SQLiteRes PreparedStatementExecuted)] Bool
resetPos = Reset

-- Utility functions to help handle failure

