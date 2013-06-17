module IdrisWeb.DB.SQLite.SQLiteCodes
-- Status codes for SQLite

%access public

SQLiteCode : Type
SQLiteCode = Int

sqlite_OK : Int
sqlite_OK = 0 -- Successful result 
-- beginning-of-error-codes 

sqlite_ERROR : Int
sqlite_ERROR = 1 -- SQL error or missing database 

sqlite_INTERNAL : Int
sqlite_INTERNAL = 2 -- Internal logic error in SQLite 

sqlite_PERM : Int
sqlite_PERM = 3 -- Access permission denied 

sqlite_ABORT : Int
sqlite_ABORT = 4 -- Callback routine requested an abort 

sqlite_BUSY : Int
sqlite_BUSY = 5 -- The database file is locked 

sqlite_LOCKED : Int
sqlite_LOCKED = 6 -- A table in the database is locked 

sqlite_NOMEM : Int
sqlite_NOMEM = 7 -- A malloc() failed 

sqlite_READONLY : Int
sqlite_READONLY = 8 -- Attempt to write a readonly database 

sqlite_INTERRUPT : Int
sqlite_INTERRUPT = 9 -- Operation terminated by sqlite3_interrupt()

sqlite_IOERR : Int
sqlite_IOERR = 10 -- Some kind of disk I/O error occurred 

sqlite_CORRUPT : Int
sqlite_CORRUPT = 11 -- The database disk image is malformed 

sqlite_NOTFOUND : Int
sqlite_NOTFOUND = 12 -- Unknown opcode in sqlite3_file_control() 

sqlite_FULL : Int
sqlite_FULL = 13 -- Insertion failed because database is full 

sqlite_CANTOPEN : Int
sqlite_CANTOPEN = 14 -- Unable to open the database file 

sqlite_PROTOCOL : Int
sqlite_PROTOCOL = 15 -- Database lock protocol error 

sqlite_EMPTY : Int
sqlite_EMPTY = 16 -- Database is empty 

sqlite_SCHEMA : Int
sqlite_SCHEMA = 17 -- The database schema changed 

sqlite_TOOBIG : Int
sqlite_TOOBIG = 18 -- String or BLOB exceeds size limit 

sqlite_CONSTRAINT : Int
sqlite_CONSTRAINT = 19 -- Abort due to constraint violation 

sqlite_MISMATCH : Int
sqlite_MISMATCH = 20 -- Data type mismatch 

sqlite_MISUSE : Int
sqlite_MISUSE = 21 -- Library used incorrectly 

sqlite_NOLFS : Int
sqlite_NOLFS = 22 -- Uses OS features not supported on host 

sqlite_AUTH : Int
sqlite_AUTH = 23 -- Authorization denied 

sqlite_FORMAT : Int
sqlite_FORMAT = 24 -- Auxiliary database format error 

sqlite_RANGE : Int
sqlite_RANGE = 25 -- 2nd parameter to sqlite3_bind out of range 

sqlite_NOTADB : Int
sqlite_NOTADB = 26 -- File opened that is not a database file 

sqlite_NOTICE : Int
sqlite_NOTICE = 27 -- Notifications from sqlite3_log() 

sqlite_WARNING : Int
sqlite_WARNING = 28 -- Warnings from sqlite3_log() 

sqlite_ROW : Int
sqlite_ROW = 100 -- sqlite3_step() has another row ready 

sqlite_DONE : Int
sqlite_DONE = 101 -- sqlite3_step() has finished executing 
-- end-of-error-codes 

data StepResult = StepFail
                | StepComplete
                | NoMoreRows


stepResult : Int -> StepResult 
stepResult sqlite_ROW = StepComplete -- step complete, but more data available
stepResult sqlite_DONE = NoMoreRows -- statement has been fully executed
stepResult _ = StepFail -- an error occurred

