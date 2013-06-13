module IdrisWeb.DB.SQLite.SQLiteCodes
-- Status codes for SQLite

%access public

SQLiteCode : Type
SQLiteCode = Int

SQLITE_OK : SQLiteCode
SQLITE_OK = 0 -- Successful result 
-- beginning-of-error-codes 

SQLITE_ERROR : SQLiteCode
SQLITE_ERROR = 1 -- SQL error or missing database 

SQLITE_INTERNAL : SQLiteCode
SQLITE_INTERNAL = 2 -- Internal logic error in SQLite 

SQLITE_PERM : SQLiteCode
SQLITE_PERM = 3 -- Access permission denied 

SQLITE_ABORT : SQLiteCode
SQLITE_ABORT = 4 -- Callback routine requested an abort 

SQLITE_BUSY : SQLiteCode
SQLITE_BUSY = 5 -- The database file is locked 

SQLITE_LOCKED : SQLiteCode
SQLITE_LOCKED = 6 -- A table in the database is locked 

SQLITE_NOMEM : SQLiteCode
SQLITE_NOMEM = 7 -- A malloc() failed 

SQLITE_READONLY : SQLiteCode
SQLITE_READONLY = 8 -- Attempt to write a readonly database 

SQLITE_INTERRUPT : SQLiteCode
SQLITE_INTERRUPT = 9 -- Operation terminated by sqlite3_interrupt()

SQLITE_IOERR : SQLiteCode
SQLITE_IOERR = 10 -- Some kind of disk I/O error occurred 

SQLITE_CORRUPT : SQLiteCode
SQLITE_CORRUPT = 11 -- The database disk image is malformed 

SQLITE_NOTFOUND : SQLiteCode
SQLITE_NOTFOUND = 12 -- Unknown opcode in sqlite3_file_control() 

SQLITE_FULL : SQLiteCode
SQLITE_FULL = 13 -- Insertion failed because database is full 

SQLITE_CANTOPEN : SQLiteCode
SQLITE_CANTOPEN = 14 -- Unable to open the database file 

SQLITE_PROTOCOL : SQLiteCode
SQLITE_PROTOCOL = 15 -- Database lock protocol error 

SQLITE_EMPTY : SQLiteCode
SQLITE_EMPTY = 16 -- Database is empty 

SQLITE_SCHEMA : SQLiteCode
SQLITE_SCHEMA = 17 -- The database schema changed 

SQLITE_TOOBIG : SQLiteCode
SQLITE_TOOBIG = 18 -- String or BLOB exceeds size limit 

SQLITE_CONSTRAINT : SQLiteCode
SQLITE_CONSTRAINT = 19 -- Abort due to constraint violation 

SQLITE_MISMATCH : SQLiteCode
SQLITE_MISMATCH = 20 -- Data type mismatch 

SQLITE_MISUSE : SQLiteCode
SQLITE_MISUSE = 21 -- Library used incorrectly 

SQLITE_NOLFS : SQLiteCode
SQLITE_NOLFS = 22 -- Uses OS features not supported on host 

SQLITE_AUTH : SQLiteCode
SQLITE_AUTH = 23 -- Authorization denied 

SQLITE_FORMAT : SQLiteCode
SQLITE_FORMAT = 24 -- Auxiliary database format error 

SQLITE_RANGE : SQLiteCode
SQLITE_RANGE = 25 -- 2nd parameter to sqlite3_bind out of range 

SQLITE_NOTADB : SQLiteCode
SQLITE_NOTADB = 26 -- File opened that is not a database file 

SQLITE_NOTICE : SQLiteCode
SQLITE_NOTICE = 27 -- Notifications from sqlite3_log() 

SQLITE_WARNING : SQLiteCode
SQLITE_WARNING = 28 -- Warnings from sqlite3_log() 

SQLITE_ROW : SQLiteCode
SQLITE_ROW = 100 -- sqlite3_step() has another row ready 

SQLITE_DONE : SQLiteCode
SQLITE_DONE = 101 -- sqlite3_step() has finished executing 
-- end-of-error-codes 
