--module IdrisWeb.DB.SQLite.SQLiteTest
module Main
import Effects
import SQLite
import SQLiteCodes


testInsert : String -> Int -> EffM IO [SQLITE ()] [SQLITE ()] (Either String Int)
testInsert name age = do 
                open_db <- openDB "test.db"
                if open_db then do
                  let sql = "INSERT INTO `test` (`name`, `age`) VALUES (?, ?);"
                  sql_prep_res <- prepareStatement sql
                  if sql_prep_res then do
                    startBind
                    bindText 1 name 
                    bindInt 2 age
                    bind_res <- finishBind
                    if bind_res then do
                                  beginExecution
                                  executeInsert
                                else do
                                  err <- bindFail
                                  Effects.pure $ Left err
                  else do
                    err <- stmtFail
                    Effects.pure $ Left err
                else do
                  err <- connFail
                  Effects.pure $ Left err


                      
collectResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] (List (String, Int))
collectResults = do
  step_result <- nextRow
  case step_result of
      StepComplete => do name <- getColumnText 1
                         age <- getColumnInt 2
                         xs <- collectResults
                         Effects.pure $ (name, age) :: xs
      NoMoreRows => Effects.pure []
      StepFail => Effects.pure []



testSelect : Eff IO [SQLITE ()] (Either String (List (String, Int)))
testSelect = do
  open_db <- openDB "test.db"
  if open_db then do
    let sql = "SELECT * FROM `test`;"
    sql_prep_res <- prepareStatement sql
    if sql_prep_res then do 
      startBind
      finishBind
      beginExecution
      results <- collectResults
      finaliseStatement
      closeDB
      Effects.pure $ Right results
    else do err <- stmtFail
            Effects.pure $ Left err
  else do err <- connFail
          Effects.pure $ Left err
                                       
main : IO ()
main =
{-  do select_res <- run [()] testSelect
          case select_res of
               Left err => putStrLn $ "Error: " ++ err
               Right results => do mapM (putStrLn . show) results
                                   pure ()
                                   -}
  do res <- run [()] (testInsert "SimonJF" (fromInteger 21))
     case res of Left err => putStrLn $ "Error inserting: " ++ err
                 Right i_id => putStrLn $ "Operation completed successfully; insert ID: " ++ (show i_id)

