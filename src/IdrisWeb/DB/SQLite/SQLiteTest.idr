--module IdrisWeb.DB.SQLite.SQLiteTest
module Main
import Effects
import SQLiteNew
import SQLiteCodes


testInsert : String -> Int -> EffM IO [SQLITE ()] [SQLITE ()] (Either SQLiteCode ())
testInsert name age = do 
  open_db <- openDB "test.db"
  if_valid then do
    let sql = "INSERT INTO `test` (`name`, `age`) VALUES (?, ?);"
    prep_res <- prepareStatement sql
    if_valid then do
      bindText 1 name 
      bindInt 2 age
      bind_res <- finishBind
      if_valid then do
        executeStatement
        finalise 
        closeDB
        Effects.pure $ Right ()
      else do
        cleanupBindFail
        Effects.pure $ (Left (-1))
    else do
      cleanupPSFail
--                  let (Left err) = prep_res
      Effects.pure $ Left (-2)
  else do
--                let (Left err) = open_db
    Effects.pure $ Left (-3)


collectResults : EffM IO [SQLITE (Either (SQLiteExecuting InvalidRow) 
                                        (SQLiteExecuting ValidRow))] 
                         [SQLITE (SQLiteExecuting InvalidRow)] 
                         (List (String, Int))
collectResults = 
  if_valid then do
    name <- getColumnText 0
    age <- getColumnInt 1
    step_result <- nextRow
    xs <- collectResults
    Effects.pure $ (name, age) :: xs
  else Effects.pure []

{-
  step_result <- nextRow
  case step_result of
      StepComplete => do name <- getColumnText 1
                         age <- getColumnInt 2
                         xs <- collectResults
                         Effects.pure $ (name, age) :: xs
      NoMoreRows => Effects.pure []
      StepFail => Effects.pure []
-}


testSelect : Eff IO [SQLITE ()] (Either Int (List (String, Int)))
testSelect = do
  open_db <- openDB "test.db"
  if_valid then do
    let sql = "SELECT * FROM `test`;"
    sql_prep_res <- prepareStatement sql
    if_valid then do 
      finishBind
      if_valid then do
        executeStatement
        results <- collectResults
        finaliseInvalid
        closeDB
        Effects.pure $ Right results
      else do cleanupBindFail
              Effects.pure $ Left (-1)
    else do cleanupPSFail
            Effects.pure $ Left (-2)
  else Effects.pure $ Left (-3)
  
main : IO ()
main = do select_res <- run [()] testSelect
          case select_res of
               Left err => putStrLn $ "Error: " ++ (show err)
               Right results => do traverse (putStrLn . show) results
                                   pure ()


{-
main : IO ()
main = do insert_res <- run [()] (testInsert "Simon" 21)
          case insert_res of 
            Left err => putStrLn $ "Error inserting" ++ (show err)
            Right _ => putStrLn $ "Operation completed successfully."
-}


