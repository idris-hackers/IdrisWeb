module Main
import Cgi
import Session
import SessionUtils
import SQLite

ThreadID : Type
ThreadID = Int

DB_NAME : String
DB_NAME = "/tmp/messageboard.db"

UserID : Type
UserID = Int

USERID_VAR : String
USERID_VAR = "user_id"

----------
-- Handler info
----------
handleRegisterForm : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning),
                                                                  SESSION (SessionRes SessionUninitialised),
                                                                  SQLITE ()
                                                                 ]

handlePost : Maybe Int -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning), 
                                                            SESSION (SessionRes SessionUninitialised), 
                                                            SQLITE ()
                                                      ] 

handleNewThread : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning), 
                                                               SESSION (SessionRes SessionUninitialised), 
                                                               SQLITE ()
                                                              ]  

handleLoginForm : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning),
                                                                  SESSION (SessionRes SessionUninitialised),
                                                                  SQLITE ()
                                                              ] 

handlers : HandlerList
handlers = [(([FormString, FormString], [CgiEffect, SessionEffect, SqliteEffect]) ** (handleRegisterForm, "handleRegisterForm")),
            (([FormString, FormString], [CgiEffect, SessionEffect, SqliteEffect]) ** (handleLoginForm, "handleLoginForm")),
            (([FormString, FormString], [CgiEffect, SessionEffect, SqliteEffect]) ** (handleNewThread, "handleNewThread")),
            (([FormInt, FormString], [CgiEffect, SessionEffect, SqliteEffect]) ** (handlePost, "handlePost"))]
 

-- Template system would be nice...
htmlPreamble : String
htmlPreamble = "<html><head><title>IdrisWeb Message Board</title></head><body>"

htmlPostamble : String
htmlPostamble = "</body></html>"

notLoggedIn : EffM IO [CGI (InitialisedCGI TaskRunning), 
                       SESSION (SessionRes SessionInitialised), 
                       SQLITE ()] 
                      [CGI (InitialisedCGI TaskRunning), 
                       SESSION (SessionRes SessionUninitialised), 
                       SQLITE ()] () 
notLoggedIn = do output htmlPreamble
                 output "<h1>Error</h1><br />You must be logged in to do that!"
                 output htmlPostamble 
                 discardSession

outputWithPreamble : String -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
outputWithPreamble txt = do output htmlPreamble
                            output txt
                            output htmlPostamble
----------- 
-- Post Creation
-----------
postInsert : Int -> Int -> String -> Eff IO [SQLITE ()] Bool
postInsert uid thread_id content = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "INSERT INTO `Posts` (`UserID`, `ThreadID`, `Content`) VALUES (?, ?, ?)"
    stmt_res <- prepareStatement sql
    if stmt_res then do
      startBind
      bindInt 1 uid
      bindInt 2 thread_id
      bindText 3 content
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        nextRow
        finaliseStatement
        closeDB
        Effects.pure True
      else do bindFail
              Effects.pure False
    else do stmtFail
            Effects.pure False
  else do connFail
          Effects.pure False

addPostToDB : Int -> String -> SessionData -> EffM IO [CGI (InitialisedCGI TaskRunning),
                                                       SESSION (SessionRes SessionInitialised),
                                                       SQLITE ()]
                                                      [CGI (InitialisedCGI TaskRunning),
                                                       SESSION (SessionRes SessionUninitialised),
                                                       SQLITE ()] ()
addPostToDB thread_id content sd = do
-- TODO: would be nice to abstract this out
  case lookup USERID_VAR sd of
    Just (SInt uid) => do insert_res <- postInsert uid thread_id content
                          if insert_res then do
                            -- TODO: redirection would be nice
                            outputWithPreamble "Post successful"
                            discardSession
                            Effects.pure ()
                          else do
                            outputWithPreamble "There was an error adding the post to the database."
                            discardSession
                            Effects.pure ()
    Nothing => do notLoggedIn
                  Effects.pure ()
                         


handlePost (Just thread_id) (Just content) = do withSession (addPostToDB thread_id content) notLoggedIn
                                                pure ()
handlePost _ _ = do outputWithPreamble"<h1>Error</h1><br />There was an error processing your post."
                    pure ()
{-
newPostForm : Int -> UserForm
newPostForm thread_id = do
  addHidden FormInt thread_id
  addTextBox "Post Content" FormString Nothing
  useEffects [CgiEffect, SessionEffect, SqliteEffect]
  addSubmit handlePost handlers


showNewPostForm : Int -> CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
showNewPostForm thread_id = do
  output htmlPreamble 
  output "<h2>Create new post</h2>"
  addForm "newPostForm" "messageboard" (newPostForm thread_id)
  output htmlPostamble
-}
----------- 
-- Thread Creation
-----------

irritating_inner_case_fix_threadinsert : Int -> String -> EffM IO [SQLITE (SQLiteRes PreparedStatementExecuting)] [SQLITE ()] Bool
irritating_inner_case_fix_threadinsert uid content = do
        thread_insert_res <- executeInsert 
        case thread_insert_res of
          Left err => do closeDB
                         pure False
          -- If the thread inserted correctly, insert the first post
          -- Ideally, this would be a transaction, but it will do for now.
          Right thread_id => do closeDB
                                postInsert uid thread_id content
                                pure True

threadInsert : Int -> String -> String -> Eff IO [SQLITE ()] Bool
threadInsert uid title content = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "INSERT INTO `Threads` (`UserID`, `Title`) VALUES (?, ?)"
    stmt_res <- prepareStatement sql
    if stmt_res then do
      startBind
      bindInt 1 uid
      bindText 2 title
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        irritating_inner_case_fix_threadinsert uid content
      else do bindFail
              pure False
    else do stmtFail
            pure False
  else do connFail
          pure False

addNewThread : String -> String -> SessionData -> EffM IO [CGI (InitialisedCGI TaskRunning),
                                                       SESSION (SessionRes SessionInitialised),
                                                       SQLITE ()]
                                                      [CGI (InitialisedCGI TaskRunning),
                                                       SESSION (SessionRes SessionUninitialised),
                                                       SQLITE ()] ()
addNewThread title content sd = do 
-- TODO: would be nice to abstract this out
  case lookup USERID_VAR sd of
    Just (SInt uid) => do insert_res <- threadInsert uid title content
                          if insert_res then do
                            -- TODO: redirection would be nice
                            output "Thread creation successful" 
                            discardSession
                            pure () 
                          else do
                            output "There was an error adding the thread to the database."
                            discardSession
                            pure ()
    Nothing => do notLoggedIn
                  pure ()
  
-- Create a new thread, given the title and content
handleNewThread (Just title) (Just content) = do withSession (addNewThread title content) notLoggedIn
                                                 pure ()
handleNewThread _ _ = do outputWithPreamble "<h1>Error</h1><br />There was an error posting your thread."
                         pure ()

newThreadForm : UserForm
newThreadForm = do
  addTextBox "Title" FormString Nothing
  addTextBox "Post Content" FormString Nothing -- password field would be good
  useEffects [CgiEffect, SessionEffect, SqliteEffect]
  addSubmit handleNewThread handlers 


showNewThreadForm : CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
showNewThreadForm = do output htmlPreamble
                       output "<h1>New Thread</h1>"
                       addForm "newThreadForm" "messageboard" newThreadForm
                       output htmlPostamble


----------- 
-- Registration
-----------

innerInsertCase : (Either String Int) -> Eff IO [SQLITE ()] (Either String ())
innerInsertCase (Left err) = Effects.pure (Left err)
innerInsertCase _ = Effects.pure (Right ())

-- UP TO HERE
insertUser : String -> String -> Eff IO [SQLITE ()] (Either String ())
insertUser name pwd = do
  conn <- openDB DB_NAME
  if conn then do
    let sql = "INSERT INTO `Users` (`Username`, `Password`) VALUES (?, ?)"
    sql_prep_res <- prepareStatement sql
    if sql_prep_res then do
      startBind
      bindText 1 name
      bindText 2 pwd
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        res <- executeInsert
        closeDB
        innerInsertCase res
      else do
        err <- bindFail
        Effects.pure $ Left err 
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err

-- Fine up until here
userExists' : StepResult -> EffM IO [SQLITE (SQLiteRes PreparedStatementExecuting)] [SQLITE ()] (Either String Bool)
userExists' next_row_res =
  case next_row_res of
    StepComplete => do
      num_rows <- getColumnInt 0
      finaliseStatement
      closeDB
      Effects.pure $ Right (num_rows > 0)
    _ => do executeFail
            Effects.pure $ Left "Error executing row count function"


userExists : String -> Eff IO [SQLITE ()] (Either String Bool)
userExists username = do
  conn <- openDB DB_NAME
  if conn then do
    let sql = "SELECT count(*) FROM `Users` WHERE `Username` = ?"
    prep_res <- prepareStatement sql
    if prep_res then do
      startBind 
      bindText 1 username
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        next_row_res <- nextRow
        userExists' next_row_res -- Temporary bug workaround
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail 
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err


handleRegisterForm (Just name) (Just pwd) = do 
  user_exists_res <- userExists name
  case user_exists_res of
    Left err => do outputWithPreamble "Error checking for user existence"
                   pure ()
    Right user_exists => 
      if (not user_exists) then do 
        insert_res <- insertUser name pwd
        case insert_res of
          Left err => do outputWithPreamble ("Error inserting new user" ++ err)
                         pure ()
          Right insert_res => do outputWithPreamble "User created successfully!"
                                 pure ()
      else do outputWithPreamble "This user already exists; please pick another name!"
              pure ()
                       
handleRegisterForm _ _ = do outputWithPreamble "Error processing form input data."
                            pure ()

registerForm : UserForm
registerForm = do
  addTextBox "Username" FormString Nothing
  addTextBox "Password" FormString Nothing -- password field would be good
  useEffects [CgiEffect, SessionEffect, SqliteEffect]
  addSubmit handleRegisterForm handlers

showRegisterForm : CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
showRegisterForm = do output htmlPreamble
                      output "<h1>Create a new account</h1>"
                      addForm "registerForm" "messageboard" registerForm
                      output htmlPostamble

----------- 
-- Login 
-----------
alreadyLoggedIn : SessionData ->
                  EffM IO [CGI (InitialisedCGI TaskRunning), 
                           SESSION (SessionRes SessionInitialised), 
                           SQLITE ()] 
                          [CGI (InitialisedCGI TaskRunning), 
                           SESSION (SessionRes SessionUninitialised), 
                           SQLITE ()] () 
alreadyLoggedIn _ = do outputWithPreamble "<h1>Error</h1><br />You appear to already be logged in!"
                       discardSession
-- If the credentials match, return an ID
-- Maybe consolidate the Maybe UserID into the Either, or possibly keep them
-- distinct to encapsulate the system error vs auth failure
authUser' : StepResult -> EffM IO [SQLITE (SQLiteRes PreparedStatementExecuting)] [SQLITE ()] (Either String (Maybe UserID))
authUser' next_row_res = 
  case next_row_res of
    StepComplete => do
      user_id <- getColumnInt 0
      finaliseStatement
      closeDB
      Effects.pure $ Right (Just user_id)
    NoMoreRows => do
      finaliseStatement
      closeDB
      Effects.pure $ Right Nothing -- No user with the given credentials
    _ => do executeFail
            Effects.pure $ Left "Error executing row count function"


authUser : String -> String -> Eff IO [SQLITE ()] (Either String (Maybe UserID))
authUser username password = do
  conn <- openDB DB_NAME
  if conn then do
    let sql = "SELECT `UserID` FROM `Users` WHERE `Username` = ? AND `Password` = ?"
    prep_res <- prepareStatement sql
    if prep_res then do
      startBind 
      bindText 1 username
      bindText 2 password 
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        next_row_res <- nextRow
        authUser' next_row_res
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail 
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err


setSession : UserID -> Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised), SQLITE ()] Bool
setSession user_id = do
  create_res <- lift (Drop (Keep (Drop (SubNil)))) (createSession [(USERID_VAR, SInt user_id)])
  sess_res <- lift (Keep (Keep (Drop (SubNil)))) setSessionCookie
  db_res <- lift (Drop (Keep (Drop (SubNil)))) writeSessionToDB
  pure (sess_res && db_res)


handleLoginForm (Just name) (Just pwd) = do
  auth_res <- lift' (authUser name pwd)
  case auth_res of
    Right (Just uid) => do
      set_sess_res <- setSession uid
      if set_sess_res then do
        lift' (output $ "Welcome, " ++ name)
        pure ()
      else do
        lift' (output "Could not set session")
        pure ()
    Right Nothing => do
      lift' (output "Invalid username or password")
      pure ()
    Left err => do
      lift' (output $ "Error: " ++ err)
      pure ()


loginForm : UserForm
loginForm = do
  addTextBox "Username" FormString Nothing
  addTextBox "Password" FormString Nothing -- password field would be good
  useEffects [CgiEffect, SessionEffect, SqliteEffect]
  addSubmit handleLoginForm handlers

showLoginForm : CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
showLoginForm = do output htmlPreamble
                   output "<h1>Log in</h1>"
                   addForm "loginForm" "messageboard" loginForm
                   output "</html>"



----------- 
-- Post / Thread Display
-----------

--asInt : Integer -> Int
--asInt i = fromInteger i

collectPostResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] (List (String, String))
collectPostResults = do
  step_result <- nextRow
  case step_result of
    StepComplete => do name <- getColumnText 0
                       content <- getColumnText 1
                       xs <- collectPostResults
                       Effects.pure $ (name, content) :: xs
    NoMoreRows => Effects.pure []
    StepFail => Effects.pure []

-- Gets the posts
getPosts : Int -> Eff IO [SQLITE ()] (Either String (List (String, String)))
getPosts thread_id = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "SELECT `Username`, `Content` FROM `Posts` NATURAL JOIN `Users` WHERE `ThreadID` = ?"
    prep_res <- prepareStatement sql
    if prep_res then do
      startBind
      bindInt 1 thread_id
      bind_res <- finishBind
      if bind_res then do
        beginExecution
        results <- collectPostResults
        finaliseStatement
        closeDB 
        Effects.pure $ Right results
      else do
        err <- bindFail
        Effects.pure $ Left err
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err



collectThreadResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] (List (Int, String, Int, String))
collectThreadResults = do
  step_result <- nextRow
  case step_result of
    StepComplete => do thread_id <- getColumnInt 0
                       title <- getColumnText 1
                       uid <- getColumnInt 2
                       username <- getColumnText 3
                       xs <- collectThreadResults
                       Effects.pure $ (thread_id, title, uid, username) :: xs
    NoMoreRows => Effects.pure []
    StepFail => Effects.pure []


-- Returns (Title, Thread starter ID, Thread starter name)
getThreads : Eff IO [SQLITE ()] (Either String (List (Int, String, Int, String)))
getThreads = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "SELECT `ThreadID`, `Title`, `UserID`, `Username` FROM `Threads` NATURAL JOIN `Users`"
    prep_res <- prepareStatement sql
    if prep_res then do
      startBind
      finishBind
      beginExecution
      results <- collectThreadResults
      finaliseStatement
      closeDB
      Effects.pure $ Right results
    else do
      err <- stmtFail
      Effects.pure $ Left err
  else do
    err <- connFail
    Effects.pure $ Left err


traversePosts : List (String, String) -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
traversePosts [] = pure ()
traversePosts ((name, content) :: xs) = do output $ "<tr><td>" ++ name ++ "</td><td>" ++ content ++ "</td></tr>" 
                                           traversePosts xs 


printPosts : ThreadID -> CGIProg [SQLITE ()] ()
printPosts thread_id = do 
  post_res <- lift' (getPosts thread_id)
  case post_res of
    Left err => do lift' (output $ "Could not retrieve posts, error: " ++ err)
                   Effects.pure ()
    Right posts => do lift' (output "<table>")
                      traversePosts posts
                      lift' (output "</table>")
                      lift' (output $ "<a href=\"?action=newpost&thread_id=" ++ (show thread_id) ++ "\">New post</a><br />")

                      --(Prelude.Applicative.traverse (\(name, content) => (lift (Keep (Drop (SubNil))) (output $ "<tr><td>" ++ name ++ "</td><td>" ++ content ++ "</td></tr>"))
                      -- posts))
                      Effects.pure ()

traverseThreads : List (Int, String, Int, String) -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
traverseThreads [] = pure ()
traverseThreads ((thread_id, title, uid, username) :: xs) = do
  (output $ "<tr><td><a href=\"?action=showthread&thread_id=" ++ 
    (show thread_id) ++ "\">" ++ title ++ "</a></td><td>" ++ username ++ "</td></tr>")
  traverseThreads xs

printThreads : CGIProg [SQLITE ()] ()
printThreads = do
  thread_res <- getThreads
  case thread_res of
    Left err => do lift' (output $ "Could not retrieve threads, error: " ++ err)
                   Effects.pure ()
    Right threads => do lift' (output htmlPreamble)
                        lift (Keep (Drop (SubNil))) (output "<table><tr><th>Title</th><th>Author</th></tr>")
                        traverseThreads threads
                        lift (Keep (Drop (SubNil))) (output "</table><br />")
                        output "<a href=\"?action=newthread\">Create a new thread</a><br />"
                        output "<a href=\"?action=register\">Register</a><br />"
                        output "<a href=\"?action=login\">Log In</a><br />"
                        output htmlPostamble
                        Effects.pure ()
----------- 
-- Request handling
-----------
handleNonFormRequest : Maybe String -> Maybe Int -> CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
handleNonFormRequest (Just "newthread") Nothing = showNewThreadForm
--handleNonFormRequest (Just "newpost") (Just thread_id) = showNewPostForm thread_id
handleNonFormRequest (Just "showthread") (Just thread_id) = printPosts thread_id
handleNonFormRequest (Just "register") Nothing = showRegisterForm
handleNonFormRequest (Just "login") Nothing = showLoginForm
handleNonFormRequest Nothing _ =  printThreads



-- Hacky, probably best to use the parser
strToInt : String -> Int
strToInt s = cast s

handleRequest : CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
handleRequest = do handler_var <- queryPostVar "handler"
                   -- A better way might be (ifHandlerSet ...)
                   case handler_var of
                     Just _ => do lift (Keep (Drop (Drop (SubNil)))) (handleForm handlers)
                                  Effects.pure ()
                     Nothing => do
                       action <- lift (Keep (Drop (Drop (SubNil)))) (queryGetVar "action")
                       thread_id <- lift (Keep (Drop (Drop (SubNil)))) (queryGetVar "thread_id")
                       handleNonFormRequest action (map strToInt thread_id)

main : IO ()
main = do runCGI [initCGIState, InvalidSession, ()] handleRequest
          pure ()

