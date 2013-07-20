module Main
import Cgi
import Session
import SessionUtils
import SQLite

ThreadID : Type
ThreadID = Integer

DB_NAME : String
DB_NAME = "/tmp/messageboard.db"

-- Template system would be nice...
htmlPreamble : String
htmlPreamble = "<html><head><title>IdrisWeb Message Board</title></head><body>"

htmlPostamble : String
htmlPostamble = "</body></html>"

----------- 
-- Post Creation
-----------
handlePost : Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning), 
                                                            SESSION (SessionRes UninitialisedSession), 
                                                            SQLITE ()
                                         ] Bool
handlePost (Just content) = do ...
handlePost _ = do ...

newPostForm : UserForm
newPostForm = do
-- todo: addHidden operation
  addTextBox "Post Content" FormString Nothing
  addSubmit handlePost "handlePost" [CGIEffect, SessionEffect, SQLiteEffect] FormBool


showNewPostForm : CGIProg [SESSION (SessionRes InitialisedSession), SQLITE ()] ()
showNewPostForm = do output "<h2>Create new post</h2>"
                     addForm newPostForm
----------- 
-- Thread Creation
-----------
-- Create a new thread, given the title and content
handleNewThread : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning), 
                                                               SESSION (SessionRes UninitialisedSession), 
                                                               SQLITE ()
                                                              ] Bool
handleNewThread (Just title) (Just content) = do ...
handleNewThread _ _ = do ...


newThreadForm : UserForm
newThreadForm = do
  addTextBox "Title" FormString Nothing
  addTextBox "Post Content" FormString Nothing -- password field would be good
  addSubmit handleNewThread "handleNewThread" [CGIEffect, SessionEffect, SQLiteEffect] FormBool


showNewThreadForm : CGIProg [SESSION (SessionRes InitialisedSession), SQLITE ()] ()
showNewThreadForm = do output htmlPreamble
                       output "<h1>New Thread</h1>"
                       addForm newThreadForm
                       output htmlPostamble


----------- 
-- Registration
-----------
handleRegisterForm : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning),
                                                                  SESSION (SessionRes UninitialisedSession),
                                                                  SQLITE ()
                                                                 ] Bool
handleRegisterForm (Just name) (Just pwd) = do ...
handleRegisterForm _ _ = do ...

registerForm : UserForm
registerForm = do
  addTextBox "Username" FormString Nothing
  addTextBox "Password" FormString Nothing -- password field would be good
  addSubmit handleRegisterForm "handleRegisterForm" [CGIEffect, SessionEffect, SQLiteEffect] FormBool

showRegisterForm : CGIProg [SESSION (SessionRes UninitialisedSession), SQLITE ()] ()
showRegisterForm = do output htmlPreamble
                      output "<h1>Create a new account</h1>"
                      addForm registerForm
                      output "</html>"


----------- 
-- Login 
-----------
handleLoginForm : Maybe String -> Maybe String -> FormHandler [CGI (InitialisedCGI TaskRunning),
                                                                  SESSION (SessionRes UninitialisedSession),
                                                                  SQLITE ()
                                                              ] Bool
handleLoginForm name password = do ...


loginForm : UserForm
loginForm = do
  addTextBox "Username" FormString Nothing
  addTextBox "Password" FormString Nothing -- password field would be good
  addSubmit handleLoginForm "handleLoginForm" [CGIEffect, SessionEffect, SQLiteEffect] FormBool

showLoginForm : CGIProg [SESSION (SessionRes UninitialisedSession), SQLITE ()] ()
showLoginForm = do output htmlPreamble
                      output "<h1>Log in</h1>"
                      addForm loginForm
                      output "</html>"



----------- 
-- Post / Thread Display
-----------

asInt : Integer -> Int
asInt i = fromInteger i

collectPostResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] (List (String, String))
collectPostResults = do
  step_result <- nextRow
  case step_result of
    StepComplete => do name <- getColumnText 1
                       content <- getColumnText 2
                       xs <- collectPostResults
                       pure $ (name, content) :: xs
    NoMoreRows => pure []
    StepFail => pure []

-- Gets the posts
getPosts : Integer -> Eff IO [SQLITE ()] (Either String (List String, String))
getPosts thread_id = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "SELECT `Username`, `Content` FROM `Posts` NATURAL JOIN `Users` WHERE `ThreadID` = ?"
    prep_res <- prepareStatement sql
    if prep_res then do
      startBind
      bindInt 1 (asInt thread_id)
      bind_res <- finishBind
      if finishBind then do
        beginExecution
        results <- collectPostResults
        finaliseStatement
        closeDB 
        pure $ Right results
      else do
        err <- bindFail
        pure $ Left err
    else do
      err <- stmtFail
      pure $ Left err
  else do
    err <- connFail
    pure $ Left err



collectThreadResults : Eff IO [SQLITE (SQLiteRes PreparedStatementExecuting)] (List (String, Int, String))
collectThreadResults = do
  step_result <- nextRow
  case step_result of
    StepComplete => do title <- getColumnText 1
                       uid <- getColumnInt 2
                       username <- getColumnText 3
                       xs <- collectThreadResults
                       pure $ (name, uid, username) :: xs
    NoMoreRows => pure []
    StepFail => pure []

-- Returns (Title, Thread starter ID, Thread starter name)
getThreads : Eff IO [SQLITE ()] (Either String (List (String, Int, String)))
getThreads = do
  open_db <- openDB DB_NAME
  if open_db then do
    let sql = "SELECT `Title`, `UserID`, `Username` FROM `Threads` NATURAL JOIN `Users`"
    prep_res <- prepareStatement sql
    if prep_res then do
      startBind
      finishBind
      beginExecution
      results <- collectThreadResults
      finaliseStatement
      closeDB
      pure $ Right results
    else do
      err <- stmtFail
      pure $ Left err
  else do
    err <- connFail
    pure $ Left err


printPosts : ThreadID -> CGIProg [SESSION (SessionRes UninitialisedSession), SQLITE ()] ()
printPosts thread_id = do 
  post_res <- getPosts thread_id
  case post_res of
    Left err => do output $ "Could not retrieve posts, error: " ++ err                        
                   pure ()
    Right posts = do output "<table>"
                     (traverse (\(name, content) => output $ "<tr><td>" ++ name ++ "</td><td>" ++ content ++ "</td></tr>")
                      posts)
                     output "</table>"
                     pure ()

printThreads : CGIProg [SESSION (SessionRes UninitialisedSession), SQLITE ()] ()
printThreads = do
  thread_res <- getThreads
  case thread_res of
    Left err => do output $ "Could not retrieve threads, error: " ++ err
                   pure ()
    Right threads => do output "<table><th><td>Title</td><td>Author</td>"
                        (traverse (\(title, uid, username) => output $ "<tr><td>" ++ title ++ "</td><td>" ++ username ++ "</td></tr>")
                         posts)
                        output "</table>"
                        pure ()
                    
----------- 
-- Request handling
-----------
handleRequest : CGIProg [SESSION (SessionRes UninitialisedSession), SQLITE ()] ()
handleRequest = do action <- queryGetVar "action"

main : IO ()
main = do runCGI [initCGIState, InvalidSession, ()] handleRequest
          pure ()
