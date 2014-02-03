module Main
import Effects
import IdrisWeb.CGI.Cgi
import IdrisWeb.Session.Session
import IdrisWeb.Session.SessionUtils
import IdrisWeb.DB.SQLite.SQLiteNew

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
  conn_res <- openDB DB_NAME
  if_valid then do
    let sql = "INSERT INTO `Posts` (`UserID`, `ThreadID`, `Content`) VALUES (?, ?, ?)"
    ps_res <- prepareStatement sql
    if_valid then do
      bindInt 1 uid
      bindInt 2 thread_id
      bindText 3 content
      bind_res <- finishBind
      if_valid then do
        executeStatement
        finalise
        closeDB
        return True
      else do
        cleanupBindFail
        return  False
    else do
      cleanupPSFail
      return False
  else 
    return False



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
                            return ()
                          else do
                            outputWithPreamble "There was an error adding the post to the database."
                            discardSession
                            return ()
    Nothing => do notLoggedIn
                  return ()
                         


handlePost (Just thread_id) (Just content) = do withSession (addPostToDB thread_id content) notLoggedIn
                                                pure ()
handlePost _ _ = do outputWithPreamble"<h1>Error</h1><br />There was an error processing your post."
                    pure ()

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
  addForm (newPostForm thread_id)
  output htmlPostamble

----------- 
-- Thread Creation
-----------

threadInsert : Int -> String -> String -> Eff IO [SQLITE ()] (Maybe QueryError)
threadInsert uid title content = do
  let query = "INSERT INTO `Threads` (`UserID`, `Title`) VALUES (?, ?)"
  insert_res <- executeInsert DB_NAME query [(1, DBInt uid), (2, DBText title)]
  case insert_res of
    Left err => return (Just err)
    Right thread_id => do 
      post_res <- postInsert uid thread_id content
      if post_res then return Nothing else return $ Just (ExecError "post")
                          

addNewThread : String -> String -> SessionData -> EffM IO [CGI (InitialisedCGI TaskRunning),
                                                       SESSION (SessionRes SessionInitialised),
                                                       SQLITE ()]
                                                      [CGI (InitialisedCGI TaskRunning),
                                                       SESSION (SessionRes SessionUninitialised),
                                                       SQLITE ()] ()
addNewThread title content sd = do 
  case lookup USERID_VAR sd of
    Just (SInt uid) => 
      do insert_res <- threadInsert uid title content
         case insert_res of
           Just err => do
             output $ "There was an error adding the thread to the database: " ++ show err
             discardSession
             return ()
           Nothing => do
             output "Thread added successfully"
             discardSession
             return ()
    Nothing => do notLoggedIn
                  return ()
  
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
                       addForm newThreadForm
                       output htmlPostamble


----------- 
-- Registration
-----------

insertUser : String -> String -> Eff IO [SQLITE ()] (Either QueryError Int)
insertUser name pwd = executeInsert DB_NAME query bind_vals
  where query = "INSERT INTO `Users` (`Username`, `Password`) VALUES (?, ?)"
        bind_vals = [(1, DBText name), (2, DBText pwd)]


userExists' : EffM IO [SQLITE (Either (SQLiteExecuting InvalidRow) (SQLiteExecuting ValidRow))] 
              [SQLITE ()] Bool
userExists' =
  if_valid then do
    finaliseValid
    closeDB
    return True
  else do
    finaliseInvalid
    closeDB
    return False


userExists : String -> Eff IO [SQLITE ()] (Either QueryError Bool)
userExists username = do
  conn_res <- openDB DB_NAME
  if_valid then do
    let sql = "SELECT * FROM `Users` WHERE `Username` = ?"
    ps_res <- prepareStatement sql
    if_valid then do
      bindText 1 username
      bind_res <- finishBind
      if_valid then do
        executeStatement
        res <- userExists'  
        return $ Right res
      else do
        let be = getBindError bind_res
        cleanupBindFail
        return $ Left be
    else do
      cleanupPSFail
      return $ Left (getQueryError ps_res)
  else 
    return $ Left (getQueryError conn_res)


handleRegisterForm (Just name) (Just pwd) = do 
  user_exists_res <- userExists name
  case user_exists_res of
    Left err => do outputWithPreamble "Error checking for user existence"
                   pure ()
    Right user_exists => 
      if (not user_exists) then do 
        insert_res <- insertUser name pwd
        case insert_res of
          Left err => do outputWithPreamble ("Error inserting new user" ++ (show err))
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
                      addForm registerForm
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
authUser' : EffM IO [SQLITE (Either (SQLiteExecuting InvalidRow) 
                                    (SQLiteExecuting ValidRow))] 
                    [SQLITE ()] 
                    (Either QueryError (Maybe UserID))
authUser' = 
  if_valid then do
    user_id <- getColumnInt 0
    finaliseValid
    closeDB
    return $ Right (Just user_id)
  else do
    finaliseInvalid
    closeDB
    return $ Right Nothing

authUser : String -> String -> Eff IO [SQLITE ()] (Either QueryError (Maybe UserID))
authUser username password = do
  conn_res <- openDB DB_NAME
  if_valid then do
    let sql = "SELECT `UserID` FROM `Users` WHERE `Username` = ? AND `Password` = ?"
    ps_res <- prepareStatement sql
    if_valid then do
      bindText 1 username
      bindText 2 password 
      bind_res <- finishBind
      if_valid then do
        executeStatement 
        authUser' 
      else do
        let be = getBindError bind_res
        cleanupBindFail
        return $ Left be
    else do
      cleanupPSFail
      return $ Left (getQueryError ps_res)
  else 
    return $ Left (getQueryError conn_res)


setSession : UserID -> Eff IO [CGI (InitialisedCGI TaskRunning), SESSION (SessionRes SessionUninitialised), SQLITE ()] Bool
setSession user_id = do
  create_res <- createSession [(USERID_VAR, SInt user_id)]
  sess_res <- lift' setSessionCookie
  db_res <- lift' writeSessionToDB

  pure (sess_res && db_res)


handleLoginForm (Just name) (Just pwd) = do
  auth_res <- authUser name pwd
  case auth_res of
    Right (Just uid) => do
      set_sess_res <- setSession uid
      if set_sess_res then do
        output $ "Welcome, " ++ name
        pure ()
      else do
        output "Could not set session"
        pure ()
    Right Nothing => do
      output "Invalid username or password"
      pure ()
    Left err => do
      output $ "Error: " ++ (show err)
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
                   addForm loginForm
                   output "</html>"



----------- 
-- Post / Thread Display
-----------

collectPostResults : Eff IO [SQLITE (SQLiteExecuting ValidRow)] (List DBVal) -- (List (String, String))
collectPostResults = do name <- getColumnText 0
                        content <- getColumnText 1
                        pure [DBText name, DBText content]
-- Gets the posts
getPosts : Int -> Eff IO [SQLITE ()] (Either QueryError ResultSet)
getPosts thread_id =
  executeSelect DB_NAME query bind_vals collectPostResults
 where query = "SELECT `Username`, `Content` FROM `Posts` NATURAL JOIN `Users` WHERE `ThreadID` = ?"
       bind_vals = [(1, DBInt thread_id)]  


collectThreadResults : Eff IO [SQLITE (SQLiteExecuting ValidRow)] (List DBVal) 
collectThreadResults = do thread_id <- getColumnInt 0
                          title <- getColumnText 1
                          uid <- getColumnInt 2
                          username <- getColumnText 3
                          pure [DBInt thread_id, DBText title, DBInt uid, DBText username]

-- Returns (Title, Thread starter ID, Thread starter name)
getThreads : Eff IO [SQLITE ()] (Either QueryError ResultSet)
getThreads = executeSelect DB_NAME query [] collectThreadResults
  where query = "SELECT `ThreadID`, `Title`, `UserID`, `Username` FROM `Threads` NATURAL JOIN `Users`"


traversePosts : ResultSet -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
traversePosts [] = pure ()
traversePosts (x :: xs) = do traverseRow x 
                             traversePosts xs
  where traverseRow : List DBVal -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
        traverseRow ((DBText name)::(DBText content)::[]) = output $ "<tr><td>" ++ name ++ "</td><td>" ++ content ++ "</td></tr>"
        traverseRow _ = pure () -- invalid row, discard

printPosts : ThreadID -> CGIProg [SQLITE ()] ()
printPosts thread_id = do 
  post_res <- getPosts thread_id
  case post_res of
    Left err => do output $ "Could not retrieve posts, error: " ++ (show err)
                   return ()
    Right posts => do output "<table>"
                      traversePosts posts
                      output "</table>"
                      output $ "<a href=\"?action=newpost&thread_id=" ++ (show thread_id) ++ "\">New post</a><br />"
                      return ()

traverseThreads : ResultSet -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
traverseThreads [] = pure ()
traverseThreads (x::xs) = do traverseRow x
                             traverseThreads xs
  where traverseRow : List DBVal -> Eff IO [CGI (InitialisedCGI TaskRunning)] ()
        traverseRow ((DBInt thread_id)::(DBText title)::(DBInt user_id)::(DBText username)::[]) =
           (output $ "<tr><td><a href=\"?action=showthread&thread_id=" ++ 
            (show thread_id) ++ "\">" ++ title ++ "</a></td><td>" ++ username ++ "</td></tr>") 
        traverseRow _ = pure ()

printThreads : CGIProg [SQLITE ()] ()
printThreads = do
  thread_res <- getThreads
  case thread_res of
    Left err => do output $ "Could not retrieve threads, error: " ++ (show err)
                   return ()
    Right threads => do output htmlPreamble
                        output "<table><tr><th>Title</th><th>Author</th></tr>"
                        traverseThreads threads
                        output "</table><br />"
                        output "<a href=\"?action=newthread\">Create a new thread</a><br />"
                        output "<a href=\"?action=register\">Register</a><br />"
                        output "<a href=\"?action=login\">Log In</a><br />"
                        output htmlPostamble
                        return ()
----------- 
-- Request handling
-----------
handleNonFormRequest : Maybe String -> Maybe Int -> CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
handleNonFormRequest (Just "newthread") Nothing = showNewThreadForm
handleNonFormRequest (Just "newpost") (Just thread_id) = showNewPostForm thread_id
handleNonFormRequest (Just "showthread") (Just thread_id) = printPosts thread_id
handleNonFormRequest (Just "register") Nothing = showRegisterForm
handleNonFormRequest (Just "login") Nothing = showLoginForm
handleNonFormRequest Nothing _ =  printThreads



-- Hacky, probably best to use the parser
strToInt : String -> Int
strToInt s = cast s

handleRequest : CGIProg [SESSION (SessionRes SessionUninitialised), SQLITE ()] ()
handleRequest = do handler_set <- isHandlerSet
                   if handler_set then do
                     handleForm handlers
                     return ()
                   else do
                     action <- queryGetVar "action"
                     thread_id <- queryGetVar "thread_id"
                     handleNonFormRequest action (map strToInt thread_id)

main : IO ()
main = do runCGI [initCGIState, InvalidSession, ()] handleRequest
          pure ()

