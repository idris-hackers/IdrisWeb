CREATE TABLE Users (
    UserID INTEGER NOT NULL PRIMARY KEY,
    Username TEXT NOT NULL,
    Password TEXT NOT NULL
);

CREATE TABLE Threads (
    ThreadID INTEGER NOT NULL PRIMARY KEY,
    Title TEXT NOT NULL,
    UserID INTEGER NOT NULL references `Users` (`UserID`)
)

CREATE TABLE Posts (
    PostID INTEGER NOT NULL PRIMARY KEY,
    ThreadID INTEGER NOT NULL references `Threads` (`ThreadID`),
    Content TEXT NOT NULL,
    UserID INTEGER NOT NULL references `Users` (`UserID`)
)

