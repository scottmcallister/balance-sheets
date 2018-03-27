{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Bootstrap where

import Database.SQLite.Simple as Sql

bootstrapConnection :: Sql.Connection -> IO ()
bootstrapConnection conn = do
    createSchema conn
    populateData conn

createSchema :: Sql.Connection -> IO ()
createSchema conn = do
    executeDB "PRAGMA foreign_keys = ON"
    executeDB "create table user (id integer primary key asc, firstName varchar2(255), firstName varchar2(255))"

    where
        executeDB = Sql.execute_ conn

initialUsers :: [(Int, String, String)]
initialUsers = 
    [ (1, "Scott", "McAllister")
    , (2, "Kelly", "McAllister")
    ]

populateData :: Sql.Connection -> IO ()
populateData conn =
    mapM_ insertUsers initialUsers
    
    where
        insertUsers = Sql.execute conn "insert into user (id, firstName, lastName) values(?, ?, ?)"