module Dummy (dummyUsers, dummyToken) where

import Data.Time
import Model.User

dummyUsers :: [User]
dummyUsers =
  [ User {email = "ilham@example.com", username = "ilhamelhamdi", password = "password123", name = "Ilham Elhamdi", registered_date = UTCTime (fromGregorian 2021 5 1) (secondsToDiffTime 0)},
    User {email = "john@example.com", username = "johnsmith", password = "password123", name = "John Smith", registered_date = UTCTime (fromGregorian 2021 6 1) (secondsToDiffTime 0)},
    User {email = "jane@example.com", username = "janedoe", password = "password123", name = "Jane Doe", registered_date = UTCTime (fromGregorian 2021 7 1) (secondsToDiffTime 0)},
    User {email = "alice@example.com", username = "alice", password = "password123", name = "Alice Wonderland", registered_date = UTCTime (fromGregorian 2021 8 1) (secondsToDiffTime 0)},
    User {email = "bob@example.com", username = "bobmarley", password = "password123", name = "Bob Marley", registered_date = UTCTime (fromGregorian 2021 9 1) (secondsToDiffTime 0)}
  ]

dummyToken :: String
dummyToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"