{-# LANGUAGE DisambiguateRecordFields #-}

module Dummy (dummyUsers, dummyToken) where

import Data.Text (Text, pack)
import Data.Time
import Model.User

dummyUsers :: [User]
dummyUsers =
  [ User
      { id = 1,
        email = "ilham@example.com",
        password = "password123",
        username = "ilham",
        display_name = "Ilham Elhamdi",
        registered_at = UTCTime (fromGregorian 2021 5 1) (secondsToDiffTime 0),
        modified_at = UTCTime (fromGregorian 2021 5 1) (secondsToDiffTime 0)
      },
    User
      { id = 2,
        email = "john@example.com",
        password = "password123",
        username = "john",
        display_name = "John Smith",
        registered_at = UTCTime (fromGregorian 2021 6 1) (secondsToDiffTime 0),
        modified_at = UTCTime (fromGregorian 2021 6 1) (secondsToDiffTime 0)
      },
    User
      { id = 3,
        email = "jane@example.com",
        password = "password123",
        username = "jane",
        display_name = "Jane Doe",
        registered_at = UTCTime (fromGregorian 2021 7 1) (secondsToDiffTime 0),
        modified_at = UTCTime (fromGregorian 2021 7 1) (secondsToDiffTime 0)
      },
    User
      { id = 4,
        email = "alice@example.com",
        password = "password123",
        username = "alice",
        display_name = "Alice Wonderland",
        registered_at = UTCTime (fromGregorian 2021 8 1) (secondsToDiffTime 0),
        modified_at = UTCTime (fromGregorian 2021 8 1) (secondsToDiffTime 0)
      },
    User
      { id = 5,
        email = "bob@example.com",
        password = "password123",
        username = "bob",
        display_name = "Bob Marley",
        registered_at = UTCTime (fromGregorian 2021 9 1) (secondsToDiffTime 0),
        modified_at = UTCTime (fromGregorian 2021 9 1) (secondsToDiffTime 0)
      }
  ]

dummyToken :: Text
dummyToken = pack "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c"