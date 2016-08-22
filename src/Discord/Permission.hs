module Discord.Permission
    ( Permission (..)
    , Role (..)
    ) where


import           Data.Aeson


data Permission = Permission
  { perId          :: !String
  , perName        :: !String
  , perHoist       :: !Bool
  , perPosition    :: !Int
  , perPermissions :: !Int
  , perManaged     :: !Bool
  , perMentionable :: !Bool
  }
  deriving (Show)

-- todo JSON

data Role = Role
  { rId          :: !String
  , rName        :: !String
  , rColor       :: !Int
  , rHoist       :: !Bool
  , rPosition    :: !Int
  , rPermissions :: !Int
  , rManaged     :: !Bool
  , rMentionable :: !Bool
  }
  deriving (Show)

-- todo Json
