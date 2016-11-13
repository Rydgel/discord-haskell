module Discord.Types.User
    ( User(..)
    , UserGuild(..)
    , ConnectionObject(..)
    ) where


import           Data.Aeson
import           Discord.Types.Guild

data User = User
  { uId            :: !String
  , uUsername      :: !String
  , uDiscriminator :: !String
  , uAvatar        :: !String
  , uBot           :: !Bool
  , uMfaEnabled    :: !Bool
  , uVerified      :: !Bool
  , uEmail         :: !String
  }
  deriving (Show)

-- todo Json

data UserGuild = UserGuild
  { ugId         :: !String
  , uName        :: !String
  , uIcon        :: !String
  , uOwner       :: !Bool
  , uPermissions :: !Int
  }
  deriving (Show)

-- todo Json
type IntegrationObject = String
data ConnectionObject = ConnectionObject
  { coId           :: !String
  , coName         :: !String
  , coType         :: !String
  , coRevoked      :: !Bool
  , coIntegrations :: ![IntegrationObject]
  }
  deriving (Show)

-- todo Json
