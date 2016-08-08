module Discord.Channel
    ( Channel (..)
    , Message (..)
    , Overwrite (..)
    , Embed (..)
    , Thumbnail (..)
    , Provider (..)
    , Attachment (..)
    ) where


import           Data.Aeson
import           Discord.Permission
import           Discord.User


data Channel
  = GuildChannel
    { gcId            :: !String
    , gcGuildId       :: !String
    , gcName          :: !String
    , gcType          :: !String
    , gcPosition      :: !Int
    , gcIsPrivate     :: !Bool
    , gcOverwrite     :: ![Overwrite]
    , gcTopic         :: !(Maybe String)
    , gcLastMessageId :: !(Maybe String)
    , gcBitrate       :: !(Maybe Int)
    , gcUserLimit     :: !(Maybe Int)
    }
  | DMChannel
    { dmcId            :: !String
    , dmcIsPrivate     :: !Bool
    , dmcRecipient     :: !User
    , dmcLastMessageId :: !String
    }
  deriving (Show)

-- todo Json
-- todo Json


data Message = Message
  { mId              :: !String
  , mChannelId       :: !String
  , mAuthor          :: !User
  , mContent         :: !String
  , mTimestamp       :: !String         -- fixme Timestamp type
  , mEditedTimestamp :: !(Maybe String) -- fixme Timestamp type
  , mTts             :: !Bool
  , mMentionEveryone :: !Bool
  , mMentions        :: ![User]
  , mMentionRoles    :: ![Role]
  , mAttachments     :: ![Attachment]
  , mEmbeds          :: ![Embed]
  , mNonce           :: !(Maybe Int)
  , mPinned          :: !Bool
  }
  deriving (Show)

-- todo Json

data Overwrite = Overwrite
  { oId    :: !String
  , oType  :: !String
  , oAllow :: !Int
  , oDeny  :: !Int
  }
  deriving (Show)

-- todo Json

data Embed = Embed
  { eTitle       :: !String
  , eType        :: !String
  , eDescription :: !String
  , eUrl         :: !String
  , eThumbnail   :: !Thumbnail
  , eProvider    :: !Provider
  }
  deriving (Show)

-- todo Json

data Thumbnail = Thumbnail
  { thUrl      :: !String
  , thProxyUrl :: !String
  , thHeight   :: !Int
  , thWidth    :: !Int
  }
  deriving (Show)

-- todo Json

data Provider = Provider
  { pName :: !String
  , pUrl  :: !String
  }
  deriving (Show)

-- todo Json

data Attachment = Attachment
  { attId       :: !String
  , attFilename :: !String
  , attSize     :: !Int
  , attUrl      :: !String
  , attProxyUrl :: !String
  , attHeight   :: !(Maybe Int)
  , attWidth    :: !(Maybe Int)
  }
  deriving (Show)

-- todo Json



-- API endpoints for Channels


--
