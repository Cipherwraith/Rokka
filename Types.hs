module Types where

data Header = Header {
  getData       :: String
} deriving (Show)


data Input = Input {
  server        :: Maybe String,
  board         :: Maybe String,
  post          :: Maybe String,
  raw           :: Maybe String,
  sid           :: Maybe String,
  start         :: Maybe Int,
  end           :: Maybe Int,
  last          :: Maybe Int,
  keepFirst     :: Maybe Bool,
  remFirst      :: Maybe Bool,
  error         :: Maybe Bool,
  errorMessage  :: Maybe String
} deriving (Show)

