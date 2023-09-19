{-# LANGUAGE DataKinds #-}
module API where

import Data.Aeson
import GHC.Natural
import GHC.Generics
import Servant

newtype Id = MkId Int
  deriving (Eq, ToJSON, FromJSON)
newtype Team = MkTeam Natural
  deriving (Eq, ToJSON, FromJSON)
newtype Location = MkLocation (Double, Double)
  deriving (Eq, ToJSON, FromJSON)
newtype Date = MkDate Integer
  deriving (Eq, ToJSON, FromJSON)

data UpdateData = MkUpdateData
  { uMonuments :: [(Team, Monument)]
  , uPoteaux :: [(Team, Poteau)]
  }
  deriving (Generic, Eq)
instance ToJSON UpdateData

data Poteau = MkPoteau 
  { pLocation :: Location
  , pDate :: Date
  , pTeam :: Team
  }
  deriving (Generic, Eq)
instance ToJSON Poteau

data Monument = MkMonument
  { mId :: Id
  , mLocation :: Location
  , mNom :: String
  , mTeam :: Team
  }
  deriving (Generic, Eq)
instance ToJSON Monument

data Claim 
  = ClaimPoteau 
  { cTeam :: Team
  , cLoc :: Location
  , cpNb :: Natural
  , cDate :: Date
  }
  | ClaimMonument
  { cTeam :: Team
  , cMonId :: Id
  , cDate :: Date
  }
  deriving (Generic, Eq)
instance ToJSON Claim
instance FromJSON Claim

type API 
  = "ready" :> Get '[JSON] Bool
  :<|> "update" :> Get '[JSON] UpdateData
  :<|> "claim" :> ReqBody '[JSON] Claim :> Post '[JSON] Bool

