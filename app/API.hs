{-# LANGUAGE DataKinds #-}

module API where

import Data.Aeson
import GHC.Generics
import GHC.Natural
import Servant

newtype Id = MkId Int
  deriving (Eq, ToJSON, FromJSON)

newtype Team = MkTeam Natural
  deriving (Eq, ToJSON, FromJSON)

newtype Location = MkLocation (Double, Double)
  deriving (Eq, ToJSON, FromJSON)

newtype Date = MkDate Integer
  deriving (Eq, ToJSON, FromJSON)

data UpdateData
  = NotReady
  | ReadyUpdateData
      { uMonuments :: [(Team, Monument)],
        uPoteaux :: [(Team, Poteau)],
        uStartTime :: Date
      }
  deriving (Generic, Eq)

instance ToJSON UpdateData

data Poteau = MkPoteau
  { pLocation :: Location,
    pDate :: Date,
    pTeam :: Team
  }
  deriving (Generic, Eq)

instance ToJSON Poteau

data Monument = MkMonument
  { mId :: Id,
    mLocation :: Location,
    mNom :: String,
    mTeam :: Team
  }
  deriving (Generic, Eq)

instance ToJSON Monument

instance FromJSON Monument

data Claim
  = ClaimPoteau
      { cTeam :: Team,
        cLoc :: Location,
        cpNb :: Natural,
        cDate :: Date
      }
  | ClaimMonument
      { cTeam :: Team,
        cMonId :: Id,
        cDate :: Date
      }
  deriving (Generic, Eq)

instance ToJSON Claim

instance FromJSON Claim

type API =
  "ready" :> Post '[JSON] Bool
    :<|> "update" :> Get '[JSON] UpdateData
    :<|> "claim" :> ReqBody '[JSON] Claim :> Post '[JSON] Bool
