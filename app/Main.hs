{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import API
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar qualified as MVar
import Control.Monad
import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Coerce
import Data.Foldable
import Data.IntMap
import Data.Maybe
import Data.Sequence
import Data.Time
import Data.Time.Clock.POSIX
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Concurrent.MVar
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Reader.Static
import GHC.Natural
import Network.Wai.Handler.Warp
import Servant
import System.IO

type (||>) = (Effectful.:>)

-- api

proxy :: Proxy API
proxy = Proxy @API

-- effect definitions

-- * mvar business

data SynchronisingState = MkSync
  { readyVar :: MVar Date,
    freeMonumentsVar :: MVar (IntMap Monument),
    capturedMonumentsVar :: MVar [(Team, Monument)],
    capturedPoteauxVar :: MVar [(Team, Poteau)],
    activeClaimsVar :: MVar (Seq Claim)
  }

initSyncState :: IO SynchronisingState
initSyncState = do
  readyVar <- MVar.newEmptyMVar
  monuments <-
    fromMaybe [] <$> do
      monExists <- runEff (runFileSystem (doesFileExist "monuments.json"))
      if monExists
        then decode <$> B.readFile "monuments.json"
        else pure Nothing
  freeMonumentsVar <- MVar.newMVar (Data.IntMap.fromList [(coerce mId m, m) | m <- monuments])
  capturedPoteauxVar <- MVar.newMVar mempty
  capturedMonumentsVar <- MVar.newMVar mempty
  activeClaimsVar <- MVar.newMVar mempty
  pure MkSync {..}

data Log s :: Effect where
  Log :: s -> Log s m ()

type instance DispatchOf (Log s) = Dynamic

runClaimLogger ::
  (Concurrent ||> es, Reader SynchronisingState ||> es) =>
  Eff (Log Claim ': es) a ->
  Eff es a
runClaimLogger = interpret $ \_ -> \case
  Log c -> do
    claims <- asks activeClaimsVar
    modifyMVar_ claims (pure . (|> c))

-- main effects

type App =
  Eff
    '[ Log Claim,
       Reader SynchronisingState,
       Concurrent,
       Error ServerError,
       IOE
     ]

naturalTransformation :: SynchronisingState -> App a -> Handler a
naturalTransformation s =
  Handler
    . ExceptT
    . runEff
    . runErrorNoCallStack
    . runConcurrent
    . runReader s
    . runClaimLogger

-- implementation

currentDate :: IO Date
currentDate = MkDate . truncate <$> getPOSIXTime

isReadyPage :: (Concurrent ||> es, Reader SynchronisingState ||> es, IOE ||> es) => Eff es Bool
isReadyPage = do
  ready <- asks readyVar
  putMVar ready =<< liftIO currentDate
  not <$> isEmptyMVar ready

updatePage :: (Concurrent ||> es, Reader SynchronisingState ||> es) => Eff es UpdateData
updatePage = do
  isReady <- asks readyVar >>= fmap not . isEmptyMVar
  monuments <- readMVar =<< asks capturedMonumentsVar
  poteaux <- readMVar =<< asks capturedPoteauxVar
  date <-
    if isReady
      then join (asks (readMVar . readyVar))
      else pure (coerce @Integer 0)
  pure $
    ReadyUpdateData
      monuments
      poteaux
      (scores monuments poteaux)
      date
      isReady
  where
    scores :: a -> [(Team, Poteau)] -> [(Team, Natural)]
    scores _mon pot =
      fmap (first (coerce @Natural . fromIntegral))
        . Data.IntMap.toList
        $ Data.Foldable.foldl'
          (Data.IntMap.unionWith (+))
          mempty
          (potScore <$> pot)

    potScore :: (Team, Poteau) -> IntMap Natural
    potScore (t, MkPoteau {..}) = Data.IntMap.singleton (fromEnum t) pNb

claimPage :: (Log Claim ||> es, Reader SynchronisingState ||> es, Concurrent ||> es, IOE ||> es) => Claim -> Eff es Bool
claimPage claim = do
  isReady <- fmap not . isEmptyMVar =<< asks readyVar
  monuments <- readMVar =<< asks freeMonumentsVar
  let isValid
        | ClaimMonument {..} <- claim =
            isReady
              && isJust (monuments Data.IntMap.!? coerce cMonId)
              && cTeam == mTeam (monuments Data.IntMap.! coerce cMonId)
        | ClaimPoteau {} <- claim = isReady
  when isValid $ do
    send (Log claim)
  pure isValid

-- runtime

server :: SynchronisingState -> Application
server s =
  serve proxy . hoistServer proxy (naturalTransformation s) $
    isReadyPage
      :<|> updatePage
      :<|> claimPage

move :: (t -> Bool) -> (t -> a) -> MVar [t] -> MVar [a] -> IO ()
move p f aVar bVar = do
  x <- head . Prelude.filter p <$> MVar.readMVar aVar
  MVar.modifyMVar_ aVar (pure . Prelude.filter (not . p))
  MVar.modifyMVar_ bVar (pure . (f x :))

printClaim :: Int -> Claim -> IO ()
printClaim k (ClaimMonument t i d) = do
  putStrLn (show k <> "- Monument Claim by Team " <> show (coerce @Team @Natural t))
  putStrLn ("--- Monument #" <> show (coerce @Id @Int i))
  putStrLn ("--- " <> formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime (fromInteger (coerce d))))
printClaim k (ClaimPoteau t l n d) = do
  putStrLn (show k <> "- Poteau Claim by Team " <> show (coerce @Team @Natural t))
  putStrLn ("--- Claim of " <> show n <> " poteaux")
  putStrLn ("--- Location" <> show (coerce @Location @(Double, Double) l))
  putStrLn ("--- " <> formatTime defaultTimeLocale "%H:%M:%S" (posixSecondsToUTCTime (fromInteger (coerce d))))

replLoop :: SynchronisingState -> IO ()
replLoop sync = loop
  where
    loop = do
      putStr "> "
      hFlush stdout
      input <- getLine
      case words input of
        [x] | x `elem` ["quit", "exit"] -> pure ()
        ["clear"] -> do
          putStr "\ESC[2J"
          hFlush stdout
          loop
        ["claims"] -> do
          claims <- MVar.readMVar (activeClaimsVar sync)
          traverse_
            (uncurry printClaim)
            (Prelude.zip [0 :: Int ..] (Data.Foldable.toList claims))
          loop
        ["reject", n']
          | all (`elem` ['0' .. '9']) n' -> do
              let n = read @Int n'
              MVar.modifyMVar_
                (activeClaimsVar sync)
                (pure . deleteAt n)
              loop
        ["accept", n']
          | all (`elem` ['0' .. '9']) n' -> do
              let n = read @Int n'
              claims <- MVar.readMVar (activeClaimsVar sync)
              let claim' = claims Data.Sequence.!? n
              case claim' of
                Nothing -> putStrLn "mauvais index boulet."
                Just claim -> do
                  MVar.modifyMVar_
                    (activeClaimsVar sync)
                    (pure . Data.Sequence.filter (/= claim))
                  case claim of
                    ClaimMonument {..} -> do
                      monuments <- MVar.readMVar (freeMonumentsVar sync)
                      let mon = monuments Data.IntMap.! (coerce cMonId)
                      MVar.modifyMVar_ (freeMonumentsVar sync) (pure . delete (coerce cMonId))
                      MVar.modifyMVar_
                        (capturedMonumentsVar sync)
                        (pure . ((cTeam, mon) :))
                    ClaimPoteau {..} -> do
                      date <- currentDate
                      let nouveauPoteau = MkPoteau cLoc date cTeam cpNb
                      MVar.modifyMVar_
                        (capturedPoteauxVar sync)
                        (pure . ((cTeam, nouveauPoteau) :))
              loop
        _ -> putStrLn "Commande inconnue." >> loop

main :: IO ()
main = do
  syncState <- initSyncState
  child <- forkIO (run 8080 (server syncState))
  replLoop syncState
  killThread child
  pure ()
