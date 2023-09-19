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
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Coerce
import Data.Foldable
import Data.IntMap
import Data.Maybe
import Data.Sequence
import Effectful hiding ((:>))
import Effectful qualified
import Effectful.Concurrent.MVar
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Network.Wai.Handler.Warp
import Servant

type (||>) = (Effectful.:>)

-- api

proxy :: Proxy API
proxy = Proxy @API

-- effect definitions

-- * mvar business

data SynchronisingState = MkSync
  { readyVar :: MVar (),
    freeMonumentsVar :: MVar (IntMap Monument),
    capturedMonumentsVar :: MVar [(Team, Monument)],
    capturedPoteauxVar :: MVar [(Team, Poteau)],
    activeClaimsVar :: MVar (Seq Claim)
  }

initSyncState :: IO SynchronisingState
initSyncState = runEff . runConcurrent $ do
  readyVar <- newEmptyMVar
  -- read file ?
  freeMonumentsVar <- newMVar mempty
  capturedPoteauxVar <- newMVar mempty
  capturedMonumentsVar <- newMVar mempty
  activeClaimsVar <- newMVar mempty
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

isReadyPage :: (Concurrent ||> es, Reader SynchronisingState ||> es) => Eff es Bool
isReadyPage = do
  ready <- asks readyVar
  not <$> isEmptyMVar ready

updatePage :: (Concurrent ||> es, Reader SynchronisingState ||> es) => Eff es UpdateData
updatePage =
  MkUpdateData
    <$> join (asks (readMVar . capturedMonumentsVar))
    <*> join (asks (readMVar . capturedPoteauxVar))

claimPage :: (Log Claim ||> es, Reader SynchronisingState ||> es, Concurrent ||> es, IOE ||> es) => Claim -> Eff es Bool
claimPage claim = do
  monuments <- readMVar =<< asks freeMonumentsVar
  let isValid
        | ClaimMonument {..} <- claim = isJust (monuments Data.IntMap.!? coerce cMonId)
        | ClaimPoteau {} <- claim = True
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

replLoop :: SynchronisingState -> IO ()
replLoop sync = loop
  where
    loop = do
      input <- getLine
      case words input of
        [x] | x `elem` ["quit", "exit"] -> pure ()
        ["ready"] ->
          MVar.putMVar (readyVar sync) ()
            >> loop
        ["claims"] -> do
          claims <- MVar.readMVar (activeClaimsVar sync)
          traverse_
            (B.putStrLn . encode)
            (Prelude.zip [0 :: Int ..] (Data.Foldable.toList claims))
          loop
        ["validate", n']
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
                      MVar.putMVar (freeMonumentsVar sync) (delete (coerce cMonId) monuments)
                      MVar.modifyMVar_
                        (capturedMonumentsVar sync)
                        (pure . ((cTeam, mon) :))
                    ClaimPoteau {..} -> do
                      let nouveauPoteau = undefined -- FIXME
                      MVar.modifyMVar_
                        (capturedPoteauxVar sync)
                        (pure . (nouveauPoteau :))
                    >> loop
        _ -> putStrLn "Commande inconnue." >> loop

main :: IO ()
main = do
  syncState <- initSyncState
  child <- forkIO (run 8080 (server syncState))
  replLoop syncState
  killThread child
  pure ()
