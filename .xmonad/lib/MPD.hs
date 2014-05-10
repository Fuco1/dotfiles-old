{-# LANGUAGE OverloadedStrings #-}

module MPD
       ( playPlaylist
       , playArtist
       , playDirectory
       , playTrack
       , Action(..)
       , deleteCurrent
       , MPD.clear
       , MPD.next
       , MPD.previous
       , MPD.stop
       , MPD.toggle
       ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad (when, void, mapM_, (<=<), (>=>), ap, liftM)
import Control.Monad.Trans (liftIO, lift)
import Data.Function (on)
import Data.List (elemIndex, isInfixOf, groupBy)
import Data.Maybe (fromJust, isJust)
import Data.Map (lookup)
import Network.MPD as M
import Network.MPD.Commands.Extensions (addMany, listArtists, getPlaylist)
import XMonad hiding ((=?))
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)
import Prelude hiding (lookup)

import qualified Data.ByteString.Char8 as C

import qualified Constants
import Utils

data MPDPrompt = MPDPrompt String
instance XPrompt MPDPrompt where
    showXPrompt (MPDPrompt s) = s ++ ": "

data Action = Clear | Add deriving (Show, Eq)

playPlaylist :: Action -> X ()
playPlaylist = playGeneric
               getPlaylists
               "playlist"
               (load . PlaylistName . C.pack)

playArtist :: Action -> X ()
playArtist = playGeneric
             listArtists
             "artist"
             (\x -> findAdd (Artist =? (mkValue x)))

playGeneric :: ToString a => MPD [a] -> String -> (String -> MPD ()) -> Action -> X ()
playGeneric getter prompt mpd action = do
  Right choices <- liftMPD getter
  Just pick <- askPrompt
               ((if (action == Clear) then "Play " else "Add ") ++ prompt)
               . map toString $ choices
  liftMPD_ $ do
    when (action == Clear) M.clear
    mpd pick
    when (action == Clear) $ play Nothing

playDirectory :: Action -> X ()
playDirectory action = do
  Just dir <- listDirectorySongs ""
  liftMPD_ $ do
    when (action == Clear) M.clear
    songs <- getSongs dir
    addMany "" songs
    when (action == Clear) $ play Nothing

playTrack :: X ()
playTrack = do
  Right choices <- liftMPD $ playlistInfo Nothing
  mkXPrompt
    (MPDPrompt "Jump to track")
    Constants.prompt
    (trackCompl choices)
    (action . (read :: String -> Int) . takeWhile (/=':'))
  where action = liftMPD_ . play . Just

trackCompl :: [Song] -> String -> IO [String]
trackCompl choices pick = return mapped
  where picked = filter (isInfixOf pick . strToLower . getName) choices
        grouped = groupBy ((==) `on` getName) picked
        mapped = grouped >>= (\x -> case x of
                                 [a] -> [indexify a]
                                 xs  -> map (\v -> indexify v ++ " [" ++ getAlbum v ++ "]") xs)
        getName (Song { sgFilePath = Path path
                      , sgTags = tags })
          = case lookup Title tags of
              Just ([Value x]) -> C.unpack x
              Nothing          -> C.unpack path
        getAlbum (Song { sgTags = tags })
          = case lookup Album tags of
              Just ([Value x]) -> C.unpack x
              Nothing          -> ""
        getIndex (Song { sgIndex = Just index }) = show index
        indexify = uncurry (++) . (flip (++) ": " . getIndex &&& getName)

---------------------------------------- getting data
askPrompt :: String -> [String] -> X (Maybe String)
askPrompt name choices =
  mkXPromptWithReturn
    (MPDPrompt name)
    Constants.prompt
    (return . (flip filter) choices . (isInfixOf `on` strToLower))
    return

listDirectorySongs :: String -> X (Maybe String)
listDirectorySongs path = do
  d <- liftMPD $ liftM (map (\(Path x) -> getFileName . C.unpack $ x)) . getDirectories $ path
  case d of
    Left _     -> return . Just $ path
    Right []   -> return . Just $ path
    Right dirs -> do
      picked <- askPrompt "Find directory" dirs
      case picked of
        Nothing   -> return Nothing
        Just ""   -> return . Just $ path
        Just pick -> do
          let new = if path == "" then pick else path ++ "/" ++ pick
          listDirectorySongs new

getDirectories :: String -> MPD [Path]
getDirectories = liftM getDirsFromResults . lsInfo . mkPath

getPlaylists :: MPD [PlaylistName]
getPlaylists = liftM getPlaylistsFromResults . lsInfo . mkPath $ ""

getSongs :: String -> MPD [Path]
getSongs path = do
  songs <- liftM getSongsFromResults . lsInfo . mkPath $ path
  return $ [x | Song {sgFilePath = x} <- songs]

---------------------------------------- projections from results
getSongsFromResults :: [LsResult] -> [Song]
getSongsFromResults r = [x | (LsSong x) <- r]

getDirsFromResults :: [LsResult] -> [Path]
getDirsFromResults r = [x | (LsDirectory x) <- r]

getPlaylistsFromResults :: [LsResult] -> [PlaylistName]
getPlaylistsFromResults r = [x | (LsPlaylist x) <- r]

---------------------------------------- other utils
mkPath :: String -> Path
mkPath = Path . C.pack

mkValue :: String -> Value
mkValue = Value . C.pack

liftMPD :: MonadIO m => MPD a -> m (Response a)
liftMPD = liftIO . withMPD

liftMPD_ :: MonadIO m => MPD a -> m ()
liftMPD_ = liftM (const ()) . liftMPD

---------------------------------------- playlist control

deleteCurrent :: X ()
deleteCurrent = liftMPD_ $ do
  Just (Song { sgIndex = Just index }) <- currentSong
  delete index

clear :: X ()
clear = liftMPD_ M.clear

next :: X ()
next = liftMPD_ M.next

previous :: X ()
previous = liftMPD_ M.previous

stop :: X ()
stop = liftMPD_ M.stop

toggle :: X ()
toggle = liftMPD_ $ do
  Status { stState = s } <- status
  case s of
    Playing -> pause True
    Paused  -> play Nothing
    Stopped -> play Nothing
