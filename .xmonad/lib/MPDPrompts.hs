module MPDPrompts (
  playPlaylist
  , playArtist
  , playTrack
  ) where

import Control.Applicative ((<$>))
import Data.List (elemIndex, isInfixOf)
import Data.Maybe (fromJust)
import XMonad
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)

import qualified Constants
import Utils

data MPDPrompt = MPDPrompt String
instance XPrompt MPDPrompt where
    showXPrompt (MPDPrompt s) = s ++ ": "

playPlaylist :: X ()
playPlaylist = mpdPrompt
               "Play playlist"
               ["lsplaylists"]
               (\x -> runMPCSequence [ ["clear"]
                                      , ["load", x]
                                      , ["toggle"]
                                      ])

playArtist :: X ()
playArtist = mpdPrompt
             "Play artist"
             ["list", "Artist"]
             (\x -> runMPCSequence [ ["clear"]
                                   , ["findadd", "Artist", x]
                                   , ["toggle"]
                                   ])

playTrack :: X ()
playTrack = mkXPrompt
            (MPDPrompt "Play track")
            Constants.prompt
            (\x -> filter (isInfixOf (strToLower x) . strToLower) <$> tracks)
            (\x -> do tr <- liftIO tracks
                      let t = (fromJust . elemIndex x) tr
                      runProcessWithInput "mpc" ["play", show (1 + t)] ""
                      return ())
  where tracks = lines <$> runProcessWithInput "mpc" query ""
        query = ["-f", "%title%", "playlist"]

mpdPrompt :: String -> [String] -> (String -> X ()) -> X ()
mpdPrompt prompt query action = mkXPrompt
                                (MPDPrompt prompt)
                                Constants.prompt
                                (getMPCCompl query)
                                action


runMPCSequence :: [[String]] -> X ()
runMPCSequence [] = error "Empty commands sequence"
runMPCSequence cmds = mapM_ (flip (runProcessWithInput "mpc") "") cmds

getMPCCompl :: [String] -> String -> IO [String]
getMPCCompl query s = filter (subseq (strToLower s) . strToLower) <$> options
  where options = lines <$> runProcessWithInput "mpc" query ""
