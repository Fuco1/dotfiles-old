module MPDPrompts (
  playPlaylist
  , playArtist
  ) where

import Control.Applicative ((<$>))
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
