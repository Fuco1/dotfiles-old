module Utils
       ( subseq
       , matchAllLists
       , matchAllWords
       , getFileName
       , strToLower
       , recompileXMonad
       , (<%>)
       , notifySend
       ) where

import Control.Monad (void)
import Data.Char (toLower)
import Data.List (and, isInfixOf)
import System.Process (readProcess)
import XMonad

subseq :: Eq a => [a] -> [a] -> Bool
[]     `subseq` _      = True
(_:_ ) `subseq` []     = False
(a:as) `subseq` (b:bs) = (if a == b then as else a:as) `subseq` bs

-- | Return true if every list in second argument is a sublist of first.
matchAllLists :: Eq a => [[a]] -> [a] -> Bool
matchAllLists ws w = and (map (flip isInfixOf w) ws)

matchAllWords :: String -> String -> Bool
matchAllWords = matchAllLists . words

getFileName :: String -> String
getFileName = reverse . takeWhile (/= '/') . reverse

strToLower :: String -> String
strToLower = map toLower

recompileXMonad :: X ()
recompileXMonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

infixr 6 <%>
(<%>) :: String -> String -> String
(<%>) = ((++) . (flip (++) " "))

notifySend :: Int -> String -> String -> IO ()
notifySend timeout title msg =
  void $ readProcess "notify-send" ["-u", "low", "-t", show (timeout * 1000), title, msg] ""

-- TODO: add submap support
