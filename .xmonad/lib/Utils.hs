module Utils
       ( subseq
       , getFileName
       , strToLower
       , recompileXMonad
       ) where

import Data.Char (toLower)
import XMonad (spawn, X (..))

subseq :: Eq a => [a] -> [a] -> Bool
[]     `subseq` _      = True
(_:_ ) `subseq` []     = False
(a:as) `subseq` (b:bs) = (if a == b then as else a:as) `subseq` bs

getFileName :: String -> String
getFileName = reverse . takeWhile (/= '/') . reverse

strToLower :: String -> String
strToLower = map toLower

recompileXMonad :: X ()
recompileXMonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
