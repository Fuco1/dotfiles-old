module PulseAudio
       ( muteSinkInput
       ) where

import Control.Monad.Trans (liftIO)
import Data.List (find)
import System.Process (readProcess)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import XMonad
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

import qualified Constants
import Utils

data PAPrompt = PAPrompt String
instance XPrompt PAPrompt where
    showXPrompt (PAPrompt s) = s ++ ": "

muteSinkInput :: X ()
muteSinkInput = do
  inputs <- liftIO $ getSinkInputs
  case inputs of
    Left x -> liftIO $ notifySend 5 "Error" $ show x
    Right [] -> liftIO $ notifySend 5 "Error" $ "No sinks available."
    Right sinks -> do
      pick <- mkXPromptWithReturn
              (PAPrompt "Mute sink input")
              Constants.prompt
              (compl sinks)
              return
      case pick of
        Just "" -> liftIO $ pacmdMute (head sinks) Toggle
        Just x -> do
          let Just sink = find ((==) x . name) sinks
          liftIO $ pacmdMute sink Toggle
        _ -> return ()
  where compl sinks pick = return $ map name picked
          where picked = filter (matchAllWords pick . strToLower . name) sinks

data MuteCmd = Mute | Unmute | Toggle deriving (Show, Eq)

data SinkInput = SinkInput { index :: Int
                           , name :: String
                           , muted :: Bool } deriving (Show)

-- | index, mute cmd
pacmdMute :: SinkInput -> MuteCmd -> IO ()
pacmdMute sink cmd = do
  let toggle = case cmd of
        Mute   -> 1
        Unmute -> 0
        Toggle -> let m = muted sink in if m then 0 else 1
  safeSpawn "pacmd" ["set-sink-input-mute", show $ index sink, show toggle]

----------------------------------------
-- parsing of the "pacmd" output... blerg
number :: Parser Int
number = do ds <- many1 digit
            return (read ds)
         <?> "number"

welcome :: Parser String
welcome = string "Welcome to PulseAudio! Use \"help\" for usage information.\n>>> "

sinksAvailable :: Parser String
sinksAvailable = string "sink input(s) available.\n"

numberOfSinks :: Parser Int
numberOfSinks = do
  num <- number
  char ' '
  sinksAvailable
  return num

yesno :: Parser Bool
yesno = (string "yes" >> return True) <|>
        (string "no" >> return False)

sink :: Parser SinkInput
sink = do
  manyTill anyChar (try (string "index: "))
  index <- number
  manyTill anyChar (try (string "muted: "))
  muted <- yesno
  manyTill anyChar (try (string "application.name = \""))
  name <- alsaPluginName <|> manyTill anyChar (try (char '"'))
  return $ SinkInput index name muted

alsaPluginName :: Parser String
alsaPluginName = do
  manyTill anyChar (try (char '['))
  name <- manyTill anyChar (try (char ']'))
  manyTill anyChar (try (char '"'))
  return name

sinkInput :: Parser [SinkInput]
sinkInput = do
  welcome
  nos <- numberOfSinks
  if nos == 0
    then return []
    else do
      count nos sink

getSinkInputs :: IO (Either ParseError [SinkInput])
getSinkInputs = do
  output <- runProcessWithInput "pacmd" ["list-sink-inputs"] ""
  return $ parse sinkInput "" output
