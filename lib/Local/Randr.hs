module Local.Randr (randrKeys) where

import           XMonad                              (X, io, spawn)
import           Control.Applicative                 ((<$>),(<|>))
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO)
import qualified Data.List                     as List
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as T
import           Text.ParserCombinators.Parsec.Char  (alphaNum)
import           Text.ParserCombinators.Parsec       (Parser
                                                     ,ParseError
                                                     ,GenParser
                                                     ,string
                                                     ,digit
                                                     ,char
                                                     ,anyToken
                                                     ,manyTill
                                                     ,eof
                                                     ,space
                                                     ,spaces
                                                     ,try
                                                     ,lookAhead
                                                     ,many

                                                     )
import           XMonad.Util.Run                     (runProcessWithInput, safeSpawn)
import Data.Maybe
-- width and height

runXRandR :: MonadIO m => m String
runXRandR = runProcessWithInput "xrandr" [] ""

parseXRandR :: String -> Either ParseError [Display]
parseXRandR = P.parse parseDisplays "wat"

data Display = Display
  { name :: String,
    connected :: Bool,
    modes :: [Mode],
    x_offset :: Int
  } deriving Show

data Mode = Mode
  { width :: Int,
    height ::Int,
    active :: Bool,
    auto :: Bool
  } deriving Show

parseDisplays :: Parser [Display]
parseDisplays = do
  string "Screen" >> ignoreLine
  manyTill parseDisplay eof

parseDisplay :: Parser Display
parseDisplay = do
  n <- manyTill anyToken space
  con <- string "connected" <|> string "disconnected"

  offset <- if con == "connected"
            then spaces >> ((many digit >> char 'x' >> many digit >> char '+' >> many digit) <|> (return "0"))
            else return "0"

  ignoreLine

  modes <- if con == "connected"
    then parseModeLines
    else skipModeLines

  return $ Display n (con == "connected") modes (read offset)

parseModeLines :: Parser [Mode]
parseModeLines = manyTill parseModeLine nextDisplay

skipModeLines :: Parser [Mode]
skipModeLines = ignoreLinesTill nextDisplay >> return []

parseModeLine :: Parser Mode
parseModeLine = do
    w <- spaces >> many digit
    h <- char 'x' >> many digit
    rest <- parseLine

    return $ Mode (read w) (read h) ('*' `elem` rest) ('+' `elem` rest)

nextDisplay :: Parser ()
nextDisplay = lookAhead $ try parseDisplay >> return ()

ignoreLinesTill :: Parser () -> Parser ()
ignoreLinesTill p = do
    _ <- manyTill ignoreLine $ p <|> eof
    return ()

parseLine :: Parser String
parseLine = manyTill anyToken eol
 where eol :: Parser Char
       eol = char '\n'

ignoreLine :: Parser ()
ignoreLine = parseLine >> return ()

enableConnected :: [Display] -> Layout
enableConnected ds = map name $ filter connected ds

reverseExisting :: [Display] -> Layout
reverseExisting ds = map name $ List.sortOn (negate . x_offset) $ filter active' ds
  where active' d = (connected d) && (any active $ modes d)

type Layout = [String]

updateLayout :: ([Display] -> Layout) -> X ()
updateLayout lf = do
  co <- runXRandR
  case parseXRandR co of
    (Right ds) -> applyLayout ds $ lf ds
    other -> io $ putStrLn $ show other

preferredSize :: Display -> (Int, Int)
preferredSize (Display {modes = ms}) =
  let pref = filter auto ms
      big = List.sortOn (negate . width) ms
      (Mode {width = w, height = h}) =
        head $ if null pref then (if null big then [Mode 0 0 False False] else big) else pref
  in (w, h)

applyLayout :: [Display] -> Layout -> X ()
applyLayout ds l =
  let (ons', offs) = List.partition (\x -> connected x && (name x) `elem` l) ds
      ons = mapMaybe (\x -> List.find ((== x) . name) ons') l
      sizes = map preferredSize ons
      tallest = maximum $ map snd sizes
      output y x = ["--output", name x, y]
      fmode w h = show w++"x"++show h
      
      mode (d, (w,h)) = ["--output", name d, "--mode", fmode w h]
      position (d, (_, h), x) = ["--output", name d, "--pos", fmode x (tallest - h) ]
      xs = scanl (+) 0 $ map fst sizes
      
      command = concat [ concatMap (output "--off") offs
                       , concatMap mode $ zip ons sizes
                       , concatMap position $ zip3 ons sizes xs
                       ]
  in do io $ putStrLn $ show command
        safeSpawn "xrandr" command

randrKeys =
  [ ("M-q M-d", updateLayout enableConnected)
  , ("M-q d", updateLayout $ reverseExisting)
  ]

-- grammar

-- TODO parse the verbose form (for EDIDs?)
-- TODO save information about configurations
