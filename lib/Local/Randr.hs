module Local.Randr (randrKeys) where

import           XMonad                              (X, io)
import           Control.Applicative                 ((<$>),(<|>))
import           Control.Monad                       (void)
import           Control.Monad.IO.Class              (MonadIO)
import qualified Data.List                     as List
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as T
import           Text.ParserCombinators.Parsec.Char  (alphaNum)
import           Text.ParserCombinators.Parsec       (Parser
                                                     ,ParseError
                                                     ,GenParser)
import           XMonad.Util.Run                     (runProcessWithInput)

-- width and height

runXRandR :: MonadIO m => m String
runXRandR = runProcessWithInput "xrandr" [] ""

parseXRandR :: String -> Either ParseError [Display]
parseXRandR = P.parse xrandrOut "wat"

enableOne :: X ()
enableOne = do
  co <- runXRandR
  -- now decide what to enable
  case parseXRandR co of
    (Right ds) -> io $ putStrLn $ show ds
    what -> io$putStrLn$show what

  io $ putStrLn $ show $ P.parse rate "err" "60.06*+"

  return ()

enableMany :: X ()
enableMany = return ()

randrKeys =
  [ ("M-d M-d", enableOne)
  , ("M-d M-a", enableMany) ]

-- grammar

data Display = Display { name :: String, conn :: Bool, ms :: [Mode] }
  deriving Show

data Mode = Mode { res :: (Int, Int), rates :: [(String, Bool, Bool)] }
  deriving Show

restOfLine :: Parser ()
restOfLine = do upToLine
                P.newline
                return ()

upToLine :: Parser ()
upToLine = P.many (P.noneOf "\n") >> return ()

connection :: Parser Bool
connection = P.choice [P.string "connected"    >> return True,
                       P.string "disconnected" >> return False]

xrandrOut :: Parser [Display]
xrandrOut = do
  P.string "Screen "
  restOfLine
  d <- display `P.sepEndBy` P.newline
  return d

display :: Parser Display
display = do
  name <- P.many1 P.alphaNum
  P.space
  conn <- connection

  restOfLine

  ms <- modes

  return $ Display name conn ms

modes :: Parser [Mode]
modes = (mode `P.sepEndBy` P.newline) <|> return []

spaces = P.many1 $ P.char ' '

mode :: Parser Mode
mode = do P.string "   "
          x <- P.many1 P.digit
          P.char 'x'
          y <- P.many1 P.digit
          i <- (P.char 'i' >> return True) <|> (return False)

          spaces

          rs <- rates_

          upToLine
          return $ Mode (read x,read y) rs

rates_ = (rate `P.sepEndBy` spaces) <|> return []

rate :: Parser (String, Bool, Bool)
rate = do flt <- (P.many1 P.digit)
          P.char '.'
          flt2 <- (P.many1 P.digit)
          pr <- (P.char '*' >> return True) <|> (return False)
          ac <- (P.char '+' >> return True) <|> (return False)
          return (flt++('.':flt2), pr, ac)
