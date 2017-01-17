module Local.Marks (mark, unmark, toggleMark, isMarked, unmarked, marked, clearMarks) where

import qualified XMonad.StackSet as W
import XMonad hiding (modify, get)
import qualified XMonad.Util.ExtensibleState as XS
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative


type MarkMap = Map Char (Set Window)

data Marks = Marks MarkMap
  deriving (Typeable, Read, Show)

instance ExtensionClass Marks where
  initialValue = Marks $ M.empty
  extensionType = PersistentExtension

cleanup :: X ()
cleanup = do ws <- gets (S.fromList . W.allWindows . windowset)
             let exists :: Set Window -> Set Window
                 exists = S.intersection ws
             XS.modify $ \(Marks m) -> (Marks $ M.map exists m)

get :: X MarkMap
get = cleanup >> fmap (\(Marks m) -> m) XS.get

modify :: (MarkMap -> MarkMap) -> X ()
modify f = XS.modify (\(Marks m) -> let m' = f m in Marks m') >> cleanup

clearMarks :: Char -> X ()
clearMarks c = modify $ M.delete c

mark :: Char -> Window -> X ()
mark s w = modify $ M.alter (maybe (Just $ S.singleton w) (Just . S.insert w)) s

unmark :: Char -> Window -> X ()
unmark s w = modify $ M.alter (maybe Nothing (Just . S.delete w)) s

toggleMark :: Char -> Window -> X ()
toggleMark s w = modify $ M.alter (maybe (Just $ S.singleton w)
                                   (\s -> Just $ (if S.member w s then S.delete else S.insert) w s)) s

isMarked :: Char -> X (Window -> Bool)
isMarked s = do mm <- get
                return $ \w -> ((S.member w) . (M.findWithDefault (S.empty) s)) mm

unmarked :: Char -> [Window] -> X [Window]
unmarked s ws = do marks <- get
                   let s' = M.findWithDefault S.empty s marks
                   return $ filter (not . flip S.member s') ws

marked :: Char -> [Window] -> X [Window]
marked s ws = do marks <- get
                 let s' = M.findWithDefault S.empty s marks
                 return $ filter (flip S.member s') ws
