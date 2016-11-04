{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, GADTs, BangPatterns #-}

module Local.Ring where

import qualified Local.Theme as Theme
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Sequence as S
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Set as Set
import Control.Monad
import Data.Foldable (toList)

import XMonad.Util.XUtils
import XMonad.Util.Font
import Local.Util
import Graphics.X11.Xlib.Window (moveResizeWindow)

import qualified Debug.Trace as D

-- it is like a zipper but where the focus is optional
data Ring a = Ring
  { focus :: Maybe a
  , left :: S.Seq a
  , right :: S.Seq a
  } deriving (Read, Show, Typeable)

data RingDirection = Prev | Next

instance (Read a, Show a, Typeable a) => ExtensionClass (Ring a) where
  initialValue = Ring Nothing S.empty S.empty
  extensionType = PersistentExtension

select :: forall a. (Read a, Show a, Ord a, Typeable a) =>
          (a -> X String) -> (Maybe a -> X (Position, Position, String, String)) -> X (Maybe a)
select lbl op = do
  -- there is some code in Prompt which could be reused for this :/
  f <- initXMF "xft:Sans-16"

  XConf {display = d, theRoot = rw, XMonad.config = XConfig {modMask = mod} } <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  (as, ds) <- textExtentsXMF f "asdf"
  (x, y, _, _) <- op Nothing

  let th = 4 + as + ds
      tw = 200
      rect = Rectangle (fi x) (fi y) (fi tw) (fi th)

  window <- createNewWindow rect Nothing "" True

  let eventLoop :: Ring a -> X (Ring a)
      eventLoop state@(Ring f' _ _) = do
        n <- maybe (return "???") lbl f'
        tw' <- textWidthXMF d f $ n
        (as, ds) <- textExtentsXMF f n

        (wx, wy, fg, bg) <- op f'

        let ww = tw' + 4
            wh = as+ds+4

        -- paintAndWrite cannot do an offset, so for now we can only show 1
        -- thing
        io $ moveResizeWindow d window (fi wx) (fi wy) (fi ww) (fi wh)
        paintAndWrite window f (fi ww) (fi wh) 2
          "#333" fg fg bg [AlignCenter] [n]
        io getKey >>= (processEvent state)

      getKey :: IO (EventType, KeySym)
      getKey = allocaXEvent $ \p -> do maskEvent d (keyPressMask .|. keyReleaseMask) p
                                       KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                                       s <- keycodeToKeysym d c 0
                                       return (t, s)

      processEvent state (t, s)
        -- TODO shift key for reverse direction
        -- TODO other lines?
        | t == keyPress && s == xK_u = eventLoop $ rotateRight state
        | t == keyPress && s == xK_i = eventLoop $ rotateLeft state
        | t == keyRelease && s == xK_Super_L = return $ state
        | otherwise = refresh >> eventLoop state

  showWindow window

  -- paint the window, grab the keys, loop until we have finished

  state <- XS.get :: X (Ring a)

  io $ grabKeyboard d rw False grabModeAsync grabModeAsync currentTime
  s2@(Ring f' _ _) <- eventLoop $ rotateRight state
  io $ ungrabKeyboard d currentTime

  deleteWindow window
  releaseXMF f

  io $ sync d True

  XS.put $ fix s2

  return f'

rotate :: forall a. (Read a, Show a, Ord a, Typeable a) => RingDirection -> X (Maybe a)
rotate r = do
  XS.modify $ case r of
    Next -> (rotateRight :: (Ring a -> Ring a))
    Prev -> (rotateLeft :: (Ring a -> Ring a))
  (Ring fm _ _) <- XS.get :: X (Ring a)
  return fm

rotateLeft :: Ring a -> Ring a
rotateLeft (Ring f l r)
  | S.null l = Ring (toMaybe tr) hr (sFromMaybe f)
  | otherwise = Ring (toMaybe tl) hl (sFromMaybe f >< r)
  where
    !(hr, tr) = S.splitAt (S.length r - 1) r
    !(hl, tl) = S.splitAt (S.length l - 1) l

rotateRight :: Ring a -> Ring a
rotateRight (Ring f l r)
  | S.null r = Ring (toMaybe ll) (sFromMaybe f) rl
  | otherwise = Ring (toMaybe lr) (l >< (sFromMaybe f)) rr
  where
    !(ll, rl) = S.splitAt 1 l
    !(lr, rr) = S.splitAt 1 r

fix :: (Ring a) -> (Ring a)
fix (Ring f l r) = (Ring f S.empty (l >< r))

update :: (Read a, Show a, Ord a, Typeable a) => X ((Maybe a), [a]) -> X ()
update g = do
  (focus, cands) <- g
  let del = maybe id Set.delete focus
      cands' = del $ Set.fromList cands
      exists = flip Set.member cands'
      filt = S.filter exists
      -- we have candidates without focus in them
      update' (Ring f l r) = result
        where !result = if f == focus then rotatedRing else filteredRing
              !filteredRing = (Ring focus S.empty (f' >< l' >< r'))
              !rotatedRing = (Ring focus l' r')
              !l' = filt l
              !r' = filt r
              !f' = filt $ sFromMaybe f
  XS.modify $ update'

contents :: forall a. (Read a, Show a, Ord a, Typeable a) => X (Maybe a, [a])
contents = do
  (Ring f l r) <- XS.get :: X (Ring a)
  return $ (f, toList $ l >< r)

sFromMaybe :: Maybe a -> S.Seq a
sFromMaybe Nothing = S.empty
sFromMaybe (Just x) = S.singleton x

toMaybe :: S.Seq a -> Maybe a
toMaybe s = if S.null s then Nothing else Just $ S.index s 0

