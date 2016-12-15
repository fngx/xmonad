{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
module Local.Prompt where

import XMonad hiding (config)
import qualified XMonad (config)
import XMonad.Util.Font
import XMonad.Util.Types
import XMonad.Util.Stack -- for zipper
import qualified XMonad.StackSet as W
import qualified Data.Map.Strict as M
import Data.List (foldl', elemIndex, (\\), isPrefixOf, findIndex)

import Control.Arrow (first, (&&&), (***))
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Data.Ratio

import Text.ParserCombinators.ReadP hiding (get)
import XMonad.Util.EZConfig (parseKey)

import qualified Debug.Trace as D

-- a replacement for XMonad.Prompt

-- |> input input input                [Run] [Term] [10] |
-- |  first second third fourth ....                     |

-- todo: window prompt
-- todo: ws prompt
-- todo: run prompt

-- question: is it a good idea to create the thunks for an IO () for every completion?

data Config = Config
  { normal :: (String, String)
  , highlight :: (String, String)
  , item :: (String, String)
  , border :: (Int, String)
  , font :: String
  , keymap :: [(String, Prompt ())] -- ezconfig style but no submaps
  , rawKeymap :: [((KeyMask, KeySym), Prompt ())]
  , prompt :: String
  , top :: Bool
  }

type Prompt = StateT PromptState X

-- a choice has a name, and then a list of named actions
-- that you can take on it
type Choice = (String, String, [(String, X ())])

cName (a, _, _) = a
cColor (_, a, _) = a
cActions (_, _, a) = a

data Pager = Pager
  { pages :: Zipper (Zipper Choice) -- slightly annoying; inner zipper can never be empty
  , action :: String
  }

emptyP :: Pager
emptyP = Pager { pages = emptyZ, action = "" }

fillPager :: Pager -> [(Choice, Int)] -> Int -> Pager
fillPager pager choices width =
  let chop :: (Zipper (Zipper Choice), Int) -> (Choice, Int) -> (Zipper (Zipper Choice), Int)
      chop (pager, usedWidth) (choice, choiceWidth) =
        -- add the choice to the current page
        let newWidth = usedWidth + choiceWidth
            -- the result if adding a new page
            addNewPage = (insertDownZ (singletonZ choice) pager, choiceWidth)
            -- the result if adding to the current page
            extendPage = (onFocusedZ (insertDownZ choice) pager, newWidth)
        in
          if isNothing pager || newWidth > width then addNewPage
          else extendPage

      -- this is the division of choices into pages
      newPager = fst $ foldl chop (emptyZ, 0) choices
  in rightP $ fixAction $ Pager {pages = newPager, action = action pager}

-- TODO this would be better if we reset whenever the list changes
fixAction :: Pager -> Pager
fixAction p@(Pager { pages = ps, action = a })
  | a `elem` actions = p
  | not $ null actions = p { action = head actions }
  | otherwise = p { action = "" }
  where actions = maybe [""] (\c -> map fst $ cActions $ fromJust $ getFocusZ c) (getFocusZ ps)

leftP :: Pager -> Pager
leftP p@(Pager {pages = Nothing}) = p
leftP p@(Pager {pages = ps}) = fixAction $ p
  {pages = goLeft ps}
  where goLeft z
          | atEndZ True $ fromJust $ getFocusZ ps = onFocusedZ (goEndZ False) (focusUpZ ps)
          | otherwise = onFocusedZ focusUpZ ps

rightP :: Pager -> Pager
rightP p@(Pager {pages = Nothing}) = p
rightP p@(Pager {pages = ps}) = fixAction $ p
  {pages = goRight ps}
  where goRight z
          | atEndZ False $ fromJust $ getFocusZ ps = onFocusedZ (goEndZ True) (focusDownZ ps)
          | otherwise = onFocusedZ focusDownZ ps

atEndZ _ Nothing = True
atEndZ b (Just (W.Stack f u d)) = null $ if b then u else d

goEndZ b = until (atEndZ b) $ if b then focusUpZ else focusDownZ

type Input = (String, String)

data PromptState = PS
  { generator :: String -> X [Choice]

  -- X state:
  , window :: !Window
  , _display :: !Display
  , context :: !GC
  , xfont :: XMonadFont

  , input :: Input
  , config :: Config
  -- whether to terminate
  , done :: Bool
  -- results from the generator last time it was computed
  -- this needs improvement
  , pager :: Pager
  , pagerNames :: [String]
  , handler :: !(M.Map (KeyMask, KeySym) (Prompt ()))
  }

readKey :: KeyMask -> String -> Maybe (KeyMask, KeySym)
readKey m = listToMaybe . parses
  where parses = map fst . filter (null.snd) . readP_to_S (parseKeyCombo m)

        indexMod = (!!) [mod1Mask,mod2Mask,mod3Mask,mod4Mask,mod5Mask]

        -- | Parse a modifier-key combination such as "M-C-s" (mod+ctrl+s).
        parseKeyCombo :: KeyMask -> ReadP (KeyMask, KeySym)
        parseKeyCombo c = do mods <- many (parseModifier c)
                             k <- parseKey
                             return (foldl' (.|.) 0 mods, k)

        -- | Parse a modifier: either M- (user-defined mod-key),
        --   C- (control), S- (shift), or M#- where # is an integer
        --   from 1 to 5 (mod1Mask through mod5Mask).
        parseModifier :: KeyMask -> ReadP KeyMask
        parseModifier c =  (string "M-" >> return c)
                           +++ (string "C-" >> return controlMask)
                           +++ (string "S-" >> return shiftMask)
                           +++ do _ <- char 'M'
                                  n <- satisfy (`elem` ['1'..'5'])
                                  _ <- char '-'
                                  return $ indexMod (read [n] - 1)

padding = 8
spaceBetweenChoice = 16

select :: Config -> (String -> X [Choice]) -> X ()
select config generator = do
  XConf {display = d, theRoot = rw, XMonad.config = XConfig {modMask = mod} } <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  font <- initXMF $ font config
  extent <- textExtentsXMF font "ASDKFH"
  -- (fi padding) +
  let wh = 2 * (fi (fst $ border config)) + 2 * ((fi $ fst extent) + (fi $ snd extent))

  -- allocation
  win <- io $ mkUnmanagedWindow d (defaultScreenOfDisplay d) rw sx (if (top config) then sy else (fi $ sy + (fi sh) - (fi wh))) sw (fi wh)
  gc <- io $ createGC d win

  io $ mapWindow d win
  -- event mask for window
  io $ selectInput d win $ exposureMask .|. keyPressMask

  -- render once with empty string
  -- I need to preserve more state, etc etc.
  initialInputs <- generator ""
  -- event loop using state monad

  let initialInputs' = fromIndex initialInputs 0

  evalStateT eventLoop $
    PS { generator = generator
       , window = win
       , context = gc
       , _display = d
       , xfont = font
       , input = ("", "")
       , config = config
       , done = False
       , pager = emptyP
       , pagerNames = []
       , handler = M.fromList $
         let ks = mapMaybe (\(k, a) -> fmap (flip (,) a) (readKey mod k)) (keymap config) in
           --D.traceShow (zip (map fst $ keymap config) (map fst ks))
           ks ++ (rawKeymap config)
       }

  -- clean up
  releaseXMF font
  io $ freeGC d gc
  io $ destroyWindow d win

eventLoop :: Prompt ()
eventLoop = do
  (d, w) <- gets (_display &&& window)
  status <- io $ grabKeyboard d w True grabModeAsync grabModeAsync currentTime
  when (status == grabSuccess) $ do
    -- compute initial option set
    promptUpdateOptions
    -- render the window once to start with
    render
    -- start reading keys
    handleKeys
    io $ ungrabKeyboard d currentTime
  io $ sync d False

render :: Prompt ()
render = do
  state <- get
  let d = _display state
      w = window state
      gc = context state
      ft = xfont state

  atts <- io $ getWindowAttributes d w
  let scr = defaultScreenOfDisplay d
      ht = (fi $ wa_height atts)
      wh = (fi $ wa_width atts)
      bw = fst $ border $ config state
      (fgString, bgString) = normal $ config state
      (hfgString, hbgString) = highlight $ config state
      (fgItem, bgItem) = item $ config state
      prompt' = prompt $ config state

  Just bc <- io $ initColor d $ snd $ border $ config state
  Just bg <- io $ initColor d $ bgString

  io $ do
    p <- createPixmap d w wh ht (defaultDepthOfScreen scr)
    setForeground d gc bc
    fillRectangle d p gc 0 0 wh ht
    setForeground d gc bg
    fillRectangle d p gc (fi bw) (fi bw) (wh - (fi $ 2*bw)) (ht - (fi $ 2*bw))

    let str = printStringXMF d p ft gc
        normStr = str fgString bgString
        hiStr = str hfgString hbgString

        leftX = fi $ bw + padding
        rightX = (fi wh) - bw
        top2 = (floor $ (fi ht) / 2)

        actions = map fst $
                  maybe []
                  (\x -> maybe [] cActions $ getFocusZ x) $
                  getFocusZ $ pages $ pager state

        selAction = action $ pager state

        printItem :: Int -> Int -> Int -> (Int -> Int -> Int) -> (Position -> Position -> String -> IO ()) -> String -> IO Int
        printItem y0 x0 spc dir prn text = do
          width <- textWidthXMF d ft text
          (rising, _) <- textExtentsXMF ft text
          let x1 = dir x0 width
          prn (fi (min x0 x1)) (fi (y0 + (fi rising))) text
          return $ dir x1 spc

        -- we want to render the actions from the right to the left
        printAction x text = printItem (fi bw) (fi x) (2*padding) (-)
          (if text == selAction then hiStr else normStr)
          text

        printOption fg bg x choice = let nam = cName choice
                                         fg' = cColor choice
                                         str' = str (if null fg' then fg else fg') bg in
                                       printItem (fi $ top2) (fi x) spaceBetweenChoice (+) str' nam


    foldM_ printAction (rightX) $ reverse actions

    setForeground d gc bc

--    fillRectangle d p gc (fi $ (padding+spaceBetweenChoice)) 0 (fi bw) ht

    -- todo shrink input if too long
    inx0 <- printItem bw leftX 0 (+) normStr $ prompt'

    inx1 <- printItem bw inx0 0 (+) normStr $ reverse $ fst $ input state
    inx2 <- if null $ snd $ input state then printItem bw inx1 0 (+) hiStr $ " "
            else printItem bw inx1 0 (+) hiStr $ take 1 $ snd $ input state
    inx3 <- printItem bw inx2 0 (+) normStr $ drop 1 $ snd $ input state

    -- TODO render < and > for left and right indicators
--    whenJust () $ \opts ->
    case getFocusZ $ pages $ pager state of
      Just (Just (W.Stack f l r)) -> do x1 <- foldM (printOption fgItem bgItem) inx0 $ reverse l
                                        x2 <- printOption hfgString hbgString x1 f
                                        foldM_ (printOption fgItem bgItem) x2 r
      _ -> do printItem (fi top2) inx0 0 (+) normStr "no match"
              return ()

    whenJust (pages $ pager state) $ \(W.Stack _ l r) -> when (not $ null l && null r) $ printItem (fi top2) rightX 0 (-) normStr "…" >> return ()

    copyArea d p w gc 0 0 wh ht 0 0
    io $ freePixmap d p

  return ()

nextKeyEvent :: Display -> X (Maybe (KeyMask, KeySym, String))
nextKeyEvent d = do
  io $ do allocaXEvent $ \e -> do
            maskEvent d (exposureMask .|. keyPressMask) e
            ev <- getEvent e
            if ev_event_type ev == keyPress then
              do x <- lookupString $ asKeyEvent e
                 return $ case x of (Just ks, str) -> Just (ev_state ev, ks, str)
                                    _ -> Nothing
              else return $ if ev_event_type ev == expose &&
                               ev_count ev == 0 then Just (0, xK_VoidSymbol, "")
                            else Nothing

handleKeys :: Prompt ()
handleKeys = do
  (d, h) <- gets (_display &&& handler)
  XConf { XMonad.config = XConfig {modMask = mod} } <- lift ask

  keym <- lift $ nextKeyEvent d

  whenJust keym $ keyAction mod h
  when (isJust keym) render

  gets done >>= flip unless handleKeys

  where keyAction mod h (mask, sym, str) = case (M.lookup (mask, sym) h) of
          Just x -> --D.traceShow ("user handler for", mask, sym, str)
            x
          Nothing -> --D.traceShow ("default handler for ", mask, sym, str)
            defaultHandle
            where
              defaultHandle
                | mask == 0 || mask == shiftMask = promptInsertStr str
                  -- check whether one of the actions starts with the typed key
                  -- and if it does then run it
                  -- TODO allow shift to keep thing open? or redisplay?
                | mask == mod && (not $ null str) =
                    do p <- gets pager
                       let action = do pg <- getFocusZ (pages p) -- maybe monad
                                       sel <- getFocusZ pg
                                       lookup (head str) (map (\(n, a) -> (head n, a)) $ cActions sel)
                       fromMaybe promptNothing $ fmap (\x -> lift x >> promptClose) action
                | otherwise = promptNothing


-- edit operations

iintegrate :: Input -> String
iintegrate (l, []) = reverse $ l
iintegrate ([], r) = r
iintegrate (l, r) = (reverse l) ++ r

iappend :: String -> Input -> Input
iappend s (l, r) = (reverse s ++ l, r)

ileft :: Input -> Input
ileft ([], r) = ([], r)
ileft (l:ls, r) = (ls, l:r)

idleft :: Input -> Input
idleft ([], r) = ([], r)
idleft (l:ls, r) = (ls, r)

iright :: Input -> Input
iright (l, []) = (l, [])
iright (l, r:rs) = (r:l, rs)

idright :: Input -> Input
idright (l, []) = (l, [])
idright (l, r:rs) = (l, rs)

-- state manipulation operations

modifyPager :: (Pager -> Pager) -> Prompt ()
modifyPager f = modify $ \s -> s { pager = f (pager s) }

modifyCursor :: (Input -> Input) -> Prompt ()
modifyCursor f = modify $ \s -> s { input = f (input s) }

promptCursorLeft :: Prompt ()
promptCursorLeft = modifyCursor ileft

promptCursorRight :: Prompt ()
promptCursorRight = modifyCursor iright

promptNextOption :: Prompt ()
promptNextOption = modifyPager rightP

promptPrevOption :: Prompt ()
promptPrevOption = modifyPager leftP

promptCycleInput :: [String] -> Prompt ()
promptCycleInput ns = do
  modifyCursor $ \(l, r) ->
    let l' = let ns' = map reverse ns
                 nc = length ns
                 w = words l in
               if null w then head ns'
               else let w' = head w
                        mi = findIndex (== w') ns'
                    in case mi of
                         Nothing -> unwords $ (head ns'):w
                         Just i -> unwords $ (ns' !! ((i+1) `mod` nc)):tail w
    in (l', r)
  promptUpdateOptions

promptNextAction :: Prompt ()
promptNextAction = modifyPager na
  where
    na :: Pager -> Pager
    na p@(Pager {pages = ps, action = ac}) =
          case ps of
            Just (W.Stack (Just (W.Stack (_, _, acs) _ _)) _ _) ->
              let acs' = map fst acs
                  idx = elemIndex ac acs'
                  idx' = fmap (+ 1) idx
                  a' = fmap ((!!) (cycle acs')) idx'
                  a'' = fromMaybe ac a'
              in p {action = a''}
            _ -> p

promptPrevAction :: Prompt ()
promptPrevAction = modifyPager id

promptNothing :: Prompt ()
promptNothing = return ()

promptPerformAction :: Prompt ()
promptPerformAction = do
  Pager {pages = pg, action = ac} <- gets pager
  lift $ whenJust (getFocusZ pg) $ \p -> do
    whenJust (getFocusZ p) $ \(c, t, acs) -> do
      whenJust (lookup ac acs) id

promptClose :: Prompt ()
promptClose = modify $ \s->s { done=True }

promptDone :: Prompt ()
promptDone = do
  promptPerformAction
  promptClose

promptClear :: Prompt ()
promptClear = do
  modifyCursor $ const ("", "")
  promptUpdateOptions

promptInsertStr :: String -> Prompt ()
promptInsertStr str = do
  when (not $ null str) $ do
    modifyCursor $ iappend str
    promptUpdateOptions

promptBackspace :: Prompt ()
promptBackspace = do
  modifyCursor idleft
  promptUpdateOptions

promptDel :: Prompt ()
promptDel = do
  modifyCursor idright
  promptUpdateOptions

promptAppendSpace :: Prompt ()
promptAppendSpace = do
  (inp, Pager {pages = pg}) <- gets (input &&& pager)
  let inp' = iintegrate inp
  when (not $ ' ' `elem` inp') $ do
    whenJust (getFocusZ pg) $ \p -> do
      whenJust (getFocusZ p) $ \(c, _, _) -> do
        when (inp' `isPrefixOf` c) $
          promptInsertStr $ drop (length inp') c
  promptInsertStr " "

promptComplete :: Prompt ()
promptComplete = do
  (inp, names) <- gets (input &&& pagerNames)
  -- this is wrong

  let inp' = reverse $ fst inp
      names' = names \\ [inp']
      pfx = if null names' then "" else foldl1 commonPrefix $ names'
      inp'' = drop (length inp') pfx
  if null inp'' then promptNextOption
    else promptInsertStr inp''

    where
      commonPrefix :: (Eq e) => [e] -> [e] -> [e]
      commonPrefix _ [] = []
      commonPrefix [] _ = []
      commonPrefix (x:xs) (y:ys)
        | x == y    = x : commonPrefix xs ys
        | otherwise = []


promptUpdateOptions :: Prompt ()
promptUpdateOptions = do
  (gen, inp) <- gets (generator &&& input)
  choices <- lift $ gen (iintegrate inp)

  -- find out the window width and string widths, so we can pack into the pager
  -- this might be a bit wasteful - not sure.
  (win, (dis, (font, ptext))) <- gets (window &&& _display &&& xfont &&& (prompt . config))
  (wwidth, swidths) <- io $ do wa <- getWindowAttributes dis win
                               ws <- mapM (textWidthXMF dis font) (map cName choices)
                               return $ (wa_width wa, map fi ws)
  pwidth <- io $ textWidthXMF dis font ptext
  -- now pack the pager and update it
  modify $ \s -> s { pager = fillPager
                             (pager s)
                             (zip choices $ map (\w -> fi $ (fi w) + spaceBetweenChoice) swidths)
                             (floor $ (fi wwidth) - (fi pwidth))
                   , pagerNames = map cName choices }

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes
