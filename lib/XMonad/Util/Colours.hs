module XMonad.Util.Colours where

-- import Graphics.X11.Xlib
-- import XMonad
-- import Control.Exception.Extensible (bracket, catch, SomeException(..))
-- import Data.List
-- import qualified Data.Map.Strict as M

-- getResources :: String -> X [String]
-- getResources k = do
--   d <- asks display
--   r <- asks theRoot
--   let getProp = (internAtom d "RESOURCE_MANAGER" False >>= getTextProperty d r)
--       extract = wcTextPropertyToTextList d
--   io $ bracket getProp (xFree . tp_value) extract `catch` \(SomeException _) -> return []

-- resourceMap :: [String] -> M.Map String String
-- resourceMap = foldl (\m s -> let (k, a) = (span (== ':') s) in M.insert k a m) M.empty

-- updatedDefaults res =
--   let m = resourceMap res in
--     -- the resource database algorithm is a bit of a nuisance to implement
--     undefined
    

-- defaults = M.fromList
--   [ ("focusedBorder", "#ff0000")
--   , ("focusedText", "#ffffff")
--   , ("urgentBorder", "#ff00ff")
--   , ("urgentText", "#ffffff")
--   , ("background", "#5e6b7b")
--   , ("foreground", "#ffffff")
--   , ("unfocusedBorder", "#559955")
--   ]

border = "#dd0000"
borderText = "#ffffff"
urgent = "#00dddd"
urgentText = "#000000"
tabBackground = "#444444"
background = "#121212"
dimBorder = "#888888"
dimText = "#eeeeee"
text = "#ffffff"

font = "xft:Monospace-11"
