module Local.Util where

import XMonad
import Data.Ratio ((%))

trim :: Int->String->String
trim n s
  | len > n = start ++ elip ++ end
  | otherwise = s
  where
    len = length s
    n' = fromIntegral $ ceiling (n % 2)
    start = take n' s
    end = drop (len - n') s
    elip = "…"

wclass :: Window -> X String
wclass w = fmap ren (runQuery className w)
  where ren n
          | n == "chromium-browser" = "C"
          | n == "Emacs" = "E"
          | n == "URxvt" = "T"
          | otherwise = n
