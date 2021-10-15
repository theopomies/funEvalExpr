module Display (display) where

import Text.Printf (printf)

display :: Double -> IO ()
display = printf "%.2f\n"
