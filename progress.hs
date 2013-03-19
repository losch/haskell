-- Prints a nice progress bar
--
-- Parameters:
--     Size of the progress bar
--     Percentage of filled elements
--
import System.Environment
import System.Exit
import Data.List

progress :: Int -> Float -> String
progress size fill =
    (replicate filled '#') ++ (replicate partial '+') ++ (replicate empty '-')
    where scaled  = fromIntegral size * (fill / 100)
          filled  = truncate scaled
          partial | (scaled - fromIntegral filled) >= 0.5 = 1
                  | otherwise                             = 0
          empty   = size - filled - partial

drawProgress :: Int -> Float -> IO ()
drawProgress size fill = putStrLn $ progress size fill

usage :: String
usage = "Usage: progress [SIZE] [PERCENTAGE]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        (size:fill:_) -> drawProgress (read size) (read fill)
        _ -> putStrLn usage

