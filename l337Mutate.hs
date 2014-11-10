import Data.Char
import Data.List

leets = ["oO0*","aA4@","lL1!","eE3" ,"tT7" ,"iI1!","sS5"]

leetchar c []     = nub [c, toUpper c, toLower c]
leetchar c (l:ls) = if c `elem` l then l else leetchar c ls

leet []     = return []
leet (x:xs) = do
  p <- leetchar x leets
  n <- leet xs
  return $ p : n

main = do
  cs <- getContents
  mapM_ putStrLn (concatMap leet (lines cs))
