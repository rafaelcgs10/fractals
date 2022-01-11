import Control.Monad ()
import Data.Complex (Complex ((:+)), magnitude)
import Data.Int ()
import Data.List ( transpose )
import GHC.Real (Integral(toInteger))

iterations = 200
windowWidth = 1800
windowHeigh = 1200

x // y = fromIntegral x / fromIntegral y
x */* y = x * fromIntegral y

normalizeToPlane (x, y) = ((3 // windowWidth */* x) - 2.0, (2 // windowHeigh */* y) - 1.0)

points = [(x, y) | y <- [0 .. windowHeigh - 1], x <- [0 .. windowWidth - 1]]

calcColor iter
  | iter == iterations = (0, 0, 0)
  | otherwise = (red, green, blue)
  where
    red = if fromIntegral iter > fromIntegral iterations * 0.8 then normalized else normalized ** 0.3
    green = if fromIntegral iter > fromIntegral iterations * 0.8 then normalized else normalized ** 0.2
    blue = if fromIntegral iter > fromIntegral iterations * 0.8 then normalized else normalized ** 0.1
    normalized = iter // iterations

mandelInterations r i =
  length . takeWhile (\z -> magnitude z <= 2)
    . take iterations
    $ iterate (\z -> z ^ 2 + (r :+ i)) 0

mandelPointColor (x, y) = (x, y, (calcColor . mandelInterations r) i)
  where
    (r, i) = normalizeToPlane (x, y)

mandelPlane = map mandelPointColor points

to256 n = floor (n * 255)

showPPMPoint (_, _, (r, g, b)) = do
  show (to256 r) ++ " " ++ show (to256 g) ++ " " ++ show (to256 b)

slice n [] = [[]]
slice n xs = let (l, ls) = splitAt n xs in l : slice n ls

showPPMLine l = unwords $ map showPPMPoint l

mandelPPM = map showPPMLine $ slice windowHeigh mandelPlane

printLine [] = return ()
printLine (x:xs) = do
  putStrLn x
  printLine xs

printlPPMMandel = printLine mandelPPM

printPPMHeader = do
  putStrLn "P3"
  putStrLn $ show windowWidth ++ " " ++ show windowHeigh
  putStrLn "255"

main = do
  printPPMHeader
  printlPPMMandel
