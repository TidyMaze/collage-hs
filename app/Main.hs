module Main where

import Graphics.Image.IO
import Graphics.Image.ColorSpace
import Graphics.Image.Types
import Data.String
import System.Directory

imagesPath :: FilePath
imagesPath = "images"

listImages :: IO[FilePath]
listImages = fmap (map ((imagesPath ++ "/") ++)) (listDirectory imagesPath)

readImageAndDisplayIfPossible :: FilePath -> IO()
readImageAndDisplayIfPossible p = (readImage p :: IO (Either String (Image VS RGB Double))) >>= showImageOrLog

showImageOrLog :: Either String (Image VS RGB Double) -> IO ()
showImageOrLog (Left msg) = putStrLn ("FAILED to load file: " ++ msg)
showImageOrLog (Right img) = displayImage img

readAllImageAndDisplayIfPossible :: [FilePath] -> IO()
readAllImageAndDisplayIfPossible = mapM_ readImageAndDisplayIfPossible

main :: IO ()
main = listImages >>= readAllImageAndDisplayIfPossible
  -- c1 <- readImageRGB VU "images/chat2.jpg"
  -- displayImage c1
  -- c2 <- readImageRGB VU "images/chat3.jpg"
  -- displayImage c2
  -- displayImage ((c1 + c2) / 2)
