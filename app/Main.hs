module Main where

import Graphics.Image.IO
import Graphics.Image.ColorSpace
import Graphics.Image.Types
import Graphics.Image.Processing
import Graphics.Image (dims)
import Graphics.Image.Interface (BaseArray)
import Data.String
import System.Directory

type LocalizedImage = (FilePath, Image VS RGB Double)

targetDimensions :: (Int, Int)
targetDimensions = (1080, 1980)

imagesPath :: FilePath
imagesPath = "images"

listImages :: IO[FilePath]
listImages = fmap (map ((imagesPath ++ "/") ++)) (listDirectory imagesPath)

mkLocalizedImage :: FilePath -> Image VS RGB Double -> LocalizedImage
mkLocalizedImage a b = (a, b)

readImageAndDisplayIfPossible :: FilePath -> IO()
readImageAndDisplayIfPossible p = fmap (\e -> (p, e)) maybeLoaded >>= showImageOrLog
  where
    maybeLoaded = readImage p :: IO (Either String (Image VS RGB Double))

showImageOrLog :: (FilePath, Either String (Image VS RGB Double)) -> IO ()
showImageOrLog (path, Left msg) = putStrLn ("FAILED to load file " ++ path ++ ": " ++ msg)
showImageOrLog (path, Right img) = do
  _ <- putStrLn ("SUCCESS loading file " ++ path ++ " with dims " ++ show (dims img))
  displayImage $ processImage img

readAllImageAndDisplayIfPossible :: [FilePath] -> IO()
readAllImageAndDisplayIfPossible = mapM_ readImageAndDisplayIfPossible

processImage :: Image VS RGB Double -> Image VS RGB Double
processImage = resize Bilinear Edge targetDimensions

main :: IO ()
main = listImages >>= readAllImageAndDisplayIfPossible
  -- c1 <- readImageRGB VU "images/chat2.jpg"
  -- displayImage c1
  -- c2 <- readImageRGB VU "images/chat3.jpg"
  -- displayImage c2
  -- displayImage ((c1 + c2) / 2)
