module Main where

import Graphics.Image.IO
import Graphics.Image.ColorSpace
import Graphics.Image.Types
import Graphics.Image (dims)
import Graphics.Image.Interface (BaseArray)
import Data.String
import System.Directory

type LocalizedImage = (FilePath, Image VS RGB Double)

imagesPath :: FilePath
imagesPath = "images"

listImages :: IO[FilePath]
listImages = fmap (map ((imagesPath ++ "/") ++)) (listDirectory imagesPath)

logDims :: BaseArray arr cs e => Image arr cs e -> IO ()
logDims = print . dims

mkLocalizedImage :: FilePath -> Image VS RGB Double -> LocalizedImage
mkLocalizedImage a b = (a, b)

readImageAndDisplayIfPossible :: FilePath -> IO()
readImageAndDisplayIfPossible p = fmap (\e -> (p, e)) maybeLoaded  >>= showImageOrLog
  where
    maybeLoaded = readImage p :: IO (Either String (Image VS RGB Double))

showImageOrLog :: (FilePath, Either String (Image VS RGB Double)) -> IO ()
showImageOrLog (path, Left msg) = putStrLn ("FAILED to load file " ++ path ++ ": " ++ msg)
showImageOrLog (path, Right img) = do
  _ <- logDims img
  displayImage img

readAllImageAndDisplayIfPossible :: [FilePath] -> IO()
readAllImageAndDisplayIfPossible = mapM_ readImageAndDisplayIfPossible


main :: IO ()
main = listImages >>= readAllImageAndDisplayIfPossible
  -- c1 <- readImageRGB VU "images/chat2.jpg"
  -- displayImage c1
  -- c2 <- readImageRGB VU "images/chat3.jpg"
  -- displayImage c2
  -- displayImage ((c1 + c2) / 2)
