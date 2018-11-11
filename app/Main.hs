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
type Dimensions = (Int, Int)

targetDimensions :: Dimensions
targetDimensions = (600, 800)

generateLayouts :: Int -> [Dimensions]
generateLayouts nbImages = map (\width -> (ceiling (fromIntegral nbImages / fromIntegral width), width) ) [1 .. nbImages]

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
showImageOrLog (path, Left msg) =
  putStrLn ("FAILED to load file " ++ path ++ ": " ++ msg)
showImageOrLog (path, Right img) =
  do
    _ <- putStrLn ("SUCCESS loading file " ++ path ++ " with dims " ++ show (dims img))
    (displayImage . processImage) img

readAllImageAndDisplayIfPossible :: [FilePath] -> IO()
readAllImageAndDisplayIfPossible = mapM_ readImageAndDisplayIfPossible

resizeKeepRatioWithFill :: Dimensions -> Image VS RGB Double -> Image VS RGB Double
resizeKeepRatioWithFill (tHeight, tWidth) img = resize Bilinear Edge destinationSize img
    where
      destinationSize =
        if rVertical >= rHorizontal
        then (tHeight, ceiling (rVertical * fromIntegral width))
        else (ceiling (rHorizontal * fromIntegral height), tWidth)
      rVertical = fromIntegral tHeight / fromIntegral height
      rHorizontal = fromIntegral tWidth / fromIntegral width
      (height, width) = dims img

centeredCrop :: Dimensions -> Image VS RGB Double -> Image VS RGB Double
centeredCrop (tHeight, tWidth) img = crop (startY, startX) (tHeight,tWidth) img
  where
    startY = floor ((fromIntegral height - fromIntegral tHeight) / 2)
    startX = floor ((fromIntegral width - fromIntegral tWidth) / 2)
    (height, width) = dims img

processImage :: Image VS RGB Double -> Image VS RGB Double
processImage = centeredCrop targetDimensions . resizeKeepRatioWithFill targetDimensions

main :: IO ()
main = listImages >>= readAllImageAndDisplayIfPossible
