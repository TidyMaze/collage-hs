module Main where

import Graphics.Image.IO
import Graphics.Image.ColorSpace
import Graphics.Image.Types
import Graphics.Image.Processing
import Graphics.Image (dims, makeImageR)
import Graphics.Image.Interface (BaseArray)
import Data.String
import Data.Maybe
import System.Directory

type LocalizedImage = (FilePath, Image VS RGB Double)
type Dimensions = (Int, Int)

targetDimensions :: Dimensions
targetDimensions = (600, 800)

generateLayouts :: Int -> [Dimensions]
generateLayouts nbImages = map layoutFromWidth [1 .. nbImages]
  where
    layoutFromWidth width = (height, width)
      where
        height = ceiling (fromIntegral nbImages / fromIntegral width)

imagesPath :: FilePath
imagesPath = "images"

listImages :: IO[FilePath]
listImages = fmap (map ((imagesPath ++ "/") ++)) (listDirectory imagesPath)

readImageAndDisplayIfPossible :: FilePath -> IO (Maybe (Image VS RGB Double))
readImageAndDisplayIfPossible p = fmap (\e -> (p, e)) maybeLoaded >>= handleImageLoadingResult
  where
    maybeLoaded = readImage p :: IO (Either String (Image VS RGB Double))

handleImageLoadingResult :: (FilePath, Either String (Image VS RGB Double)) -> IO (Maybe (Image VS RGB Double))
handleImageLoadingResult (path, Left msg) = do
  putStrLn ("FAILED to load file " ++ path ++ ": " ++ msg)
  return Nothing
handleImageLoadingResult (path, Right img) =
  do
    _ <- putStrLn ("SUCCESS loading file " ++ path ++ " with dims " ++ show (dims img))
    _ <- displayImage img
    return $ Just img

collectAllImages :: [FilePath] -> IO[Image VS RGB Double]
collectAllImages = fmap catMaybes . mapM readImageAndDisplayIfPossible

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

processImage :: Dimensions -> Image VS RGB Double -> Image VS RGB Double
processImage dimensions = centeredCrop dimensions . resizeKeepRatioWithFill dimensions

mergeImages :: Dimensions -> [Image VS RGB Double] -> Image VS RGB Double
mergeImages layout images = superimpose (0,0) firstProcessed blank
  where
    miniDimensions = (newHeight, newWidth)
    newHeight = floor (fromIntegral (fst targetDimensions) / fromIntegral (fst layout))
    newWidth = floor (fromIntegral (snd targetDimensions) / fromIntegral (snd layout))
    firstProcessed = processImage miniDimensions (head images)
    blank = makeImageR VS targetDimensions (\(i, j) -> PixelRGB 255 255 255)

main :: IO ()
main = fmap (\images -> mergeImages (head (generateLayouts (length images))) images) (listImages >>= collectAllImages) >>= displayImage
  where
    layouts = generateLayouts
