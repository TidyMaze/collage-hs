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
import Data.List

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

loadImageWithLogs :: FilePath -> IO (Maybe (Image VS RGB Double))
loadImageWithLogs p = fmap (\e -> (p, e)) maybeLoaded >>= handleImageLoadingResult
  where
    maybeLoaded = readImage p :: IO (Either String (Image VS RGB Double))

handleImageLoadingResult :: (FilePath, Either String (Image VS RGB Double)) -> IO (Maybe (Image VS RGB Double))
handleImageLoadingResult (path, Left msg) = do
  putStrLn ("FAILED to load file " ++ path ++ ": " ++ msg)
  return Nothing
handleImageLoadingResult (path, Right img) =
  do
    _ <- putStrLn ("SUCCESS loading file " ++ path ++ " with dims " ++ show (dims img))
    return $ Just img

collectAllImages :: [FilePath] -> IO[Image VS RGB Double]
collectAllImages = fmap catMaybes . mapM loadImageWithLogs

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

centeredCrop :: Dimensions -> Image VS RGB Double -> (Image VS RGB Double, Int)
centeredCrop (tHeight, tWidth) img = (cropped, loss)
  where
    loss = height * width - tHeight * tWidth
    cropped = crop (startY, startX) (tHeight,tWidth) img
    startY = floor ((fromIntegral height - fromIntegral tHeight) / 2)
    startX = floor ((fromIntegral width - fromIntegral tWidth) / 2)
    (height, width) = dims img

processImage :: Dimensions -> Image VS RGB Double -> (Image VS RGB Double, Int)
processImage dimensions = centeredCrop dimensions . resizeKeepRatioWithFill dimensions


zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs = [0 .. (length xs - 1)] `zip` xs

mergeImages :: Dimensions -> [Image VS RGB Double] -> (Image VS RGB Double, Int)
mergeImages layout images = foldl appendImage (blank, 0) (zipWithIndex images)
  where
    appendImage (acc, tloss) (index,img) = (superimpose (y, x) processedImage acc, tloss + loss)
      where
        y = (index `div` snd layout) * newHeight
        x = (index `mod` snd layout) * newWidth
        (processedImage, loss) = processImage miniDimensions img
    miniDimensions = (newHeight, newWidth)
    newHeight = floor (fromIntegral (fst targetDimensions) / fromIntegral (fst layout))
    newWidth = floor (fromIntegral (snd targetDimensions) / fromIntegral (snd layout))
    blank = makeImageR VS targetDimensions (\(i, j) -> PixelRGB 255 255 255)

displayAllImages :: [(Image VS RGB Double, Int)] -> IO()
displayAllImages = mapM_ displayAndLogLoss
  where
    displayAndLogLoss :: (Image VS RGB Double, Int) -> IO()
    displayAndLogLoss (img, loss) = do
      _ <- displayImage img
      _ <- print ("loss " ++ show loss)
      return ()

sortByLoss :: [(a, Int)] -> [(a, Int)]
sortByLoss = sortOn snd

main :: IO ()
main = (fmap (sortByLoss . makeAllCollages) (listImages >>= collectAllImages)) >>= displayAllImages
  where
    makeAllCollages :: [Image VS RGB Double] -> [(Image VS RGB Double, Int)]
    makeAllCollages images = map (`mergeImages` images) layouts
      where
        layouts = generateLayouts (length images)
