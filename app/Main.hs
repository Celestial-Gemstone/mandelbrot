{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Complex (Complex(..))
import GHC.TypeNats (Natural)
import Codec.Picture (Image, generateImage, PngSavable (encodePng), Pixel (PixelBaseComponent, mixWith), PixelRGB8(..))
import System.Process (CreateProcess (..), proc, createProcess, StdStream (CreatePipe, NoStream, Inherit), waitForProcess)
import Control.Monad (forM_, forM)

import qualified Data.ByteString.Char8 as B
import Data.ByteString (toStrict)

import GHC.IO.Handle (Handle)
import Control.Concurrent (newEmptyMVar, forkIO, putMVar, readMVar)


type MandelbrotPixel = PixelRGB8
type MandelbrotImage = Image MandelbrotPixel


main :: IO ()
main = do
  (Just hin, _, _, hd) <- createProcess $
    ffmpeg { std_in = CreatePipe, std_out = Inherit, std_err = NoStream }
  renderFrames' @Double hin defaultRenderConfig
  _ <- waitForProcess hd
  pure ()

defaultRenderConfig :: RealFloat a => RenderConfig a
defaultRenderConfig = MkImageConfig
  { imageWidth  = 960
  , imageHeight = 540
  , maxIterations = 2500
  , frameAmount = 500
  , startingWidth = 4
  , endingWidth   = 0.0000000000001
  , center = (-1.643388009235943) :+ 0.0000000018501
  }

renderFrames' :: RealFloat a => Handle -> RenderConfig a -> IO ()
renderFrames' hIn conf@MkImageConfig{..} = do
    mvars <- go
    forM_ mvars $ \(mvar, n) -> do
        x <- readMVar mvar
        B.hPutStr hIn x
        info conf "wrote" n
  where
    go = forM [0 .. frameAmount] $ \n -> do
      res <- newEmptyMVar
      _ <- forkIO $ do
          info conf "starting to render" n
          let !image_data = toStrict . encodePng $ makeImage conf n
          info conf "finished rendering" n
          putMVar res image_data
      pure (res, n)

info :: Show b => RenderConfig a -> String -> b -> IO ()
info MkImageConfig{..} str x = putStrLn $ concat [str, ": ", show x, "/", show frameAmount]

ffmpeg :: CreateProcess
ffmpeg = proc "ffmpeg"
  [ "-y"
  , "-i", "-"
  , "-c:v", "libx264"
  , "-vf", "format=yuv420p"
  , "-r", "60"
  , "-movflags", "+faststart"
  , "out.mp4"]

data IterationResult
  = Escaped Natural
  | Contained
  deriving Show

iterateMandelbrot' :: (Num a, Ord a) => Natural -> Complex a -> IterationResult
iterateMandelbrot' max_iter (x0 :+ y0) = go 0 0 0 0 0
  where
    go !iter x y x2 y2
      | iter >= max_iter = Contained
      | x2 + y2 >= 4     = Escaped iter
      | otherwise        = go (iter + 1) x' y' (x' * x') (y' * y')
        where y' = 2 * x * y + y0
              x' = x2 - y2 + x0

data RenderConfig a = MkImageConfig
  { imageWidth    :: Int
  , imageHeight   :: Int
  , maxIterations :: Natural
  , frameAmount   :: Natural
  , startingWidth :: a
  , endingWidth   :: a
  , center        :: Complex a
  }

makeImage :: RealFloat a
  => RenderConfig a   -- ^ image config
  -> Natural          -- ^ frame
  -> MandelbrotImage
makeImage conf@MkImageConfig{..} frame = generateImage per_pixel imageWidth imageHeight
  where
    width = translateFrame conf frame
    ratio = fromIntegral imageHeight / fromIntegral imageWidth
    step = width / fromIntegral imageWidth
    half_w = width / 2
    half_step = step / 3
    tl = center - (half_w :+ half_w * ratio)
    gray_factor = maxBound `div` fromIntegral maxIterations
    per_pixel x y = let tl_center = tl + (fromIntegral x * step :+ fromIntegral y * step)
                     in combine . map per_value $ [tl_center + (x' :+ y') | let l = [-half_step, half_step], x' <- l, y' <- l]
                     -- in per_value tl_center
    per_value z = case iterateMandelbrot' maxIterations z of
      Escaped n -> gray (gray_factor * fromIntegral n)
      Contained -> black

translateFrame :: Floating a => RenderConfig a -> Natural -> a
translateFrame MkImageConfig{..} x
  = startingWidth * ((endingWidth / startingWidth) ** (fromIntegral x / fromIntegral frameAmount))

combine :: (Pixel a, Integral (PixelBaseComponent a)) => [a] -> a
combine = foldr1 (mixWith (\_ x y -> (x + y) `div` 2))

black :: MandelbrotPixel
black = PixelRGB8 0 0 0

gray :: PixelBaseComponent MandelbrotPixel -> MandelbrotPixel
gray n = PixelRGB8 n 0 n

