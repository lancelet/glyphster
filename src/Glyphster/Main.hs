{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
module Glyphster.Main (main) where

import Codec.Picture (PixelRGB8 (PixelRGB8))
import qualified Codec.Picture as Picture
import Codec.Picture.Types (Image)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)
import Glyphster.App (App)
import qualified Glyphster.App as App
import Glyphster.ComputeTest (render)
import Glyphster.VulkanManager (createAllocator, createDevice, createInstance)

main :: IO ()
main = runResourceT $ do
  lift . putStrLn $ "Glyphster Start"
  inst <- createInstance
  (phys, _pdi, dev) <- createDevice inst
  allocator <- createAllocator inst phys dev

  App.runApp inst phys dev allocator app

  lift . putStrLn $ "Finished"

app :: App ()
app = do
  imageFloat <- render
  let imageInt :: Image PixelRGB8
      imageInt =
        Picture.pixelMap
          ( \f ->
              let w8 :: Word8
                  w8 = ceiling (f * 255)
               in PixelRGB8 w8 w8 w8
          )
          imageFloat
  liftIO $ LBS.writeFile "test.png" (Picture.encodePng imageInt)
