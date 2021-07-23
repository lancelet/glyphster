{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- |
module Glyphster.App
  ( -- * Types
    App,
    GlobalHandles,

    -- * Functions
    runApp,
    getInstance,
    getPhysicalDevice,
    getDevice,
    getAllocator,
    getComputeQueueFamilyIndex,
  )
where

import Control.Exception.Safe (MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow, ResourceT)
import Data.Word (Word32)
import qualified Vulkan as Vk
import qualified VulkanMemoryAllocator as VMA

-- | @App@ holds global handles and tracks resource management.
newtype App a = App {unApp :: ReaderT GlobalHandles (ResourceT IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadIO,
      MonadResource
    )

-- | Global handles for the app.
data GlobalHandles = GlobalHandles
  { ghInstance :: Vk.Instance,
    ghPhysicalDevice :: Vk.PhysicalDevice,
    ghDevice :: Vk.Device,
    ghAllocator :: VMA.Allocator,
    ghComputeQueueFamilyIndex :: Word32
  }

-- | Run the app, supplying fields for the reader.
runApp ::
  Vk.Instance ->
  Vk.PhysicalDevice ->
  Vk.Device ->
  VMA.Allocator ->
  App a ->
  ResourceT IO a
runApp ghInstance ghPhysicalDevice ghDevice ghAllocator =
  let ghComputeQueueFamilyIndex = 0
   in flip runReaderT GlobalHandles {..} . unApp

-------------------------------------------------------------------------------
-- Getters for global handles.
-------------------------------------------------------------------------------

getInstance :: App Vk.Instance
getInstance = App (asks ghInstance)

getPhysicalDevice :: App Vk.PhysicalDevice
getPhysicalDevice = App (asks ghPhysicalDevice)

getDevice :: App Vk.Device
getDevice = App (asks ghDevice)

getAllocator :: App VMA.Allocator
getAllocator = App (asks ghAllocator)

getComputeQueueFamilyIndex :: App Word32
getComputeQueueFamilyIndex = App (asks ghComputeQueueFamilyIndex)
