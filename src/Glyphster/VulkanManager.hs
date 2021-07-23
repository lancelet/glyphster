{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
module Glyphster.VulkanManager
  ( -- * Types
    PhysicalDeviceInfo (..),

    -- * Functions
    createInstance,
    createDevice,
    createAllocator,
  )
where

import Control.Exception.Safe (MonadThrow, throwString)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Bits (Bits, zeroBits, (.&.))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Extra (fromMaybeM)
import Say (sayErr)
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core11 as Vk (pattern API_VERSION_1_1)
import qualified Vulkan.Extensions.VK_KHR_portability_subset as Vk
import qualified Vulkan.Utils.Initialization as VkUtil
import qualified Vulkan.Zero as Vk
import qualified VulkanMemoryAllocator as VMA

-------------------------------------------------------------------------------
-- Instance
-------------------------------------------------------------------------------

apiVersion :: Word32
apiVersion = Vk.API_VERSION_1_1

-- | Create a Vulkan instance with a debug messenger.
createInstance :: MonadResource m => m Vk.Instance
createInstance = VkUtil.createDebugInstanceFromRequirements [] [] ici
  where
    ici :: Vk.InstanceCreateInfo _
    ici =
      Vk.zero
        { Vk.applicationInfo =
            Just
              Vk.zero
                { Vk.applicationName = Just "glyphster",
                  Vk.apiVersion = apiVersion
                }
        }

-------------------------------------------------------------------------------
-- Device
-------------------------------------------------------------------------------

createDevice ::
  (MonadResource m, MonadThrow m) =>
  Vk.Instance ->
  m (Vk.PhysicalDevice, PhysicalDeviceInfo, Vk.Device)
createDevice inst = do
  (pdi, physDevice) <-
    fromMaybeM
      (throwString "Unable to find appropriate PhysicalDevice")
      (VkUtil.pickPhysicalDevice inst physicalDeviceInfo id)
  sayErr . ("Using device: " <>) =<< VkUtil.physicalDeviceName physDevice

  let deviceCreateInfo :: Vk.DeviceCreateInfo _
      deviceCreateInfo =
        Vk.zero
          { Vk.queueCreateInfos =
              [ SomeStruct
                  Vk.zero
                    { Vk.queueFamilyIndex = pdiComputeQueueFamilyIndex pdi,
                      Vk.queuePriorities = [1]
                    }
              ],
            Vk.enabledExtensionNames =
              [ Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME
              ]
          }

  (_, dev) <- Vk.withDevice physDevice deviceCreateInfo Nothing allocate
  pure (physDevice, pdi, dev)

-------------------------------------------------------------------------------
-- Physical Device
-------------------------------------------------------------------------------

data PhysicalDeviceInfo = PhysicalDeviceInfo
  { -- | Total memory of the device.
    pdiTotalMemory :: Word64,
    -- | Queue family index of the first compute queue.
    pdiComputeQueueFamilyIndex :: Word32
  }
  deriving (Eq, Ord)

physicalDeviceInfo ::
  MonadIO m =>
  Vk.PhysicalDevice ->
  m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo physDevice = runMaybeT $ do
  pdiTotalMemory <- do
    heaps <- Vk.memoryHeaps <$> Vk.getPhysicalDeviceMemoryProperties physDevice
    pure $ sum ((Vk.size :: Vk.MemoryHeap -> Vk.DeviceSize) <$> heaps)

  pdiComputeQueueFamilyIndex <- do
    qFamProp <- Vk.getPhysicalDeviceQueueFamilyProperties physDevice

    let isComputeQ :: Vk.QueueFamilyProperties -> Bool
        isComputeQ q =
          (Vk.QUEUE_COMPUTE_BIT .&&. Vk.queueFlags q) && (Vk.queueCount q > 0)

        computeQueueIndices :: Vector Word32
        computeQueueIndices =
          fromIntegral
            . fst
            <$> V.filter
              (isComputeQ . snd)
              (V.indexed qFamProp)

    MaybeT (pure $ computeQueueIndices V.!? 0)

  pure PhysicalDeviceInfo {..}

-------------------------------------------------------------------------------
-- Allocator
-------------------------------------------------------------------------------

createAllocator ::
  (MonadResource m) =>
  Vk.Instance ->
  Vk.PhysicalDevice ->
  Vk.Device ->
  m VMA.Allocator
createAllocator inst phys dev = do
  (_, allocator) <-
    VMA.withAllocator
      Vk.zero
        { VMA.flags = Vk.zero,
          VMA.physicalDevice = Vk.physicalDeviceHandle phys,
          VMA.device = Vk.deviceHandle dev,
          VMA.instance' = Vk.instanceHandle inst,
          VMA.vulkanApiVersion = apiVersion
        }
      allocate
  pure allocator

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
