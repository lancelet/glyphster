{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
module Glyphster.ComputeTest where

import Codec.Picture (Image)
import qualified Codec.Picture as Picture
import Control.Exception.Safe (throwString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate)
import Data.ByteString (ByteString)
import Data.Word (Word32)
import Foreign (Ptr, Storable (peek), castPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Glyphster.App (App)
import qualified Glyphster.App as App
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Utils.ShaderQQ.GLSL.Glslang as Glslang
import qualified Vulkan.Zero as Vk
import qualified VulkanMemoryAllocator as VMA

render :: App (Image Float)
render = do
  device <- App.getDevice

  let width, height, workgroupX, workgroupY :: Int
      width = 512
      height = 512
      workgroupX = 32
      workgroupY = 4

      szBytes :: Int
      szBytes = width * height * sizeOf (0 :: Float)

  (buf, bufAlloc, bufAllocInfo) <- createBuffer szBytes
  (descriptorSet, descriptorSetLayout) <- createDescriptorSet

  -- Assign buffer to the descriptor set
  let wds :: Vk.SomeStruct Vk.WriteDescriptorSet
      wds =
        Vk.SomeStruct
          Vk.zero
            { Vk.dstSet = descriptorSet,
              Vk.dstBinding = 0,
              Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER,
              Vk.descriptorCount = 1,
              Vk.bufferInfo = [Vk.DescriptorBufferInfo buf 0 Vk.WHOLE_SIZE]
            }
  Vk.updateDescriptorSets device [wds] []

  (pipeline, pipelineLayout) <- createComputePipeline descriptorSetLayout
  commandBuffer <- createCommandBuffer
  fillCommandBuffer
    commandBuffer
    pipeline
    pipelineLayout
    descriptorSet
    width
    height
    workgroupX
    workgroupY
  executeAndBlock commandBuffer

  -- make sure the buffer changes are present on the host
  allocator <- App.getAllocator
  VMA.invalidateAllocation allocator bufAlloc 0 Vk.WHOLE_SIZE

  -- copy into a JuicyPixels image
  let bufAddr :: Ptr Float
      bufAddr = castPtr (VMA.mappedData bufAllocInfo)

  liftIO $
    Picture.withImage
      width
      height
      ( \x y ->
          let ofs :: Ptr Float
              ofs = plusPtr bufAddr (((y * width) + x) * sizeOf (0 :: Float))
           in peek ofs
      )

createBuffer :: Int -> App (Vk.Buffer, VMA.Allocation, VMA.AllocationInfo)
createBuffer szBytes = do
  allocator <- App.getAllocator

  let bci :: Vk.BufferCreateInfo '[]
      bci =
        Vk.zero
          { Vk.size = fromIntegral szBytes,
            Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
          }

      aci :: VMA.AllocationCreateInfo
      aci =
        Vk.zero
          { -- Memory is persistently mapped.
            VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT,
            -- Resources written by GPU, read by CPU.
            VMA.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
          }

  (_, (buf, bufAlloc, bufAllocInfo)) <-
    VMA.withBuffer allocator bci aci allocate

  pure (buf, bufAlloc, bufAllocInfo)

createDescriptorSet :: App (Vk.DescriptorSet, Vk.DescriptorSetLayout)
createDescriptorSet = do
  device <- App.getDevice

  -- create a descriptor pool
  let dpci :: Vk.DescriptorPoolCreateInfo '[]
      dpci =
        Vk.zero
          { Vk.maxSets = 1,
            Vk.poolSizes =
              [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1]
          }
  (_, descriptorPool) <- Vk.withDescriptorPool device dpci Nothing allocate

  -- create a descriptor set and layout for the buffer
  let dslci :: Vk.DescriptorSetLayoutCreateInfo '[]
      dslci =
        Vk.zero
          { Vk.bindings =
              [ Vk.zero
                  { Vk.binding = 0,
                    Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER,
                    Vk.descriptorCount = 1,
                    Vk.stageFlags = Vk.SHADER_STAGE_COMPUTE_BIT
                  }
              ]
          }
  (_, descriptorSetLayout) <-
    Vk.withDescriptorSetLayout device dslci Nothing allocate

  -- allocate a descriptor set with the layout
  let dsai :: Vk.DescriptorSetAllocateInfo '[]
      dsai =
        Vk.zero
          { Vk.descriptorPool = descriptorPool,
            Vk.setLayouts = [descriptorSetLayout]
          }
  [descriptorSet] <- Vk.allocateDescriptorSets device dsai

  pure (descriptorSet, descriptorSetLayout)

createComputePipeline ::
  Vk.DescriptorSetLayout ->
  App (Vk.Pipeline, Vk.PipelineLayout)
createComputePipeline descriptorSetLayout = do
  device <- App.getDevice
  shader <- createShader

  -- create pipelineLayout
  let plci :: Vk.PipelineLayoutCreateInfo
      plci = Vk.zero {Vk.setLayouts = [descriptorSetLayout]}
  (_, pipelineLayout) <- Vk.withPipelineLayout device plci Nothing allocate

  -- create compute pipeline
  let cpci :: Vk.ComputePipelineCreateInfo '[]
      cpci =
        Vk.zero
          { Vk.layout = pipelineLayout,
            Vk.stage = shader,
            Vk.basePipelineHandle = Vk.zero
          }
  (_, (_, [computePipeline])) <-
    Vk.withComputePipelines device Vk.zero [Vk.SomeStruct cpci] Nothing allocate

  pure (computePipeline, pipelineLayout)

createShader :: App (Vk.SomeStruct Vk.PipelineShaderStageCreateInfo)
createShader = do
  let compCode :: ByteString
      compCode =
        [Glslang.comp|
        #version 450

        const int width = 512;
        const int height = 512;
        const int workgroup_x = 32;
        const int workgroup_y = 4;

        layout (
          local_size_x = workgroup_x,
          local_size_y = workgroup_y,
          local_size_z = 1
        ) in;
        layout(binding = 0) buffer buf {
          float imageData[];
        };

        void main() {
          uint x = gl_GlobalInvocationID.x;
          uint y = gl_GlobalInvocationID.y;

          float frac = float(y) / float(height);

          uint i = width * y + x;
          imageData[i] = frac;
        }
      |]

  device <- App.getDevice

  let smci :: Vk.ShaderModuleCreateInfo '[]
      smci = Vk.zero {Vk.code = compCode}
  (_, compModule) <- Vk.withShaderModule device smci Nothing allocate

  let pssci :: Vk.PipelineShaderStageCreateInfo '[]
      pssci =
        Vk.zero
          { Vk.stage = Vk.SHADER_STAGE_COMPUTE_BIT,
            Vk.module' = compModule,
            Vk.name = "main"
          }
  pure $ Vk.SomeStruct pssci

createCommandBuffer :: App Vk.CommandBuffer
createCommandBuffer = do
  device <- App.getDevice
  cqfi <- App.getComputeQueueFamilyIndex

  let cpci :: Vk.CommandPoolCreateInfo
      cpci = Vk.zero {Vk.queueFamilyIndex = cqfi}
  (_, commandPool) <- Vk.withCommandPool device cpci Nothing allocate

  let cbai :: Vk.CommandBufferAllocateInfo
      cbai =
        Vk.zero
          { Vk.commandPool = commandPool,
            Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY,
            Vk.commandBufferCount = 1
          }
  (_, [commandBuffer]) <- Vk.withCommandBuffers device cbai allocate

  pure commandBuffer

fillCommandBuffer ::
  Vk.CommandBuffer ->
  Vk.Pipeline ->
  Vk.PipelineLayout ->
  Vk.DescriptorSet ->
  Int ->
  Int ->
  Int ->
  Int ->
  App ()
fillCommandBuffer
  commandBuffer
  pipeline
  pipelineLayout
  descriptorSet
  width
  height
  workgroupX
  workgroupY =
    do
      let cbbi :: Vk.CommandBufferBeginInfo '[]
          cbbi =
            Vk.zero
              { Vk.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
              }
      Vk.useCommandBuffer commandBuffer cbbi $ do
        -- set up state, pipeline and descriptor set
        Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_COMPUTE pipeline
        Vk.cmdBindDescriptorSets
          commandBuffer
          Vk.PIPELINE_BIND_POINT_COMPUTE
          pipelineLayout
          0
          [descriptorSet]
          []

        -- dispatch the compute shader
        let toFloat :: Real a => a -> Float
            toFloat = realToFrac

            gcx, gcy :: Word32
            gcx = ceiling (toFloat width / toFloat workgroupX)
            gcy = ceiling (toFloat height / toFloat workgroupY)

        Vk.cmdDispatch commandBuffer gcx gcy 1

executeAndBlock :: Vk.CommandBuffer -> App ()
executeAndBlock commandBuffer = do
  device <- App.getDevice
  -- create a fence so we can know when the render is finished
  (_, fence) <- Vk.withFence device Vk.zero Nothing allocate
  -- submit the command buffer and wait for it to execute
  cqfi <- App.getComputeQueueFamilyIndex
  computeQueue <- Vk.getDeviceQueue device cqfi 0
  let si :: Vk.SubmitInfo '[]
      si =
        Vk.zero
          { Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]
          }
  Vk.queueSubmit computeQueue [Vk.SomeStruct si] fence
  let fenceTimeout = ceiling (1e9 :: Double) -- 1 second
  result <- Vk.waitForFences device [fence] True fenceTimeout
  case result of
    Vk.TIMEOUT -> throwString "Timed out waiting for compute"
    _ -> pure ()
