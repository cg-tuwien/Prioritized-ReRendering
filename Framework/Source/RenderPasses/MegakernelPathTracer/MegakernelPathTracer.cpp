/***************************************************************************
 # Copyright (c) 2015-22, NVIDIA CORPORATION. All rights reserved.
 #
 # Redistribution and use in source and binary forms, with or without
 # modification, are permitted provided that the following conditions
 # are met:
 #  * Redistributions of source code must retain the above copyright
 #    notice, this list of conditions and the following disclaimer.
 #  * Redistributions in binary form must reproduce the above copyright
 #    notice, this list of conditions and the following disclaimer in the
 #    documentation and/or other materials provided with the distribution.
 #  * Neither the name of NVIDIA CORPORATION nor the names of its
 #    contributors may be used to endorse or promote products derived
 #    from this software without specific prior written permission.
 #
 # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY
 # EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 # IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 # PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 # CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 # PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 # PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 # OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 # (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 # OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 **************************************************************************/
#include "MegakernelPathTracer.h"
#include "RenderGraph/RenderPassHelpers.h"
#include "Scene/HitInfo.h"
#include <sstream>

const RenderPass::Info MegakernelPathTracer::kInfo{ "MegakernelPathTracer", "Megakernel path tracer." };

namespace
{
    const char kShaderFile[] = "RenderPasses/MegakernelPathTracer/PathTracer.rt.slang";
    const char kReduceFile[] = "RenderPasses/MegakernelPathTracer/Reduce.slang";
    const char kParameterBlockName[] = "gData";
    //Point on the screen where a change occured. The path-tracer should update the image starting from this point.
    const char point_of_change[] = "point_of_change";
    const char change_occured[] = "change_occured";
    //Clearmode for accumulate pass
    const char clear_mode[] = "clear_mode";
    const char high_samples[] = "high_samples";

    // Ray tracing settings that affect the traversal stack size.
    // These should be set as small as possible.
    // The payload for the scatter rays is 8-12B.
    // The payload for the shadow rays is 4B.
    const uint32_t kMaxPayloadSizeBytes = HitInfo::kMaxPackedSizeInBytes;
    const uint32_t kMaxAttributeSizeBytes = 8;
    const uint32_t kMaxRecursionDepth = 1;

    // Render pass output channels.
    const std::string kColorOutput = "color";
    const std::string kAlbedoOutput = "albedo";
    const std::string kTimeOutput = "time";

    const Falcor::ChannelList kOutputChannels =
    {
        { kColorOutput,     "gOutputColor",               "Output color (linear)", true /* optional */, ResourceFormat::RGBA32Float },
        { kAlbedoOutput,    "gOutputAlbedo",              "Surface albedo (base color) or background color", true /* optional */, ResourceFormat::RGBA32Float },
        { kTimeOutput,      "gOutputTime",                "Per-pixel execution time", true /* optional */, ResourceFormat::R32Uint },
    };

    const Gui::DropdownList kRepValList =
    {
        { (uint)4, "4" },
        { (uint)8, "8" },
        { (uint)16, "16" },
        { (uint)32, "32" },
        { (uint)64, "64" },
        { (uint)128, "128" },
        { (uint)256, "256" },
        { (uint)512, "512" },
    };

    const Gui::DropdownList kSizeValList =
    {
        { (uint)8, "8" },
        { (uint)16, "16" },
        { (uint)32, "32" },
    };
};

// Don't remove this. it's required for hot-reload to function properly
extern "C" FALCOR_API_EXPORT const char* getProjDir()
{
    return PROJECT_DIR;
}

extern "C" FALCOR_API_EXPORT void getPasses(Falcor::RenderPassLibrary & lib)
{
    lib.registerPass(MegakernelPathTracer::kInfo, MegakernelPathTracer::create);
}

MegakernelPathTracer::SharedPtr MegakernelPathTracer::create(RenderContext* pRenderContext, const Dictionary& dict)
{
    return SharedPtr(new MegakernelPathTracer(dict));
}

MegakernelPathTracer::MegakernelPathTracer(const Dictionary& dict)
    : PathTracer(kInfo, dict, kOutputChannels)
{
    mRenderUI = true;
}

void MegakernelPathTracer::setScene(RenderContext* pRenderContext, const Scene::SharedPtr& pScene)
{
    PathTracer::setScene(pRenderContext, pScene);

    if (mpScene)
    {
        if (mpScene->hasProceduralGeometry())
        {
            logWarning("MegakernelPathTracer: This render pass only supports triangles. Other types of geometry will be ignored.");
        }

        Program::DefineList defines;
        defines.add(mpScene->getSceneDefines());
        defines.add("MAX_BOUNCES", std::to_string(mSharedParams.maxBounces));
        defines.add("SAMPLES_PER_PIXEL", std::to_string(mSharedParams.samplesPerPixel));
        defines.add("TILE_SIZE", std::to_string(tileSize));

        // Create ray tracing program.
        RtProgram::Desc desc;
        desc.addShaderLibrary(kShaderFile);
        desc.setMaxPayloadSize(kMaxPayloadSizeBytes);
        desc.setMaxAttributeSize(kMaxAttributeSizeBytes);
        desc.setMaxTraceRecursionDepth(kMaxRecursionDepth);

        mTracer.pBindingTable = RtBindingTable::create(2, 2, mpScene->getGeometryCount());
        auto& sbt = mTracer.pBindingTable;
        sbt->setRayGen(desc.addRayGen("rayGen"));
        sbt->setMiss(kRayTypeScatter, desc.addMiss("scatterMiss"));
        sbt->setMiss(kRayTypeShadow, desc.addMiss("shadowMiss"));
        sbt->setHitGroup(kRayTypeScatter, mpScene->getGeometryIDs(Scene::GeometryType::TriangleMesh), desc.addHitGroup("scatterClosestHit", "scatterAnyHit"));
        sbt->setHitGroup(kRayTypeShadow, mpScene->getGeometryIDs(Scene::GeometryType::TriangleMesh), desc.addHitGroup("", "shadowAnyHit"));

        mTracer.pProgram = RtProgram::create(desc, defines);

        uint2 gridDim = uint2(uint2(1920 + tileSize - 1, 1080 + tileSize - 1) / uint2(tileSize, tileSize));
        blockTex = Texture::create2D(gridDim.x, gridDim.y, Falcor::ResourceFormat::RG32Uint, 1, 1);
        reduceTex = Texture::create2D(1920, 1080, Falcor::ResourceFormat::RGBA32Float, 1, 1, 0, Falcor::ResourceBindFlags::UnorderedAccess);

        emptyUpdates = std::vector<int2>(gridDim.x * gridDim.y, int2(-1, -1));

        // Note only compensated summation needs precise floating-point mode.
        auto defs = Program::DefineList();
        defs.add("TILE_SIZE", std::to_string(tileSize));
        mpProgram = ComputeProgram::createFromFile(kReduceFile, "reduce", defs, Shader::CompilerFlags::TreatWarningsAsErrors);
        mpVars = ComputeVars::create(mpProgram->getReflector());
        mpState = ComputeState::create();
    }
}

void MegakernelPathTracer::execute(RenderContext* pRenderContext, const RenderData& renderData)
{
    pRenderContext->flush(true);

    // Call shared pre-render code.
    if (!beginFrame(pRenderContext, renderData)) return;

    //uint2 gridDim = uint2(uint2(1920 + tileSize - 1, 1080 + tileSize - 1) / uint2(tileSize, tileSize));
    uint2 gridDim = uint2(renderData.getDefaultTextureDims()) / uint2(tileSize, tileSize);
    uint maxPossible = gridDim.x * gridDim.y;

    if (mTileSizeChanged)
    {
        std::queue<int2> empty;
        std::swap(baseQueue, empty);
        emptyUpdates = std::vector<int2>(gridDim.x * gridDim.y, int2(-1, -1));

        // Note only compensated summation needs precise floating-point mode.
        mpProgram->addDefine("TILE_SIZE", std::to_string(tileSize));
        mTracer.pProgram->addDefine("TILE_SIZE", std::to_string(tileSize));

        blockTex = Texture::create2D(gridDim.x, gridDim.y, Falcor::ResourceFormat::RG32Uint, 1, 1);
    }

    if (baseQueue.empty())
    {
        for (uint y = 0; y < gridDim.y; y++)
        {
            for (uint x = 0; x < gridDim.x; x++)
            {
                baseQueue.push(uint2(x, y));
            }
        }
    }

    InternalDictionary& dict = renderData.getDictionary();

    if (dict.keyExists(point_of_change) && dict[change_occured])
    {
        if (mIncrementalEnabled)
        {
            buildSpiralQueue(dict[point_of_change], gridDim);
            tileQueue = spiralQueue;
            renderSamples = highSamples;
            dict[clear_mode] = (int)ClearMode::Incremental;
            dict[high_samples] = (int)highSamples;
        }
        else
        {
            tileQueue = baseQueue;
            renderSamples = 1;
            dict[clear_mode] = (int)ClearMode::Reset;
        }
    }

    if (tileQueue.empty())
    {
        tileQueue = baseQueue;
        renderSamples = 1;
        dict[clear_mode] = (int)ClearMode::ContinuousRefinement;
    }

    if (mpScene)
    {
        auto sceneUpdates = mpScene->getUpdates();
        if (is_set(sceneUpdates, Scene::UpdateFlags::CameraPropertiesChanged))
        {
            auto excluded = Camera::Changes::Jitter | Camera::Changes::History;
            auto cameraChanges = mpScene->getCamera()->getChanges();
            if ((cameraChanges & ~excluded) != Camera::Changes::None)
            {
                tileQueue = baseQueue;
                renderSamples = 1;
                dict[clear_mode] = (int)ClearMode::Reset;
            }
        }
    }

    blockUpdates = emptyUpdates;
    int tilesServed = 0;
    uint2 blockMax = uint2(0, 0);
    uint2 blockMin = uint2(9999, 9999);
    for (uint b = 0; (b + renderSamples) <= maxPossible; b += renderSamples)
    {
        if (tileQueue.empty())
            break;

        uint2 block = tileQueue.front();
        if (block.x > blockMax.x) blockMax.x = block.x;
        if (block.y > blockMax.y) blockMax.y = block.y;
        if (block.x < blockMin.x) blockMin.x = block.x;
        if (block.y < blockMin.y) blockMin.y = block.y;
        tileQueue.pop();
        blockUpdates[tilesServed] = block;
        tilesServed++;
    }
    dict["blockMax"] = blockMax;
    dict["blockMin"] = blockMin;
    dict["tileSize"] = tileSize;

    uint3 region = uint3(gridDim, 1);
    pRenderContext->updateSubresourceData(blockTex.get(), 0, blockUpdates.data(), Falcor::uint3(0, 0, 0), region);

    // Set compile-time constants.
    RtProgram::SharedPtr pProgram = mTracer.pProgram;
    setStaticParams(pProgram.get());

    // For optional I/O resources, set 'is_valid_<name>' defines to inform the program of which ones it can access.
    // TODO: This should be moved to a more general mechanism using Slang.
    pProgram->addDefines(getValidResourceDefines(mInputChannels, renderData));
    pProgram->addDefines(getValidResourceDefines(mOutputChannels, renderData));

    if (mUseEmissiveSampler)
    {
        // Specialize program for the current emissive light sampler options.
        FALCOR_ASSERT(mpEmissiveSampler);
        if (pProgram->addDefines(mpEmissiveSampler->getDefines())) mTracer.pVars = nullptr;
    }

    // Prepare program vars. This may trigger shader compilation.
    // The program should have all necessary defines set at this point.
    if (!mTracer.pVars) prepareVars();
    FALCOR_ASSERT(mTracer.pVars);

    // Set shared data into parameter block.
    setTracerData(renderData);

    // Bind I/O buffers. These needs to be done per-frame as the buffers may change anytime.
    auto bind = [&](const ChannelDesc& desc)
    {
        if (!desc.texname.empty())
        {
            auto var = mTracer.pVars->getRootVar();
            var[desc.texname] = renderData[desc.name]->asTexture();
        }
    };
    for (auto channel : mInputChannels) bind(channel);
    for (auto channel : mOutputChannels) bind(channel);

    mTracer.pVars->getRootVar()["gPixelMap"] = blockTex;
    mTracer.pVars->getRootVar()["PerFrameCB"]["RENDER_SAMPLES"] = renderSamples;

    // Get dimensions of ray dispatch.
    const uint2 targetDim = renderData.getDefaultTextureDims();
    FALCOR_ASSERT(targetDim.x > 0 && targetDim.y > 0);

    mpPixelDebug->prepareProgram(pProgram, mTracer.pVars->getRootVar());
    mpPixelStats->prepareProgram(pProgram, mTracer.pVars->getRootVar());

    // Spawn the rays.
    {
        FALCOR_PROFILE("MegakernelPathTracer::execute()_RayTrace");
        mpScene->raytrace(pRenderContext, mTracer.pProgram.get(), mTracer.pVars, uint3(targetDim, 1));
    }

    mpVars["PerFrameCB"]["gBlockGrid"] = gridDim;
    mpVars["PerFrameCB"]["gRenderSamples"] = renderSamples;
    mpVars["gFunTex"] = reduceTex;
    mpVars["gPixelMap"] = blockTex;
    mpVars["gInOutFrame"] = renderData["color"]->asTexture();

    pRenderContext->flush(true);
    mpState->setProgram(mpProgram);
    for (int r = renderSamples / 2; r > 0; r /= 2)
    {
        pRenderContext->uavBarrier(reduceTex.get());
        mpVars["PerFrameCB"]["gBlockDist"] = r;
        pRenderContext->dispatch(mpState.get(), mpVars.get(), uint3(tilesServed * r, 1, 1));
    }

    // Call shared post-render code.
    endFrame(pRenderContext, renderData);
}

void MegakernelPathTracer::buildSpiralQueue(uint2 point_of_change, uint2 gridDim)
{
    uint2 grid_PoC = point_of_change / uint2(tileSize, tileSize);

    std::queue<int2> empty;
    spiralQueue.swap(empty);

    spiralQueue.push(grid_PoC);

    int x = grid_PoC.x;
    int y = grid_PoC.y;
    int steps = 1;
    int direction = 0;

    while (spiralQueue.size() < gridDim.x * gridDim.y)
    {
        for (int j = 0; j < steps; j++) {
            switch (direction)
            {
            case 0: x++; break; //RIGHT
            case 1: y++; break; //DOWN
            case 2: x--; break; //LEFT
            case 3: y--; break; //UP
            }

            // unoptimal solution for the fact that the PoC may not be centered
            // and the grid may not be symmetrical
            // TODO: optimize this
            if (x >= 0 && y >= 0 && x < (int)gridDim.x && y < (int)gridDim.y)
            {
                spiralQueue.push(uint2(x, y));
            }
        }
        direction = (direction + 1) % 4;

        // every two turns the step size increases
        if ((direction % 2) == 0)
            steps++;
    }
}

void MegakernelPathTracer::prepareVars()
{
    FALCOR_ASSERT(mTracer.pProgram);

    // Specialize the program.
    mTracer.pProgram->addDefines(mpSampleGenerator->getDefines());

    // Configure scene types.
    mTracer.pProgram->setTypeConformances(mpScene->getTypeConformances());

    // Create program variables for the current program.
    // This may trigger shader compilation. If it fails, throw an exception to abort rendering.
    mTracer.pVars = RtProgramVars::create(mTracer.pProgram, mTracer.pBindingTable);

    // Bind utility classes into shared data.
    auto var = mTracer.pVars->getRootVar();
    mpSampleGenerator->setShaderData(var);

    // Create parameter block for shared data.
    ProgramReflection::SharedConstPtr pReflection = mTracer.pProgram->getReflector();
    ParameterBlockReflection::SharedConstPtr pBlockReflection = pReflection->getParameterBlock(kParameterBlockName);
    FALCOR_ASSERT(pBlockReflection);
    mTracer.pParameterBlock = ParameterBlock::create(pBlockReflection);
    FALCOR_ASSERT(mTracer.pParameterBlock);

    // Bind static resources to the parameter block here. No need to rebind them every frame if they don't change.
    // Bind the light probe if one is loaded.
    if (mpEnvMapSampler) mpEnvMapSampler->setShaderData(mTracer.pParameterBlock["envMapSampler"]);

    // Bind the parameter block to the global program variables.
    mTracer.pVars->setParameterBlock(kParameterBlockName, mTracer.pParameterBlock);
}

void MegakernelPathTracer::setTracerData(const RenderData& renderData)
{
    auto pBlock = mTracer.pParameterBlock;
    FALCOR_ASSERT(pBlock);

    // Upload parameters struct.
    pBlock["params"].setBlob(mSharedParams);

    // Bind emissive light sampler.
    if (mUseEmissiveSampler)
    {
        FALCOR_ASSERT(mpEmissiveSampler);
        mpEmissiveSampler->setShaderData(pBlock["emissiveSampler"]);
    }
}

void MegakernelPathTracer::renderUI(Gui::Widgets& widget)
{
#if DEBUG_UI
    widget.checkbox("Incremental Update?", mIncrementalEnabled);
#endif
    if (mIncrementalEnabled)
    {
        widget.dropdown("Tile Quality", kRepValList, highSamples, false);
        mTileSizeChanged = widget.dropdown("Tile Size", kSizeValList, tileSize, false);
    }
#if DEBUG_UI
    PathTracer::renderUI(widget);
#endif
}

void MegakernelPathTracer::setMethod(uint32_t method)
{
    switch (method)
    {
    case 0:
        mIncrementalEnabled = false;
        break;
    case 1:
        mIncrementalEnabled = false;
        break;
    case 2:
        mIncrementalEnabled = true;
        break;
    case 3:
        mIncrementalEnabled = true;
        break;
    default:
        break;
    }
}
