/***************************************************************************
 # Copyright (c) 2015-21, NVIDIA CORPORATION. All rights reserved.
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
#pragma once
#include "Falcor.h"
#include "RenderPasses/Shared/PathTracer/PathTracer.h"
#include "RenderPasses/Shared/IncrementalRenderingData.h"

using namespace Falcor;

/** Forward path tracer using a megakernel in DXR 1.0.

    The path tracer has a loop over the path vertices in the raygen shader.
    The kernel terminates when all paths have terminated.

    This pass implements a forward path tracer with next-event estimation,
    Russian roulette, and multiple importance sampling (MIS) with sampling
    of BRDFs and light sources.
*/
class MegakernelPathTracer : public PathTracer
{
public:
    using SharedPtr = std::shared_ptr<MegakernelPathTracer>;

    static const Info kInfo;

    static SharedPtr create(RenderContext* pRenderContext, const Dictionary& dict);

    virtual void setScene(RenderContext* pRenderContext, const Scene::SharedPtr& pScene) override;
    virtual void execute(RenderContext* pRenderContext, const RenderData& renderData) override;
    virtual void renderUI(Gui::Widgets& widget) override;
    virtual void setMethod(uint32_t method) override;

private:
    MegakernelPathTracer(const Dictionary& dict);

    void recreateVars() override { mTracer.pVars = nullptr; }
    void prepareVars();
    void setTracerData(const RenderData& renderData);
    void buildSpiralQueue(uint2 point_of_change, uint2 gridDim);

    ComputeProgram::SharedPtr   mpProgram;   ///< Accumulation programs, one per mode.
    ComputeVars::SharedPtr      mpVars;                         ///< Program variables.
    ComputeState::SharedPtr     mpState;

    Texture::SharedPtr blockTex;
    Texture::SharedPtr reduceTex;
    Texture::SharedPtr accumTex;

    std::vector<int2> blockUpdates;
    std::vector<int2> emptyUpdates;
    uint tileSize = 16;
    bool mTileSizeChanged = false;
    uint highSamples = 64;
    uint renderSamples = 1;
    std::queue<int2> tileQueue;
    std::queue<int2> baseQueue;
    std::queue<int2> spiralQueue;

    bool reset;
    bool mIncrementalEnabled = true;

    // Ray tracing program.
    struct
    {
        RtProgram::SharedPtr pProgram;
        RtBindingTable::SharedPtr pBindingTable;
        RtProgramVars::SharedPtr pVars;
        ParameterBlock::SharedPtr pParameterBlock;      ///< ParameterBlock for all data.
    } mTracer;
};
