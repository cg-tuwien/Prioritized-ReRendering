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
#include "SharedTypes.slang"

using namespace Falcor;

class InteractionPass : public RenderPass
{
public:
    using SharedPtr = std::shared_ptr<InteractionPass>;

    static const Info kInfo;

    /** Create a new render pass object.
        \param[in] pRenderContext The render context.
        \param[in] dict Dictionary of serialized parameters.
        \return A new object, or an exception is thrown if creation failed.
    */
    static SharedPtr create(RenderContext* pRenderContext = nullptr, const Dictionary& dict = {});

    virtual Dictionary getScriptingDictionary() override;
    virtual RenderPassReflection reflect(const CompileData& compileData) override;
    virtual void compile(RenderContext* pContext, const CompileData& compileData) override;
    virtual void execute(RenderContext* pRenderContext, const RenderData& renderData) override;
    void setSelectedPixelToObjectCenter();
    virtual void renderUI(Gui::Widgets& widget) override;
    virtual void setScene(RenderContext* pRenderContext, const Scene::SharedPtr& pScene) override;
    virtual bool onMouseEvent(const MouseEvent& mouseEvent) override;
    virtual bool onKeyEvent(const KeyboardEvent& keyEvent) override { return false; }

    bool getReset() { return reset; }
    void animate() {
        logInfo("animate");
        selectedObj[0].mTranslation = selectedObj[0].mTranslation + float3(0, 0.005, 0);
        mUserChangedScene = true;
    }
    bool shouldAnimate() {
        return mAnimate;
    }
    void stopAnimation() { mAnimate = false; }

private:
    InteractionPass();

    struct InteractableObject
    {
        float3 mTranslation;
        float3 mScaling;
        float3 mRotation;

        PixelData mpPixelData; ///< pixel data mapped from buffer
    };

    struct TransformMultiple
    {
        float3 average;
        float3 current;
        std::vector<float3> uponSelection;

        void add(float3 value, float num)
        {
            uponSelection.push_back(value);
            average += value;
            current = average / num;
        }

        void clear()
        {
            uponSelection.clear();
            average = float3(0);
            current = float3(0);
        }
    };

    bool mEnabled = true;
    bool reset = false;
    bool mAnimate = false;

    Scene::SharedPtr mpScene;
    InteractionPassParams mParams;
    GpuFence::SharedPtr mpFence;
    ComputePass::SharedPtr mpInteractionPass;
    Buffer::SharedPtr mpPixelDataBuffer;      ///< Buffer for recording pixel data at the selected pixel.
    Buffer::SharedPtr mpPixelDataStaging;     ///< Readback buffer.

    std::vector<InteractableObject> selectedObj;

    Gui::DropdownList matValList;
    std::map<uint32_t, glm::vec3> transValList;

    bool mPixelDataAvailable = false;
    bool mRightMouseClicked = false;
    bool mUserChangedScene = false;
    bool backgroundPixelSelected = false;

    TransformMultiple mTranslation;
    TransformMultiple mScaling;
    TransformMultiple mRotation;
};
