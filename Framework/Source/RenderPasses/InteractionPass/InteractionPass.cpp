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
#include "InteractionPass.h"
#include <glm/gtx/quaternion.hpp>
#include <glm/gtx/string_cast.hpp>

const RenderPass::Info InteractionPass::kInfo{ "InteractionPass", "Enables per frame user Interaction." };

namespace
{
    const char out[] = "dst";
    const char in[] = "src";
    const char kShaderFile[] = "RenderPasses/InteractionPass/InteractionPass.cs.slang";
    const char kShaderModel[] = "6_5";
    //Point on the screen where a change occured. The path-tracer should update the image starting from this point.
    const char point_of_change[] = "point_of_change";
    const char change_occured[] = "change_occured";
    const char background_pixel[] = "background_pixel";

    const std::string kOutput = "output";

    const char mesh_id[] = "mesh_id";
}

// Don't remove this. it's required for hot-reload to function properly
extern "C" FALCOR_API_EXPORT const char* getProjDir()
{
    return PROJECT_DIR;
}

extern "C" FALCOR_API_EXPORT void getPasses(Falcor::RenderPassLibrary & lib)
{
    lib.registerPass(InteractionPass::kInfo, InteractionPass::create);
}

InteractionPass::SharedPtr InteractionPass::create(RenderContext* pRenderContext, const Dictionary& dict)
{
    SharedPtr pPass = SharedPtr(new InteractionPass());
    return pPass;
}

Dictionary InteractionPass::getScriptingDictionary()
{
    return Dictionary();
}

InteractionPass::InteractionPass() : RenderPass(kInfo)
{
    if (!gpDevice->isFeatureSupported(Device::SupportedFeatures::RaytracingTier1_1))
    {
        throw std::exception("Raytracing Tier 1.1 is not supported by the current device");
    }
    mRenderUI = true;

    TobiiResearchStatus status;

    status = tobii_research_find_all_eyetrackers(&mEyetrackers);
    if (status != TOBII_RESEARCH_STATUS_OK) {
        printf("Finding trackers failed. Error: %d\n", status);
    }

    printf("Found %d Eye Trackers \n\n", (int)mEyetrackers->count);

    mEyetrackingEnabled = mEyetrackers->count > 0;

    if (mEyetrackingEnabled) {
        tobii_research_subscribe_to_notifications(mEyetrackers->eyetrackers[0], notification_callback, NULL);

        TobiiResearchDisplayArea display_area;
        status = tobii_research_get_display_area(mEyetrackers->eyetrackers[0], &display_area);

        char* serial_number = NULL;
        tobii_research_get_serial_number(mEyetrackers->eyetrackers[0], &serial_number);

        printf("Got display area from tracker with serial number %s with status %i:\n", serial_number, status);
        tobii_research_free_string(serial_number);

        printf("Bottom Left: (%f, %f, %f)\n",
            display_area.bottom_left.x,
            display_area.bottom_left.y,
            display_area.bottom_left.z);
        printf("Bottom Right: (%f, %f, %f)\n",
            display_area.bottom_right.x,
            display_area.bottom_right.y,
            display_area.bottom_right.z);
        printf("Height: %f\n", display_area.height);
        printf("Top Left: (%f, %f, %f)\n",
            display_area.top_left.x,
            display_area.top_left.y,
            display_area.top_left.z);
        printf("Top Right: (%f, %f, %f)\n",
            display_area.top_right.x,
            display_area.top_right.y,
            display_area.top_right.z);
        printf("Width: %f\n", display_area.width);

        int64_t system_time_stamp;
        status = tobii_research_get_system_time_stamp(&system_time_stamp);

        printf("The system time stamp in microseconds is %" PRId64 " with status %i.\n", system_time_stamp, status);

        mGazeData = new TobiiResearchGazeData();

        status = tobii_research_subscribe_to_gaze_data(mEyetrackers->eyetrackers[0], gaze_data_callback, mGazeData);
        if (status != TOBII_RESEARCH_STATUS_OK) {
            printf("Subscribing to Gaze Data failed. Error: %d\n", status);
        }

        float initial_gaze_output_frequency;
        status = tobii_research_get_gaze_output_frequency(mEyetrackers->eyetrackers[0], &initial_gaze_output_frequency);
        printf("The eye tracker's initial gaze output frequency is %f Hz with status %i.\n",
            initial_gaze_output_frequency, status);

        TobiiResearchGazeOutputFrequencies* frequencies = NULL;
        status = tobii_research_get_all_gaze_output_frequencies(mEyetrackers->eyetrackers[0], &frequencies);

        if (status == TOBII_RESEARCH_STATUS_OK) {
            for (int i = 0; i < frequencies->frequency_count; i++) {
                status = tobii_research_set_gaze_output_frequency(mEyetrackers->eyetrackers[0], frequencies->frequencies[i]);
                printf("Gaze output frequency set to %f Hz with status %i.\n", frequencies->frequencies[i], status);
            }
            tobii_research_set_gaze_output_frequency(mEyetrackers->eyetrackers[0], initial_gaze_output_frequency);

            printf("Gaze output frequency reset to %f Hz.\n", initial_gaze_output_frequency);
        }
        else {
            printf("tobii_research_get_all_gaze_output_frequencies returned status %i.\n", status);
        }

        tobii_research_free_gaze_output_frequencies(frequencies);
    }
}

RenderPassReflection InteractionPass::reflect(const CompileData& compileData)
{
    RenderPassReflection r;
    r.addOutput(out, "The destination texture");
    r.addInput(in, "The source texture");
    return r;
}

void InteractionPass::setScene(RenderContext* pRenderContext, const Scene::SharedPtr& pScene)
{
    mpScene = pScene;

    if (mpScene)
    {
        Program::Desc desc;
        desc.addShaderLibrary(kShaderFile).csEntry("main").setShaderModel(kShaderModel);
        auto typeConformances = mpScene->getTypeConformances();
        desc.addTypeConformances(typeConformances);
        mpInteractionPass = ComputePass::create(desc, Program::DefineList(), false);
        mpFence = GpuFence::create();

        // Prepare our programs for the scene.
        Shader::DefineList defines = mpScene->getSceneDefines();

        // Disable discard and gradient operations.
        defines.add("_MS_DISABLE_ALPHA_TEST");
        defines.add("_DEFAULT_ALPHA_TEST");

        mpInteractionPass->getProgram()->addDefines(defines);

        mpInteractionPass->setVars(nullptr); // Trigger recompile

        // Bind variables.
        auto var = mpInteractionPass->getRootVar()["CB"]["gInteractionPass"];
        if (!mpPixelDataBuffer)
        {
            mpPixelDataBuffer = Buffer::createStructured(var["pixelData"], 1, ResourceBindFlags::ShaderResource | ResourceBindFlags::UnorderedAccess, Buffer::CpuAccess::None, nullptr, false);
            mpPixelDataStaging = Buffer::createStructured(var["pixelData"], 1, ResourceBindFlags::None, Buffer::CpuAccess::Read, nullptr, false);
        }
        var["pixelData"] = mpPixelDataBuffer;

        /*matValList.clear();
        uint32_t materialID = 0;
        for (auto& material : mpScene->getMaterials())
        {
            matValList.push_back({ materialID, material->getName() });
            materialID++;
        }*/

        for (uint32_t id : mpScene->getGeometryIDs(GeometryType::TriangleMesh))
        {
            glm::vec3 transform = mpScene->getAnimationController()->getGlobalMatrices()[mpScene->getGeometryInstance(id).globalMatrixID][3].xyz;
            transValList[id] = transform;
        }

        mpScene->addViewpoint(float3(0.892393, 2.321199, -3.346568), float3(1.349107, 1.857762, -2.587201), float3(-0.000423, 1.000000, -0.000703), 0);
        mpScene->addViewpoint(float3(3.501745, 2.458483, -2.836822), float3(2.978681, 1.907147, -2.186873), float3(-0.000485, 1.000000, 0.000602), 0);
        mpScene->addViewpoint(float3(3.102161, 3.026642, 0.337517), float3(2.275490, 2.513116, 0.107504), float3(0.000000, 1.000000, 0.000000), 0);
        mpScene->addViewpoint(float3(-0.592427, 1.122877, -0.771308), float3(-1.527504, 1.311737, -1.071247), float3(0.002599, 0.999996, 0.000832), 0);
        mpScene->addViewpoint(float3(-0.460917, 1.76411, -1.47318), float3(-1.25415, 1.34553, -1.91543), float3(0, 1, 0), 0);
        mpScene->selectViewpoint(0);

        //find_all_eyetrackers_example();
    }
}

void InteractionPass::compile(RenderContext* pContext, const CompileData& compileData)
{
    //pFrameDim = compileData.defaultTexDims;
    mParams.frameDim = compileData.defaultTexDims;
}

void InteractionPass::execute(RenderContext* pRenderContext, const RenderData& renderData)
{
    if (mEnabled && mpScene)
    {

        InternalDictionary& dict = renderData.getDictionary();

        mpScene->setRaytracingShaderData(pRenderContext, mpInteractionPass->getRootVar());

        ShaderVar var = mpInteractionPass->getRootVar()["CB"]["gInteractionPass"];
        var["params"].setBlob(mParams);

        //mpInteractionPass->execute(pRenderContext, uint3(pFrameDim, 1));
        mpInteractionPass->execute(pRenderContext, uint3(mParams.frameDim, 1));

        pRenderContext->copyResource(mpPixelDataStaging.get(), mpPixelDataBuffer.get());
        pRenderContext->flush(false);
        mpFence->gpuSignal(pRenderContext->getLowLevelData()->getCommandQueue());
        dict["right_mouse_clicked"] = false;

        if (mRightMouseClicked)
        {
            assert(mpPixelDataStaging);
            mpFence->syncCpu();
            InteractableObject currentObj;
            currentObj.mpPixelData = *reinterpret_cast<const PixelData*>(mpPixelDataStaging->map(Buffer::MapType::Read));

            if (currentObj.mpPixelData.meshID != PixelData::kInvalidID)
            {
                dict[mesh_id] = (uint)currentObj.mpPixelData.meshInstanceID;

                glm::mat4 transform = mpScene->getAnimationController()->getGlobalMatrices()[mpScene->getGeometryInstance(currentObj.mpPixelData.meshInstanceID).globalMatrixID];

                currentObj.mTranslation = transform[3].xyz;
                currentObj.mScaling = glm::vec3(glm::length(transform[0]), glm::length(transform[1]), glm::length(transform[2]));
                auto roti = glm::mat4(transform[0] / currentObj.mScaling[0], transform[1] / currentObj.mScaling[1], transform[2] / currentObj.mScaling[2], transform[3]);
                currentObj.mRotation = glm::eulerAngles(glm::quat_cast(roti));

                //update prev objects position
                for (int i = 0; i < selectedObj.size(); i++)
                {
                    mTranslation.uponSelection[i] = selectedObj[i].mTranslation;
                    mRotation.uponSelection[i] = selectedObj[i].mRotation;
                    mScaling.uponSelection[i] = selectedObj[i].mScaling;
                }

                selectedObj.push_back(currentObj);
                setSelectedPixelToObjectCenter();

                mTranslation.add(currentObj.mTranslation, (float)selectedObj.size());
                mRotation.add(currentObj.mRotation, (float)selectedObj.size());
                mScaling.add(currentObj.mScaling, (float)selectedObj.size());

                backgroundPixelSelected = false;
                dict["right_mouse_clicked"] = true;
                dict[point_of_change] = mParams.selectedPixel;
            }
            else
            {
                selectedObj.clear();
                mTranslation.clear();
                mRotation.clear();
                mScaling.clear();
                backgroundPixelSelected = true;
            }

            mRightMouseClicked = false;
            mPixelDataAvailable = true;
        }
        dict["num_selected_obj"] = (int)selectedObj.size();

        //Set parameters for path-tracer
        if (mUserChangedScene)
        {
            // TODO: add eye tracking
            if (selectedObj.size() > 0)
            {
                setSelectedPixelToObjectCenter();
            }
            else // environment map changes
            {
                mParams.selectedPixel = 0.5f * (float2)mParams.frameDim;
                mUserChangedScene = false;
            }
            dict[point_of_change] = mParams.selectedPixel;
            dict[change_occured] = true;
        }
        else
        {
            dict[change_occured] = false;
        }

        dict[background_pixel] = backgroundPixelSelected;

        mParams.frameCount++;

        //Animations
        if (mUserChangedScene)
        {
            for (auto obj : selectedObj)
            {
                uint matID = mpScene->getGeometryInstance(obj.mpPixelData.meshInstanceID).globalMatrixID;

                Falcor::Animation::SharedPtr ptr = Falcor::Animation::create("interaction_hack_" + matID, matID, 0.00);

                Falcor::Animation::Keyframe kf_0;
                kf_0.translation = obj.mTranslation;
                kf_0.rotation = glm::quat(obj.mRotation);
                kf_0.scaling = obj.mScaling;
                kf_0.time = 0.0f;
                ptr->addKeyframe(kf_0);

                ptr->setPostInfinityBehavior(Falcor::Animation::Behavior::Constant);

                /*for (auto& anim : mpScene->getAnimations())
                {
                    if (anim->getName() == "interaction_hack_" + matID)
                    {
                        anim = ptr;
                        ptr.reset();
                        break;
                    }
                }*/

                if (ptr)
                    mpScene->getAnimations().push_back(ptr);
            }

            mUserChangedScene = false;
        }
        else
        {
            mpScene->getAnimations().clear();
        }
    }

    // Copy rendered input to output
    const auto& pSrcTex = renderData[in]->asTexture();
    const auto& pDstTex = renderData[out]->asTexture();

    if (pSrcTex && pDstTex)
    {
        pRenderContext->blit(pSrcTex->getSRV(), pDstTex->getRTV());
    }
    else
    {
        logWarning("InteractionPass::execute() - missing an input or output resource");
    }
}

void InteractionPass::setSelectedPixelToObjectCenter() {

    if (selectedObj.size() == 0)
    {
        return;
    }

    double posX = 0;
    double posY = 0;
    for (auto obj : selectedObj)
    {
        AABB bounds = mpScene->getMeshBounds(obj.mpPixelData.meshID);
        glm::mat4 transform = mpScene->getAnimationController()->getGlobalMatrices()[mpScene->getGeometryInstance(obj.mpPixelData.meshInstanceID).globalMatrixID];
        Falcor::float3 center = bounds.center();
        mpScene->selectCamera(0);
        auto camera = mpScene->getCamera();
        auto& viewProjMatrix = camera->getViewProjMatrix();
        //  mParams.selectedPixel = viewProjMatrix * Falcor::float4(mTranslation, 1);
        Falcor::float2 screenDims = mParams.frameDim;
        Falcor::float4 pixel = viewProjMatrix * transform * Falcor::float4(center, 1);
        pixel.x = pixel.x / pixel.w;
        pixel.y = pixel.y / pixel.w;
        /* pixel.x = (pixel.x + 1) * 0.5 * mParams.frameDim.x;
         pixel.y = (1 - pixel.y) * 0.5 * mParams.frameDim.y;*/
        double pixel_x = (pixel.x + 1) * 0.5 * screenDims.x;
        double pixel_y = (1 - pixel.y) * 0.5 * screenDims.y;

        posX += pixel_x;
        posY += pixel_y;
    }
    posX /= selectedObj.size();
    posY /= selectedObj.size();
    mParams.selectedPixel = uint2(posX, posY);
}

bool InteractionPass::onMouseEvent(const MouseEvent& mouseEvent)
{
    if (mouseEvent.type == MouseEvent::Type::ButtonDown && mouseEvent.button == Input::MouseButton::Right)
    {
        const InputState& inputState = gpFramework->getInputState();
        if (!inputState.isKeyDown(Input::Key::LeftControl))
        {
            selectedObj.clear();
            mTranslation.clear();
            mRotation.clear();
            mScaling.clear();
        }
        float2 cursorPos = mouseEvent.pos * (float2)mParams.frameDim;
        mParams.selectedPixel = (uint2)glm::clamp(cursorPos, float2(0.f), float2(mParams.frameDim.x - 1, mParams.frameDim.y - 1));
        mRightMouseClicked = true;
    }

    return false;
}

void InteractionPass::renderUI(Gui::Widgets& widget)
{
#if DEBUG_UI
    widget.checkbox("Enabled?", mEnabled);
#endif

    if (mEnabled)
    {
#if DEBUG_UI
        widget.var("Selected pixel", mParams.selectedPixel);
#endif

        if (mPixelDataAvailable)
        {
            std::ostringstream oss;
            if (selectedObj.size() == 1)
            {
#if DEBUG_UI
                if (selectedObj[0].mpPixelData.meshID != PixelData::kInvalidID)
                {
                    uint matID = mpScene->getGeometryInstance(selectedObj[0].mpPixelData.meshInstanceID).globalMatrixID;
                    oss << "Selected Mesh:" << std::endl
                        << "Mesh ID: " << selectedObj[0].mpPixelData.meshID << std::endl
                        << "Mesh name: " << (mpScene->hasMesh(selectedObj[0].mpPixelData.meshID) ? mpScene->getMeshName(selectedObj[0].mpPixelData.meshID) : "unknown") << std::endl
                        << "Mesh instance ID: " << selectedObj[0].mpPixelData.meshInstanceID << std::endl
                        << "Matrix ID: " << matID << std::endl
                        << "Material ID: " << selectedObj[0].mpPixelData.materialID << std::endl
                        << "Num Mats: " << mpScene->getAnimationController()->getGlobalMatrices().size() << std::endl;
                }
                else if (selectedObj[0].mpPixelData.curveInstanceID != PixelData::kInvalidID)
                {
                    oss << "Curve ID: " << selectedObj[0].mpPixelData.curveID << std::endl
                        << "Curve instance ID: " << selectedObj[0].mpPixelData.curveInstanceID << std::endl
                        << "Material ID: " << selectedObj[0].mpPixelData.materialID << std::endl;
        }
#endif
            }
            else if (selectedObj.size() > 1)
            {
#if DEBUG_UI
                oss << "Multiple Objects selected:" << std::endl;
                for (auto obj : selectedObj)
                {
                    oss << obj.mpPixelData.meshInstanceID << ", ";
                }
                oss << std::endl;
#endif
            }
            else
            {
                oss << "Background pixel" << std::endl;
                mUserChangedScene = mUserChangedScene || mpScene->getEnvMap()->renderUI(widget);
            }
            widget.text(oss.str());

            if (selectedObj.size() == 1 && selectedObj[0].mpPixelData.meshID != PixelData::kInvalidID)
            {
                mUserChangedScene = widget.var("Translation", selectedObj[0].mTranslation) || mUserChangedScene;
                mUserChangedScene = widget.var("Scaling", selectedObj[0].mScaling) || mUserChangedScene;
                mUserChangedScene = widget.var("Rotation", selectedObj[0].mRotation) || mUserChangedScene;

                reset = widget.button("Reset Transform");
                if (reset)
                {
                    selectedObj[0].mTranslation = transValList[selectedObj[0].mpPixelData.meshInstanceID];
                    selectedObj[0].mScaling = float3(1);
                    selectedObj[0].mRotation = float3(0);
                    mUserChangedScene = true;
                }
#if ANIMATION_UI
                if (widget.button("Animate"))
                {
                    mAnimate = true;
                    logInfo("animate clicked");
                }
#endif
                std::ostringstream materialStream;
                materialStream << "Material Name: " << mpScene->getMaterial(selectedObj[0].mpPixelData.materialID)->getName() << std::endl;
                widget.text(materialStream.str());
                auto materialGroup = widget.group("Edit Material", true);
                mUserChangedScene = mpScene->getMaterial(selectedObj[0].mpPixelData.materialID)->renderUI(materialGroup) || mUserChangedScene;

                //TODO: Dropdown to set to other material
                /*uint32_t newMatID = mpPixelData.materialID;
                if (widget.dropdown("Set to material", matValList, newMatID, false))
                {
                    (Material::SharedPtr)mpScene->getMaterial(mpPixelData.materialID) = mpScene->getMaterial(newMatID);
                    mUserChangedScene = true;
                }*/
            }
            else if (selectedObj.size() > 1)
            {
                auto printUI = [&](TransformMultiple& trans, const char label[])
                {
                    float3 avgTranslat = trans.average / (float)selectedObj.size();
                    float3 diff = float3(0);
                    if (widget.var(label, trans.current))
                    {
                        diff = trans.current - avgTranslat;
                        mUserChangedScene = true;
                    }
                    return diff;
                };

                float3 translatDiff = printUI(mTranslation, "Translation");
                float3 rotDiff = printUI(mRotation, "Rotation");
                float3 scaleDiff = printUI(mScaling, "Scaling");

                int objID = 0;
                for (auto& obj : selectedObj)
                {
                    obj.mTranslation = mTranslation.uponSelection[objID] + translatDiff;
                    obj.mRotation = mRotation.uponSelection[objID] + rotDiff;
                    obj.mScaling = mScaling.uponSelection[objID] + scaleDiff;
                    objID++;
                }

                if (widget.button("Reset All Transforms"))
                {
                    int objID = 0;
                    mTranslation.clear();
                    mRotation.clear();
                    mScaling.clear();
                    for (auto& obj : selectedObj)
                    {
                        obj.mTranslation = transValList[obj.mpPixelData.meshInstanceID];
                        obj.mRotation = float3(0);
                        obj.mScaling = float3(1);
                        mTranslation.add(obj.mTranslation, (float)selectedObj.size());
                        mRotation.add(obj.mRotation, (float)selectedObj.size());
                        mScaling.add(obj.mScaling, (float)selectedObj.size());
                        objID++;
                    }
                    mUserChangedScene = true;
                }
            }

            mpPixelDataStaging->unmap();
        }
#if DEBUG_UI
        widget.dummy("#spacer1", { 1, 20 });
        widget.text("Scene: " + (mpScene ? mpScene->getPath().filename().string() : "No scene loaded"));
#endif
    }
}
