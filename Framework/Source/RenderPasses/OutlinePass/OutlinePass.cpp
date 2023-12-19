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
#include "OutlinePass.h"
#include "glm/gtx/string_cast.hpp"

const RenderPass::Info OutlinePass::kInfo{ "OutlinePass", "Insert pass description here." };

// Don't remove this. it's required for hot-reload to function properly
extern "C" FALCOR_API_EXPORT const char* getProjDir()
{
    return PROJECT_DIR;
}

extern "C" FALCOR_API_EXPORT void getPasses(Falcor::RenderPassLibrary & lib)
{
    lib.registerPass(OutlinePass::kInfo, OutlinePass::create);
}

namespace
{
    const char kWireframeShaderFile[] = "RenderPasses/WireframePass/Wireframe.3d.slang";
    const char kOverlayShaderFile[] = "RenderPasses/OutlinePass/Overlay.cs.slang";

    const char kInputChannel[] = "input";
    const char kOutputChannel[] = "output";
    const char kWireframeChannel[] = "wireframe";

    const char mesh_id[] = "mesh_id";
    const char clear_mode[] = "clear_mode";
    const char kShaderModel[] = "6_5";
    const char change_occured[] = "change_occured";
    const char background_pixel[] = "background_pixel";
}

OutlinePass::SharedPtr OutlinePass::create(RenderContext* pRenderContext, const Dictionary& dict)
{
    SharedPtr pPass = SharedPtr(new OutlinePass());
    return pPass;
}

Dictionary OutlinePass::getScriptingDictionary()
{
    return Dictionary();
}

RenderPassReflection OutlinePass::reflect(const CompileData& compileData)
{
    // Define the required resources here
    RenderPassReflection reflector;
    const uint2 sz = compileData.defaultTexDims;
    reflector.addOutput(kOutputChannel, "scene overlay with wireframe representation").bindFlags(ResourceBindFlags::RenderTarget | ResourceBindFlags::UnorderedAccess | ResourceBindFlags::ShaderResource).format(ResourceFormat::RGBA32Float).texture2D(sz.x, sz.y);
    reflector.addOutput(kWireframeChannel, "wireframe representation");
    reflector.addInput(kInputChannel, "input image");
    return reflector;
}

void OutlinePass::setScene(RenderContext* pRenderContext, const Scene::SharedPtr& pScene)
{
    mpScene = pScene;

    if (mpScene)
    {
        Program::Desc desc;
        desc.addShaderLibrary(kOverlayShaderFile).csEntry("overlay").setShaderModel(kShaderModel);
        auto typeConformances = mpScene->getTypeConformances();
        desc.addTypeConformances(typeConformances);
        mOverlayShader.mpProgram = ComputeProgram::create(desc, mpScene->getSceneDefines());
        mOverlayShader.mpVars = ComputeVars::create(mOverlayShader.mpProgram->getReflector());
        mOverlayShader.mpState = ComputeState::create();
    }
}

bool OutlinePass::onMouseEvent(const MouseEvent& mouseEvent)
{
    if (mouseEvent.type == MouseEvent::Type::ButtonDown && mouseEvent.button == Input::MouseButton::Left)
    {
        mShowSelection = false;
    }
    if (mouseEvent.type == MouseEvent::Type::ButtonUp && mouseEvent.button == Input::MouseButton::Left)
    {
        cameraReset = true;
    }
    if (mouseEvent.type == MouseEvent::Type::ButtonDown && mouseEvent.button == Input::MouseButton::Right)
    {
        //prevMeshID = -1;
        const InputState& inputState = gpFramework->getInputState();
        if (!inputState.isKeyDown(Input::Key::LeftControl))
        {
            meshIDs.clear();
        }
        mShowSelection = true;
    }
    return false;
}
bool OutlinePass::onKeyEvent(const KeyboardEvent& keyEvent)
{
    if (keyEvent.key == Input::Key::LeftControl) return false;

    mShowSelection = false;
    cameraReset = true;
    return false;
}

void OutlinePass::execute(RenderContext* pRenderContext, const RenderData& renderData)
{
    InternalDictionary& dict = renderData.getDictionary();

    if (!mpScene || !mEnabled || !dict.keyExists(mesh_id))
    {
        pRenderContext->blit(renderData[kInputChannel]->asTexture()->getSRV(), renderData[kOutputChannel]->asTexture()->getRTV());
        return;
    }

    uint32_t currentViewpoint = mpScene->getCurrentViewpoint();
    if (currentViewpoint != previousViewpoint)
    {
        previousViewpoint = currentViewpoint;
        cameraReset = true;
        //mShowSelection = false;
    }

    uint32_t meshID = (uint)dict.getValue(mesh_id, (uint32_t)0);
    bool scene_reset = false;

    bool meshAlreadySelected = std::find(meshIDs.begin(), meshIDs.end(), meshID) != meshIDs.end();

    if (!meshAlreadySelected && !dict.getValue(background_pixel, false))
    {
        if ((int)dict.getValue("num_selected_obj", (int)0) <= 1)
        {
            meshIDs.clear();
        }
        meshIDs.push_back(meshID);
        try
        {
            SceneBuilder::SharedPtr singleBuilder = SceneBuilder::create(SceneBuilder::Flags::DontOptimizeGraph);
#if FALCOR_ENABLE_INTERACTIVE_IMPORT
            singleBuilder->importAsStatic = false;
#endif

            for (auto mesh : meshIDs)
            {
                std::filesystem::path path = mpScene->getPath();
                path.remove_filename();
                path += mpScene->getMeshName(mesh);

                /* //not needed anymore, as animation is applied, but would import/instanciate at given positions
                SceneBuilder::InstanceMatrices mat;
                mat.push_back(mpScene->getAnimationController()->getGlobalMatrices()[mpScene->getGeometryInstance(meshID).globalMatrixID]);
                singleBuilder->import(path, mat);*/
                singleBuilder->import(path);
            }

            Camera::SharedPtr camera = Camera::create("main");
            camera->setApertureRadius(mpScene->getCamera()->getApertureRadius());
            camera->setAspectRatio(mpScene->getCamera()->getAspectRatio());
            camera->setFarPlane(mpScene->getCamera()->getFarPlane());
            camera->setFocalDistance(mpScene->getCamera()->getFocalDistance());
            camera->setFocalLength(mpScene->getCamera()->getFocalLength());
            camera->setFrameHeight(mpScene->getCamera()->getFrameHeight());
            camera->setFrameWidth(mpScene->getCamera()->getFrameWidth());
            camera->setISOSpeed(mpScene->getCamera()->getISOSpeed());
            camera->setNearPlane(mpScene->getCamera()->getNearPlane());
            camera->setPosition(mpScene->getCamera()->getPosition());
            camera->setProjectionMatrix(mpScene->getCamera()->getProjMatrix());
            camera->setShutterSpeed(mpScene->getCamera()->getShutterSpeed());
            camera->setTarget(mpScene->getCamera()->getTarget());
            camera->setUpVector(mpScene->getCamera()->getUpVector());
            camera->setViewMatrix(mpScene->getCamera()->getViewMatrix());
            singleBuilder->addCamera(camera);
            singleBuilder->setSelectedCamera(camera);

            singleScene = singleBuilder->getScene();

            singleScene->setCameraController(Scene::CameraControllerType::FirstPerson);
            singleScene->setCameraControlsEnabled(true);

            scene_reset = true;
            mShowSelection = true;

            mWireframeShader.mpProgram = GraphicsProgram::createFromFile(kWireframeShaderFile, "vsMain", "psMain");
            mWireframeShader.mpProgram->addDefines(singleScene->getSceneDefines());
            mWireframeShader.mpProgram->setTypeConformances(singleScene->getTypeConformances());
            mWireframeShader.mpVars = GraphicsVars::create(mWireframeShader.mpProgram->getReflector());

            RasterizerState::Desc wireframeDesc;
            wireframeDesc.setFillMode(RasterizerState::FillMode::Wireframe);
            wireframeDesc.setCullMode(RasterizerState::CullMode::None);
            mWireframeShader.mpRasterState = RasterizerState::create(wireframeDesc);

            DepthStencilState::Desc dsDesc;
            dsDesc.setDepthEnabled(false);
            mWireframeShader.mpNoDepthDS = DepthStencilState::create(dsDesc);
            dsDesc.setDepthFunc(ComparisonFunc::Less).setDepthEnabled(true);
            mWireframeShader.mpDepthTestDS = DepthStencilState::create(dsDesc);

            mWireframeShader.mpState = GraphicsState::create();
            mWireframeShader.mpState->setProgram(mWireframeShader.mpProgram);
            mWireframeShader.mpState->setRasterizerState(mWireframeShader.mpRasterState);
            mWireframeShader.mpState->setDepthStencilState(mWireframeShader.mpNoDepthDS);
        }
        catch (Exception e)
        {
            logWarning(e.what());
            singleScene = nullptr;
        }
    }

    if (!singleScene)
    {
        pRenderContext->blit(renderData[kInputChannel]->asTexture()->getSRV(), renderData[kOutputChannel]->asTexture()->getRTV());
        return;
    }

    if (scene_reset || updateNextFrame)
    {
        int nodeID = 0;
        for (auto mesh : meshIDs)
        {
            glm::mat4 transform = mpScene->getAnimationController()->getGlobalMatrices()[mpScene->getGeometryInstance(mesh).globalMatrixID];
            glm::vec3 translation = transform[3].xyz;
            glm::vec3 scaling = glm::vec3(glm::length(transform[0]), glm::length(transform[1]), glm::length(transform[2]));
            auto roti = glm::mat4(transform[0] / scaling[0], transform[1] / scaling[1], transform[2] / scaling[2], transform[3]);
            glm::vec3 rotation = glm::eulerAngles(glm::quat_cast(roti));

            Falcor::Animation::SharedPtr ptr = Falcor::Animation::create("singlescene_hack"+mesh, nodeID, 0.0);
            Falcor::Animation::Keyframe kf;
            kf.rotation = glm::quat(rotation);
            kf.scaling = scaling;
            kf.translation = translation;
            kf.time = 0.0f;
            ptr->addKeyframe(kf);
            ptr->setPostInfinityBehavior(Falcor::Animation::Behavior::Constant);

            if (ptr)
                singleScene->getAnimations().push_back(ptr);

            nodeID++;
        }

        singleScene->update(pRenderContext, 0);
        updateNextFrame = false;
    }
    else
    {
        singleScene->getAnimations().clear();
    }

    if (cameraReset)
    {
        Camera::SharedPtr camera = singleScene->getCamera();
        camera->setApertureRadius(mpScene->getCamera()->getApertureRadius());
        camera->setAspectRatio(mpScene->getCamera()->getAspectRatio());
        camera->setFarPlane(mpScene->getCamera()->getFarPlane());
        camera->setFocalDistance(mpScene->getCamera()->getFocalDistance());
        camera->setFocalLength(mpScene->getCamera()->getFocalLength());
        camera->setFrameHeight(mpScene->getCamera()->getFrameHeight());
        camera->setFrameWidth(mpScene->getCamera()->getFrameWidth());
        camera->setISOSpeed(mpScene->getCamera()->getISOSpeed());
        camera->setNearPlane(mpScene->getCamera()->getNearPlane());
        camera->setPosition(mpScene->getCamera()->getPosition());
        camera->setProjectionMatrix(mpScene->getCamera()->getProjMatrix());
        camera->setShutterSpeed(mpScene->getCamera()->getShutterSpeed());
        camera->setTarget(mpScene->getCamera()->getTarget());
        camera->setUpVector(mpScene->getCamera()->getUpVector());
        camera->setViewMatrix(mpScene->getCamera()->getViewMatrix());
        singleScene->update(pRenderContext, 0);
        cameraReset = false;
    }

    if (dict.getValue(change_occured, false)) updateNextFrame = true;

    if (dict.getValue(background_pixel, false) || dict.getValue(change_occured, false)) mShowSelection = false;

    if (mShowSelection)
    {
        const float4 clearColor(0, 0, 0, 1);
        auto pTargetFbo = Fbo::create({ renderData[kWireframeChannel]->asTexture() });
        pRenderContext->clearFbo(pTargetFbo.get(), clearColor, 1.0f, 0, FboAttachmentType::All);
        mWireframeShader.mpState->setFbo(pTargetFbo);

        mWireframeShader.mpState->setDepthStencilState(mWireframeShader.mpNoDepthDS);
        mWireframeShader.mpVars["PerFrameCB"]["gColor"] = float4(0, 1, 0, 1);
        singleScene->rasterize(pRenderContext, mWireframeShader.mpState.get(), mWireframeShader.mpVars.get(), mWireframeShader.mpRasterState, mWireframeShader.mpRasterState);

        Texture::SharedPtr pSrc = renderData[kInputChannel]->asTexture();
        Texture::SharedPtr pDst = renderData[kOutputChannel]->asTexture();
        Texture::SharedPtr pWireframe = renderData[kWireframeChannel]->asTexture();
        FALCOR_ASSERT(pSrc && pDst && pWireframe);

        mOverlayShader.mpVars["gCurFrame"] = pSrc;
        mOverlayShader.mpVars["gOutputFrame"] = pDst;
        mOverlayShader.mpVars["gWireframe"] = pWireframe;

        const uint2 resolution = uint2(pSrc->getWidth(), pSrc->getHeight());

        mOverlayShader.mpVars["PerFrameCB"]["gResolution"] = resolution;
        mOverlayShader.mpVars["PerFrameCB"]["gWireframeColor"] = float4(0, 1, 0, 1);

        uint3 numGroups = div_round_up(uint3(resolution.x, resolution.y, 1u), mOverlayShader.mpProgram->getReflector()->getThreadGroupSize());
        mOverlayShader.mpState->setProgram(mOverlayShader.mpProgram);
        pRenderContext->dispatch(mOverlayShader.mpState.get(), mOverlayShader.mpVars.get(), numGroups);
    }
    else {
        pRenderContext->blit(renderData[kInputChannel]->asTexture()->getSRV(), renderData[kOutputChannel]->asTexture()->getRTV());
    }
}

void OutlinePass::renderUI(Gui::Widgets& widget)
{
    widget.checkbox("Enabled", mEnabled);
    widget.checkbox("Show Object", mShowSelection);
}
