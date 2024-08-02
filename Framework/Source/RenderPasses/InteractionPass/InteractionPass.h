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

#include "tobii_research.h"
#include "tobii_research_eyetracker.h"
#include "tobii_research_streams.h"

using namespace Falcor;

void gaze_data_callback(TobiiResearchGazeData* gaze_data, void* user_data) {
    printf("gaze\n");
    memcpy(user_data, gaze_data, sizeof(*gaze_data));
};

void notification_callback(TobiiResearchNotification* notification, void* user_data) {

    if (TOBII_RESEARCH_NOTIFICATION_CALIBRATION_MODE_ENTERED == notification->notification_type) {
        printf("Enter calibration mode notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_CALIBRATION_MODE_LEFT == notification->notification_type) {
        printf("Left calibration mode notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_CALIBRATION_CHANGED == notification->notification_type) {
        printf("Calibration changed notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_CONNECTION_LOST == notification->notification_type) {
        printf("Connection lost notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_CONNECTION_RESTORED == notification->notification_type) {
        printf("Connection restored notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_DEVICE_FAULTS == notification->notification_type) {
        printf("Device faults notification received at time stamp %" PRId64 ".\n %s \n", notification->system_time_stamp, notification->value.text);
    }

    if (TOBII_RESEARCH_NOTIFICATION_DEVICE_WARNINGS == notification->notification_type) {
        printf("Device warnings notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_DISPLAY_AREA_CHANGED == notification->notification_type) {
        printf("Display area changed notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_EYE_TRACKING_MODE_CHANGED == notification->notification_type) {
        printf("Eye tracking mode changed notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_GAZE_OUTPUT_FREQUENCY_CHANGED == notification->notification_type) {
        printf("Gaze output frequency changed notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_TRACK_BOX_CHANGED == notification->notification_type) {
        printf("Track box changed notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    if (TOBII_RESEARCH_NOTIFICATION_UNKNOWN == notification->notification_type) {
        printf("Unknown notification received at time stamp %" PRId64 ".\n", notification->system_time_stamp);
    }

    (void)(user_data); // Is NULL, see subscription call
};

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
        //selectedObj[0].mTranslation = selectedObj[0].mTranslation + float3(0, 0, 0.005);
        std::dynamic_pointer_cast<StandardMaterial>(mpScene->getMaterial(selectedObj[0].mpPixelData.materialID))->setMetallic(1);
        mUserChangedScene = true;
    }
    bool shouldAnimate() {
        return mAnimate;
    }
    void stopAnimation() { mAnimate = false; }

    void shutdown() {
        if (mEyetrackingEnabled)
        {
            TobiiResearchStatus status;
            status = tobii_research_unsubscribe_from_gaze_data(mEyetrackers->eyetrackers[0], gaze_data_callback);
            printf("Unsubscribed from gaze data with status %i.\n", status);

            printf("Last received gaze package:\n");
            printf("System time stamp: %"  PRId64 "\n", mGazeData->system_time_stamp);
            printf("Device time stamp: %"  PRId64 "\n", mGazeData->device_time_stamp);
            printf("Left eye 2D gaze point on display area: (%f, %f)\n",
                mGazeData->left_eye.gaze_point.position_on_display_area.x,
                mGazeData->left_eye.gaze_point.position_on_display_area.y);
            printf("Right eye 3d gaze origin in user coordinates (%f, %f, %f)\n",
                mGazeData->right_eye.gaze_origin.position_in_user_coordinates.x,
                mGazeData->right_eye.gaze_origin.position_in_user_coordinates.y,
                mGazeData->right_eye.gaze_origin.position_in_user_coordinates.z);

            tobii_research_unsubscribe_from_notifications(mEyetrackers->eyetrackers[0], notification_callback);

            tobii_research_free_eyetrackers(mEyetrackers);
        }
    };

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

    TobiiResearchEyeTrackers* mEyetrackers;
    TobiiResearchGazeData* mGazeData;
    bool mEyetrackingEnabled = false;
};
