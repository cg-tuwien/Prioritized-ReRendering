#pragma once
#include "Utils/HostDeviceShared.slangh"

BEGIN_NAMESPACE_FALCOR

struct InteractionPassParams
{
    uint mode = 0;                  ///< Current visualization mode. I do not understand why, but it doesnt work without this.
    uint2 frameDim = { 0, 0 };
    uint frameCount = 0;

    uint2 selectedPixel = { 0, 0 }; ///< The currently selected pixel for readback.
};

struct PixelData
{
    static const uint kInvalidID = 0xffffffff;

    uint meshInstanceID;
    uint meshID;
    uint materialID;

    uint curveInstanceID;
    uint curveID;
};

END_NAMESPACE_FALCOR
