cbuffer PerFrameCB
{
    uint2   gResolution;
    float4  gWireframeColor;
}

Texture2D<float4> gCurFrame;
Texture2D<float4> gWireframe;
RWTexture2D<float4> gOutputFrame;

[numthreads(16, 16, 1)]
void overlay(uint3 dispatchThreadId : SV_DispatchThreadID)
{
    if (any(dispatchThreadId.xy >= gResolution)) return;
    const uint2 pixelPos = dispatchThreadId.xy;

    const float4 curColor = gCurFrame[pixelPos];
    const float4 wfColor = gWireframe[pixelPos];

    float4 output;
    if (all(wfColor == gWireframeColor))
    {
        output = wfColor;
    }
    else
    {
        output = curColor;
    }

    gOutputFrame[pixelPos] = output;
}
