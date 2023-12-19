/***************************************************************************
 # Copyright (c) 2020, NVIDIA CORPORATION. All rights reserved.
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


cbuffer PerFrameCB
{
    uint2   gBlockGrid;
    uint    gBlockDist;
    uint    gRenderSamples;
}

// Input data to accumulate and accumulated output.
RWTexture2D<float4> gInOutFrame;
RWTexture2D<float4> gFunTex;
Texture2D<int2> gPixelMap;

[numthreads(TILE_SIZE, TILE_SIZE, 1)]
void reduce(uint3 dispatchThreadId : SV_DispatchThreadID)
{
    int computeBlock = dispatchThreadId.x / TILE_SIZE;
    int group = computeBlock / gBlockDist;
    int ingroup = computeBlock % gBlockDist;

    int srcBlock = group * gRenderSamples + ingroup;
    int dstBlock = srcBlock + gBlockDist;

    uint2 src = uint2(srcBlock % gBlockGrid.x, srcBlock / gBlockGrid.x);
    uint2 dst = uint2(dstBlock % gBlockGrid.x, dstBlock / gBlockGrid.x);

    uint2 srcPixel = src * TILE_SIZE + (dispatchThreadId.xy % TILE_SIZE);
    uint2 dstPixel = dst * TILE_SIZE + (dispatchThreadId.xy % TILE_SIZE);

    if(gBlockDist == gRenderSamples/2)
    {
        gFunTex[srcPixel] = (gInOutFrame[srcPixel] + gInOutFrame[dstPixel]);
        gInOutFrame[dstPixel] = 0.0f;
        gInOutFrame[srcPixel] = 0.0f;
    }
    else if(gBlockDist == 1)
    {
        uint2 blockCoord = uint2(group % gBlockGrid.x, group / gBlockGrid.x);
        int2 targetBlock = gPixelMap.Load(int3(blockCoord, 0));
        uint2 targetPixel = targetBlock * TILE_SIZE + (dispatchThreadId.xy % TILE_SIZE);
        gInOutFrame[targetPixel] = (gFunTex[srcPixel] + gFunTex[dstPixel]);

        gFunTex[dstPixel] = 0.0f;
        gFunTex[srcPixel] = 0.0f;
    }
    else
    {
        gFunTex[srcPixel] = (gFunTex[srcPixel] + gFunTex[dstPixel]);
        gFunTex[dstPixel] = 0.0f;
    }
}
