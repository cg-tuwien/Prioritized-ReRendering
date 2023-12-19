import Scene.Raster; // Imports defaultVS.

cbuffer PerFrameCB
{
    float4 gColor;
};

VSOut vsMain(VSIn vIn)
{
    return defaultVS(vIn);
}

float4 psMain() : SV_TARGET
{
    return gColor;
}
