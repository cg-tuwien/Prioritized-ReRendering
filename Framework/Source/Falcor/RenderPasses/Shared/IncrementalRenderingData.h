#pragma once
#include "Falcor.h"

enum class ClearMode
{
    ContinuousRefinement,
    Incremental,
    Reset,
};

class FALCOR_API IncrementalData
{
public:
    static Falcor::Texture::SharedPtr lastFrameDenoisedHDR;
};
