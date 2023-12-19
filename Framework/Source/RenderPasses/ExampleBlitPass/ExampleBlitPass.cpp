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
#include "ExampleBlitPass.h"

const RenderPass::Info ExampleBlitPass::kInfo{ "ExampleBlitPass", "Blits a texture into another texture" };

// Don't remove this. it's required for hot-reload to function properly
extern "C" FALCOR_API_EXPORT const char* getProjDir()
{
    return PROJECT_DIR;
}

extern "C" FALCOR_API_EXPORT void getPasses(Falcor::RenderPassLibrary& lib)
{
    lib.registerPass(ExampleBlitPass::kInfo, ExampleBlitPass::create);
}

ExampleBlitPass::SharedPtr ExampleBlitPass::create(RenderContext* pRenderContext, const Dictionary& dict)
{
    SharedPtr pPass = SharedPtr(new ExampleBlitPass);
    return pPass;
}

Dictionary ExampleBlitPass::getScriptingDictionary()
{
    return Dictionary();
}

RenderPassReflection ExampleBlitPass::reflect(const CompileData& compileData)
{
    RenderPassReflection reflector;
    reflector.addInput("input", "the source texture");
    reflector.addOutput("output", "the destination texture");
    return reflector;
}

void ExampleBlitPass::execute(RenderContext* pRenderContext, const RenderData& renderData)
{
    const auto& pSrcTex = renderData["input"]->asTexture();
    const auto& pDstTex = renderData["output"]->asTexture();

    if (pSrcTex && pDstTex)
    {
        pRenderContext->blit(pSrcTex->getSRV(), pDstTex->getRTV());
    }
    else
    {
        logWarning("ExampleBlitPass::execute() - missing an input or output resource");
    }
}

void ExampleBlitPass::renderUI(Gui::Widgets& widget)
{
}
