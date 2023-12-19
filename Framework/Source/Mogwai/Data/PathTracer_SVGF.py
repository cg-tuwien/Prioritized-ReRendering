from falcor import *

def render_graph_MegakernelPathTracer():
    g = RenderGraph("MegakernelPathTracer")
    loadRenderPassLibrary("AccumulatePass.dll")
    loadRenderPassLibrary("GBuffer.dll")
    loadRenderPassLibrary("ToneMapper.dll")
    loadRenderPassLibrary("MegakernelPathTracer.dll")
    loadRenderPassLibrary("SVGFPass.dll")
    
    MegakernelPathTracer = createPass("MegakernelPathTracer", {"params": PathTracerParams(useVBuffer=0)})
    g.addPass(MegakernelPathTracer, "MegakernelPathTracer")
    GBufferRT = createPass("GBufferRT", {"forceCullMode": False, "cull": CullMode.CullBack, "samplePattern": SamplePattern.Center, "sampleCount": 16})
    g.addPass(GBufferRT, "GBufferRT")
    GBufferRaster = createPass("GBufferRaster", {"forceCullMode": False, "cull": CullMode.CullBack, "samplePattern": SamplePattern.Center, "sampleCount": 16})
    g.addPass(GBufferRaster, "GBufferRaster")
    AccumulatePass = createPass("AccumulatePass", {'enabled': True, 'precisionMode': AccumulatePrecision.Single})
    g.addPass(AccumulatePass, "AccumulatePass")
    SVGFPass = createPass("SVGFPass")
    g.addPass(SVGFPass, "SVGFPass")
    ToneMapper = createPass("ToneMapper", {'autoExposure': False, 'exposureCompensation': 0.0})
    g.addPass(ToneMapper, "ToneMapper")
    InteractionPass = createPass("InteractionPass")
    g.addPass(InteractionPass, "InteractionPass")
    OutlinePass = createPass("OutlinePass")
    g.addPass(OutlinePass, "OutlinePass")
    g.addEdge("GBufferRT.posW", "MegakernelPathTracer.posW")
    g.addEdge("GBufferRT.normW", "MegakernelPathTracer.normalW")
    g.addEdge("GBufferRT.tangentW", "MegakernelPathTracer.tangentW")
    g.addEdge("GBufferRT.faceNormalW", "MegakernelPathTracer.faceNormalW")
    g.addEdge("GBufferRT.mtlData", "MegakernelPathTracer.mtlData")
    g.addEdge("GBufferRT.texC", "MegakernelPathTracer.texC")
    g.addEdge("GBufferRT.texGrads", "MegakernelPathTracer.texGrads")    # Required for texture filtering at primary hits (optional).
    g.addEdge("GBufferRT.viewW", "MegakernelPathTracer.viewW")          # Required for correct depth-of-field (optional).
    g.addEdge("GBufferRT.vbuffer", "MegakernelPathTracer.vbuffer")      # Required by ray footprint (optional).
    g.addEdge("MegakernelPathTracer.color", "AccumulatePass.input")
    g.addEdge("AccumulatePass.output", "SVGFPass.Color")
    g.addEdge("MegakernelPathTracer.albedo", "SVGFPass.Albedo")
    g.addEdge("GBufferRT.mvec", "SVGFPass.MotionVec")
    g.addEdge("GBufferRT.normW", "SVGFPass.WorldNormal")
    g.addEdge("GBufferRT.posW", "SVGFPass.WorldPosition")
    g.addEdge("GBufferRaster.emissive", "SVGFPass.Emission")
    g.addEdge("GBufferRaster.linearZ", "SVGFPass.LinearZ")
    g.addEdge("GBufferRaster.pnFwidth", "SVGFPass.PositionNormalFwidth")
    g.addEdge("SVGFPass.Filtered image", "ToneMapper.src")
    g.addEdge("ToneMapper.dst", "InteractionPass.src")
    g.addEdge("InteractionPass.dst", "OutlinePass.input")
    g.markOutput("InteractionPass.dst")
    return g

MegakernelPathTracer = render_graph_MegakernelPathTracer()
try: m.addGraph(MegakernelPathTracer)
except NameError: None
