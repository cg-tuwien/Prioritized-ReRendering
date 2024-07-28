def render_graph_MegakernelPathTracer():
    g = RenderGraph("MegakernelPathTracer")
    loadRenderPassLibrary("AccumulatePass.dll")
    loadRenderPassLibrary("GBuffer.dll")
    loadRenderPassLibrary("ToneMapper.dll")
    loadRenderPassLibrary("MegakernelPathTracer.dll")
    loadRenderPassLibrary("InteractionPass.dll")
    loadRenderPassLibrary("OptixDenoiser.dll")
    loadRenderPassLibrary("OutlinePass.dll")

    AccumulatePass = createPass("AccumulatePass", {"enabled": True})
    g.addPass(AccumulatePass, "AccumulatePass")
    ToneMappingPass = createPass("ToneMapper", {"autoExposure": False, "exposureCompensation": 0.0})
    g.addPass(ToneMappingPass, "ToneMappingPass")
    GBufferRT = createPass("GBufferRT", {"forceCullMode": False, "cull": CullMode.CullBack, "samplePattern": SamplePattern.Stratified, "sampleCount": 16})
    g.addPass(GBufferRT, "GBufferRT")
    MegakernelPathTracer = createPass("MegakernelPathTracer", {"params": PathTracerParams(useVBuffer=0)})
    g.addPass(MegakernelPathTracer, "MegakernelPathTracer")
    InteractionPass = createPass("InteractionPass")
    g.addPass(InteractionPass, "InteractionPass")
    OptixDenoiser = createPass("OptixDenoiser")
    g.addPass(OptixDenoiser, "OptixDenoiser")
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
    g.addEdge("AccumulatePass.output", "OptixDenoiser.color")
    g.addEdge("MegakernelPathTracer.albedo", "OptixDenoiser.albedo")
    g.addEdge("GBufferRT.mvec", "OptixDenoiser.mvec")
    g.addEdge("GBufferRT.normW", "OptixDenoiser.normal")
    g.addEdge("OptixDenoiser.output", "ToneMappingPass.src")
    g.addEdge("ToneMappingPass.dst", "InteractionPass.src")
    g.addEdge("InteractionPass.dst", "OutlinePass.input")
    
    g.markOutput("OutlinePass.output")
    g.markOutput("MegakernelPathTracer.test")

    return g

MegakernelPathTracer = render_graph_MegakernelPathTracer()
try: m.addGraph(MegakernelPathTracer)
except NameError: None
