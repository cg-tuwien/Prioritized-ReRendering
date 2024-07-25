# Real-Time Editing of Path-Traced Scenes with Prioritized Re-Rendering
[Paper](https://www.cg.tuwien.ac.at/research/publications/2024/ulschmid-2024-reo/)

## Framework Setup

Download:
- [NVAPI R470](https://developer.nvidia.com/gameworksdownload#?search=nvapi)
- [CUDA 11.3.1](https://developer.nvidia.com/cuda-11-3-1-download-archive)
- [NVIDIA OptiX 7.3](https://developer.nvidia.com/designworks/optix/downloads/legacy)

Copy into `Source/Externals/.packman/` and rename:
- the extracted `R470-developer` to `nvapi`
- `C:\Program Files\NVIDIA GPU Computing Tools\CUDA\v.11.3` to `cuda`
- `C:\ProgramData\NVIDIA Corporation\OptiX SDK 7.3.0` to `optix`

See Falcor ReadMe for further details.

Open `Framework/Falcor.sln`, select `ReleaseD3D12` as build configuration and set `Mogwai` as startup project. Build the entire RenderPass folder before building Mogwai. When running the framework, select `Source/Mogwai/Data/PathTracer_Optix.py` as script and the PBRT version (`scene-v4.pbrt`) of for example one of the scenes from [Benedikt Bitterli's Rendering Resources](https://benedikt-bitterli.me/resources/).

We hid parts of the UI during the User Study. To view the full UI set `DEBUG_UI` to 1 in `Falcor/Core/FalcorConfig.h`.

### Structure

Our main contribution to the Falcor Framework are the `InteractionPass` and `OutlinePass`, as well as the reduction compute shader `Reduce.slang` in the `MegakernelPathTracer`.

## Evaluation Setup

TODO
