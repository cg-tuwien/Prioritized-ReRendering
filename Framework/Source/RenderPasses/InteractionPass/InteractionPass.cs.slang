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

#include "Scene/SceneDefines.slangh"

import Scene.Raytracing;
import Scene.RaytracingInline;
import Scene.Intersection;

import SharedTypes;

struct InteractionPass
{
    InteractionPassParams params;

    RWStructuredBuffer<PixelData> pixelData;

    /** Run interaction pass for the given pixel.
    */
    void execute(const uint2 pixel)
    {
        if (any(pixel >= params.frameDim)) return;

        // Initialize pixel data for the selected pixel.
        if (all(pixel == params.selectedPixel))
        {
            pixelData[0].meshInstanceID = PixelData::kInvalidID;
            pixelData[0].meshID = PixelData::kInvalidID;
            pixelData[0].materialID = PixelData::kInvalidID;
            pixelData[0].curveInstanceID = PixelData::kInvalidID;
            pixelData[0].curveID = PixelData::kInvalidID;
        }

        // Trace primary ray.
        Ray ray = gScene.camera.computeRayPinhole(pixel, params.frameDim);
        RayDesc rayDesc = ray.toRayDesc();

        RayQuery<RAY_FLAG_NONE> rayQuery;

        rayQuery.TraceRayInline(
            gScene.rtAccel,
            RAY_FLAG_NONE, // OR'd with template flags above
            0xff, // InstanceInclusionMask
            rayDesc);

        CurveIntersector::Attribs curveCommittedAttribs;
        DisplacedTriangleMeshIntersector::Attribs displacedTriangleMeshCommittedAttribs;
        SDFGridHitData sdfGridCommittedHitData;

        while (rayQuery.Proceed())
        {
            if (rayQuery.CandidateType() == CANDIDATE_NON_OPAQUE_TRIANGLE)
            {
                // Alpha test for non-opaque geometry.
                const TriangleHit hit = getCandidateTriangleHit(rayQuery);
                const VertexData v = gScene.getVertexData(hit);
                const uint materialID = gScene.getMaterialID(hit.instanceID);

                if (gScene.materials.alphaTest(v, materialID, 0.f)) continue;

                rayQuery.CommitNonOpaqueTriangleHit();
            }
            else if (rayQuery.CandidateType() == CANDIDATE_PROCEDURAL_PRIMITIVE)
            {
                const GeometryInstanceID instanceID = GeometryInstanceID(rayQuery.CandidateInstanceID(), rayQuery.CandidateGeometryIndex());
                const uint primitiveIndex = rayQuery.CandidatePrimitiveIndex();
                const GeometryType type = gScene.getGeometryInstanceType(instanceID);
                const Ray raySegment = Ray(ray.origin, ray.dir, rayQuery.RayTMin(), rayQuery.CommittedRayT());
                switch (type)
                {
                case GeometryType::DisplacedTriangleMesh:
                    {
                        DisplacedTriangleMeshIntersector::Attribs attribs;
                        float t;
                        if (DisplacedTriangleMeshIntersector::intersect(raySegment, instanceID, primitiveIndex, attribs, t))
                        {
                            rayQuery.CommitProceduralPrimitiveHit(t);
                            displacedTriangleMeshCommittedAttribs = attribs;
                        }
                    }
                    break;
                case GeometryType::Curve:
                    {
                        CurveIntersector::Attribs attribs;
                        float t;
                        if (CurveIntersector::intersect(raySegment, instanceID, primitiveIndex, attribs, t))
                        {
                            rayQuery.CommitProceduralPrimitiveHit(t);
                            curveCommittedAttribs = attribs;
                        }
                    }
                    break;
                case GeometryType::SDFGrid:
                    {
                        SDFGridHitData sdfGridHitData;
                        float t;
                        if (SDFGridIntersector::intersect(raySegment, instanceID, primitiveIndex, sdfGridHitData, t))
                        {
                            rayQuery.CommitProceduralPrimitiveHit(t);
                            sdfGridCommittedHitData = sdfGridHitData;
                        }
                    }
                    break;
                default:
                    // Ignore other types of geometry.
                    ;
                }
            }
        }
        
        // Process hit/miss.
        HitInfo hit = {};
        float hitT = 0.f;

        if (rayQuery.CommittedStatus() == COMMITTED_TRIANGLE_HIT)
        {
            hit = HitInfo(getCommittedTriangleHit(rayQuery));
            hitT = rayQuery.CommittedRayT();
            handleHit(pixel, ray.dir, hit);
        }
        else if (rayQuery.CommittedStatus() == COMMITTED_PROCEDURAL_PRIMITIVE_HIT)
        {
            const GeometryInstanceID instanceID = GeometryInstanceID(rayQuery.CommittedInstanceID(), rayQuery.CommittedGeometryIndex());
            const uint primitiveIndex = rayQuery.CommittedPrimitiveIndex();
            const GeometryType type = gScene.getGeometryInstanceType(instanceID);
            switch (type)
            {
            case GeometryType::DisplacedTriangleMesh:
                {
                    DisplacedTriangleHit displacedTriangleHit;
                    displacedTriangleHit.instanceID = instanceID;
                    displacedTriangleHit.primitiveIndex = primitiveIndex;
                    displacedTriangleHit.barycentrics = displacedTriangleMeshCommittedAttribs.barycentrics;
                    displacedTriangleHit.displacement = displacedTriangleMeshCommittedAttribs.displacement;
                    hit = HitInfo(displacedTriangleHit);
                    hitT = rayQuery.CommittedRayT();
                    handleHit(pixel, ray.dir, hit);
                    break;
                }
            case GeometryType::Curve:
                {
                    CurveHit curveHit;
                    curveHit.instanceID = instanceID;
                    curveHit.primitiveIndex = primitiveIndex;
                    curveHit.barycentrics = curveCommittedAttribs.barycentrics;
                    hit = HitInfo(curveHit);
                    hitT = rayQuery.CommittedRayT();
                    const float3 curveHitPosW = ray.origin + ray.dir * hitT;
                    handleHit(pixel, ray.dir, hit, curveHitPosW);
                    break;
                }
            case GeometryType::SDFGrid:
                SDFGridHit sdfGridHit;
                sdfGridHit.instanceID = instanceID;
                sdfGridHit.hitData = sdfGridCommittedHitData;
                hit = HitInfo(sdfGridHit);
                hitT = rayQuery.CommittedRayT();
                handleHit(pixel, ray.dir, hit);
                break;
            default:
                // Should not happen.
                ;
            }
        }
    }

    void handleHit(const uint2 pixel, const float3 dir, const HitInfo hit, const float3 curveHitPosW = {})
    {
        uint materialID = PixelData::kInvalidID;

        uint meshID = PixelData::kInvalidID;
        GeometryInstanceID geometryInstanceID;
        uint meshInstanceID = PixelData::kInvalidID;

        uint curveID = PixelData::kInvalidID;
        uint curveInstanceID = PixelData::kInvalidID;

        VertexData v;
        ShadingData sd;

        switch (hit.getType())
        {
        case HitType::Triangle:
        {
            geometryInstanceID = hit.getTriangleHit().instanceID;
            meshInstanceID = geometryInstanceID.index;
            meshID = gScene.getGeometryInstance(geometryInstanceID).geometryID;
            materialID = gScene.getMaterialID(geometryInstanceID);

            break;
        }
        case HitType::Curve:
        {
            geometryInstanceID = hit.getCurveHit().instanceID;
            curveInstanceID = geometryInstanceID.index;
            curveID = gScene.getGeometryInstance(geometryInstanceID).geometryID;
            materialID = gScene.getMaterialID(geometryInstanceID);

            break;
        }
        case HitType::DisplacedTriangle:
        {
            geometryInstanceID = hit.getDisplacedTriangleHit().instanceID;
            meshInstanceID = geometryInstanceID.index;
            meshID = gScene.getGeometryInstance(geometryInstanceID).geometryID;
            materialID = gScene.getMaterialID(geometryInstanceID);

            break;
        }
        case HitType::SDFGrid:
        {
            geometryInstanceID = hit.getSDFGridHit().instanceID;
            meshInstanceID = geometryInstanceID.index;
            meshID = gScene.getGeometryInstance(geometryInstanceID).geometryID;
            materialID = gScene.getMaterialID(geometryInstanceID);

            break;
        }
        default:
            // Should not happen.
            return;
        }
        
        // Write pixel data for the selected pixel.
        if (all(pixel == params.selectedPixel))
        {
            pixelData[0].meshInstanceID = meshInstanceID;
            pixelData[0].meshID = meshID;
            pixelData[0].materialID = materialID;
            pixelData[0].curveInstanceID = curveInstanceID;
            pixelData[0].curveID = curveID;
        }
    }
};

cbuffer CB
{
    InteractionPass gInteractionPass;
}

/** Compute shader entry point for interaction pass.
*/
[numthreads(16, 16, 1)]
void main(uint3 dispatchThreadId : SV_DispatchThreadID)
{
    gInteractionPass.execute(dispatchThreadId.xy);
}
