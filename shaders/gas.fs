#version 140

// sun.fs

#include "prelight_include.fs"
#include "generic_include.fs"

uniform vec4 GBufferDepth_range;

uniform vec4 Time;
uniform vec3 cameraPos;

uniform vec4 PlanetParams;

uniform sampler2D DistortTexture;
uniform sampler2D ColorMapTexture;

in vec3 v_Forward;
in vec3 v_Normal;
in vec3 v_Position_ObjectSpace;

out vec4 out_FragData[4];

const float PI = 3.1415926535897932384626433832795;

vec3 CalculateSurfaceGradient(vec3 normal, vec3 dpdx, vec3 dpdy, float dhdx, float dhdy)
{
    vec3 r1 = cross(dpdy, normal);
    vec3 r2 = cross(normal, dpdx);
 
    return (r1 * dhdx + r2 * dhdy) / dot(dpdx, r1);
}

vec3 PerturbNormal(vec3 normal, vec3 dpdx, vec3 dpdy, float dhdx, float dhdy)
{
    return normalize(normal - CalculateSurfaceGradient(normal, dpdx, dpdy, dhdx, dhdy));
}

vec3 CalculateSurfaceNormal(vec3 position, vec3 normal, float height)
{
    vec3 dpdx = dFdx(position);
    vec3 dpdy = dFdy(position);
 
    float dhdx = dFdx(height);
    float dhdy = dFdy(height);
 
    return PerturbNormal(normal, dpdx, dpdy, dhdx, dhdy);
}

void main()
{
    float planetSeed = PlanetParams.x;
    float temperature = PlanetParams.y;
    float planetRadius = PlanetParams.z;

    float time = Time.x * 1;

    vec3 normal = vec3(0.0, 0.0, 1.0);

    vec3 viewNormal = v_Normal;

    vec3 normal_objectSpace = normalize(v_Position_ObjectSpace);
    vec3 sign_objectSpace = sign(v_Position_ObjectSpace);

    float poleRotAngle = time * 0.0125;
    float poleRotCos = cos(poleRotAngle);
    float poleRotSin = sin(poleRotAngle);
    mat2 poleRotate = mat2(poleRotCos, -poleRotSin, poleRotSin, poleRotCos);


    vec2 distortTextureSize = textureSize(DistortTexture, 0);

    float TurbulenceValue = 0.005;

    vec2 smallDistortTexScale = vec2(150.0, 130.0);
    float smallDistortStrength = 0.0025;
    float distortTexScale = 2000.0;
    float distortStrength = TurbulenceValue * 10.0;
    float bigDistortTexScale = 10500.0;
    float bigDistortStrength = TurbulenceValue;

    float poleRotAngle2 = time * -0.0125;
    float poleRotCos2 = cos(poleRotAngle2);
    float poleRotSin2 = sin(poleRotAngle2);
    mat2 poleRotate2 = mat2(poleRotCos2, -poleRotSin2, poleRotSin2, poleRotCos2);

    vec2 topUV = poleRotate2 * (v_Position_ObjectSpace.xy / bigDistortTexScale) + mod(planetSeed / 15919.0, 1.0);
    if (normal_objectSpace.z < 0.0)
        topUV += 0.5;


    float texCylBlend = smoothstep(0.0, 1.0, clamp(acos(abs(normal_objectSpace.z)) * 5.0 - 0.75, 0.0, 1.0));
    float radialBlend = smoothstep(0.0, 1.0, abs(atan(normal_objectSpace.x, normal_objectSpace.y) / PI) * 1.05 - 0.025);

    vec2 U = vec2(
        atan(normal_objectSpace.x, normal_objectSpace.y) * 0.5,
        (atan(-normal_objectSpace.x, -normal_objectSpace.y) - PI) * 0.5);
    float V = (sqrt(1.0 - normal_objectSpace.z * normal_objectSpace.z) - 1.0) * sign(normal_objectSpace.z) * planetRadius * 0.5;
    
    float noise2_V = acos(normal_objectSpace.z) * planetRadius / bigDistortTexScale;

    vec4 noise2Z_raw = texture(DistortTexture, topUV);
    vec4 noise2A_raw = texture(DistortTexture, vec2((U.x + poleRotAngle2 / PI) * (planetRadius / bigDistortTexScale), noise2_V));
    vec4 noise2B_raw = texture(DistortTexture, vec2((U.y + poleRotAngle2 / PI) * (planetRadius / bigDistortTexScale), noise2_V));

    vec4 noise2_raw = mix(
            noise2Z_raw,
            mix(
                noise2A_raw,
                noise2B_raw,
                radialBlend
            ),
            texCylBlend
        );

    noise2_raw = noise2_raw * 2.0 - 1.0;

    vec2 stripeDirUV = vec2(mod(planetSeed / 7919.0, 1.0), mod(planetSeed / 6619.0, 1.0) + v_Position_ObjectSpace.z / 10000.0 + noise2_raw.z * bigDistortStrength);
    vec4 stripeDir_raw = texture(DistortTexture, stripeDirUV);
    float stripeDir = stripeDir_raw.x * 2.0 - 1.0;

    float poleRotAngleA = time * 0.025 - stripeDir * 0.25;
    float poleRotCosA = cos(poleRotAngleA);
    float poleRotSinA = sin(poleRotAngleA);
    mat2 poleRotateA = mat2(poleRotCosA, -poleRotSinA, poleRotSinA, poleRotCosA);

    float noiseSmallDistortV = V / distortTexScale + noise2_raw.x * 0.25;

    noiseSmallDistortV *= distortTextureSize.y;
    noiseSmallDistortV = floor(noiseSmallDistortV) + smoothstep(0.0, 1.0, fract(noiseSmallDistortV));
    noiseSmallDistortV /= distortTextureSize.y;

    vec4 noiseZ_raw = texture(DistortTexture, poleRotateA * (v_Position_ObjectSpace.xy / distortTexScale * 0.5) + noise2_raw.xy * 0.125);
    vec4 noiseA_raw = texture(DistortTexture, vec2((U.x + poleRotAngleA / PI) * (planetRadius / distortTexScale), noiseSmallDistortV));
    vec4 noiseB_raw = texture(DistortTexture, vec2((U.y + poleRotAngleA / PI) * (planetRadius / distortTexScale), noiseSmallDistortV));

    vec3 noise_raw = mix(
            noiseZ_raw.rgb,
            mix(
                noiseA_raw.rgb,
                noiseB_raw.rgb,
                radialBlend
            ),
            texCylBlend
        );
    vec3 noiseSmallDistort = noise_raw * 2.0 - 1.0;

    float smallNoiseAdjScale = 4.82;
    float smallNoiseBlendV = acos(normal_objectSpace.z) * planetRadius / smallDistortTexScale.y / smallNoiseAdjScale;

    vec4 smallNoiseBlendZ_raw = texture(DistortTexture, poleRotate * (v_Position_ObjectSpace.xy / smallDistortTexScale.y) / smallNoiseAdjScale);
    vec4 smallNoiseBlendA_raw = texture(DistortTexture, vec2((U.x + poleRotAngle / PI) * planetRadius / smallDistortTexScale.x / smallNoiseAdjScale, smallNoiseBlendV));
    vec4 smallNoiseBlendB_raw = texture(DistortTexture, vec2((U.y + poleRotAngle / PI) * planetRadius / smallDistortTexScale.x / smallNoiseAdjScale, smallNoiseBlendV));

    vec3 smallNoiseBlend_raw = mix(
            smallNoiseBlendZ_raw.rgb,
            mix(
                smallNoiseBlendA_raw.rgb,
                smallNoiseBlendB_raw.rgb,
                radialBlend
            ),
            texCylBlend
        );

    
    float smallNoiseV = acos(normal_objectSpace.z) * planetRadius / smallDistortTexScale.y;

    vec4 noise3Z_raw = texture(DistortTexture, poleRotate * (v_Position_ObjectSpace.xy / smallDistortTexScale.y + noiseSmallDistort.xy * distortStrength));
    vec4 noise3A_raw = texture(DistortTexture, vec2((U.x + poleRotAngle / PI) * planetRadius / smallDistortTexScale.x, smallNoiseV) + noiseSmallDistort.xy * distortStrength + stripeDir * 1.0);
    vec4 noise3B_raw = texture(DistortTexture, vec2((U.y + poleRotAngle / PI) * planetRadius / smallDistortTexScale.x, smallNoiseV) + noiseSmallDistort.xy * distortStrength + stripeDir * 1.0);

    vec3 noise3_raw = mix(
            noise3Z_raw.rgb,
            mix(
                noise3A_raw.rgb,
                noise3B_raw.rgb,
                radialBlend
            ),
            texCylBlend
        );

    vec3 noise3Blend = pow(smallNoiseBlend_raw.rgb, vec3(8.0));
    noise3Blend = noise3Blend.rgb / (noise3Blend.x + noise3Blend.y + noise3Blend.z);
    float noise3 = noise3_raw.r * noise3Blend.r + noise3_raw.g * noise3Blend.g + noise3_raw.b * noise3Blend.b;
    noise3 = noise3 * 2.0 - 1.0;

    vec2 stripesUV = vec2(mod(planetSeed / 7919.0, 1.0), v_Position_ObjectSpace.z / 8000.0 + vec2(noiseSmallDistort.x * 0.0125, 0.0));
    // stripesUV.t = (floor(stripesUV.t * distortTextureSize.y) + smoothstep(0.0, 1.0, mod(stripesUV.t * distortTextureSize.y, 1.0))) / distortTextureSize.y;

    vec4 stripes = texture(DistortTexture, stripesUV);

    viewNormal = CalculateSurfaceNormal(v_Forward, v_Normal, stripes.x * 3.0 + noiseSmallDistort.y * 0.5 + noise3 * 0.15);

    stripes = clamp((stripes - 0.5) * 2.0 + 0.5, 0.0, 1.0);

    // float ndotv = clamp(dot(viewNormal, normalize(v_Forward)), 0.0, 1.0);
    vec2 colorMapSize = textureSize(ColorMapTexture, 0);
    float invColorMapWidth = 1.0 / colorMapSize.x;

    vec2 diffuseLUTUV = vec2(
        invColorMapWidth * 0.5 + temperature,
        clamp(0.025 + stripes.x * 0.95 + (noise3 * 0.05), 0.0, 1.0) * 0.5 + 0.25
        );
    vec4 diffuse_raw = texture(ColorMapTexture, diffuseLUTUV);
    vec4 fog_raw = texture(ColorMapTexture, vec2(diffuseLUTUV.s, 0.5));
    vec3 diffuse = mix(diffuse_raw.rgb, fog_raw.rgb, pow(1.0 - clamp(dot(v_Normal, -normalize(v_Forward)), 0.0, 1.0), 2.0));
    // diffuse *= 2.5;

    // diffuse = abs(tbn * stripeNormals);
    // diffuse = abs(stripeNormals);

    // vec4 stripes2 = texture(NoiseTexture, uvs + vec2(0.0, 1.0 / 512.0));
    // float stripesGradient = abs(stripes.x - stripes2.x);
    // diffuse *= vec3(clamp(stripesGradient, 0.0, 1.0)) * 0.5 + 0.5;

    // diffuse = vec3(abs(smallNoiseBlend_raw));
    // diffuse = vec3(abs(diffuse_raw.a));

    vec3 ambientColor = calcAmbient(viewNormal, v_Forward);
    vec3 ambient = ambientColor * diffuse.rgb + diffuse_raw.a * diffuse.rgb;

    out_FragData[0] = vec4(ambient, 1.0);
    out_FragData[1] = vec4(diffuse.rgb, 0.2);
    out_FragData[2] = vec4(length(v_Forward) * GBufferDepth_range.z - GBufferDepth_range.w, 0.0, 0.0, 1.0);
    out_FragData[3] = vec4(encodeViewNormal(viewNormal), 0.65);
}
