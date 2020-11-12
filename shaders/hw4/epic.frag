#version 330

in vec4 fragmentColor;
in vec4 vertexWorldPosition;
in vec3 vertexWorldNormal;

out vec4 finalColor;

uniform InputMaterial {
    float matRoughness;
    vec4 matSpecular;
    float matMetallic;
} material;

struct LightProperties {
    vec4 diffuseColor;
    vec4 specularColor;
    vec4 directionalLightDir;
    float spotInnerConeAngleDegrees;
    float spotOuterConeAngleDegrees;
};
uniform LightProperties genericLight;

struct PointLight {
    vec4 pointPosition;
};
uniform PointLight pointLight;

uniform vec4 cameraPosition;

uniform float constantAttenuation;
uniform float linearAttenuation;
uniform float quadraticAttenuation;

uniform int lightingType;

const float PI = 3.1415926535;

float clampedDot(vec4 A, vec4 B)
{
    return max(0, dot(A,B));
}

float computeG1(vec4 N, vec4 v, float k)
{
    return clampedDot(N, v) / (clampedDot(N, v) * (1 - k) + k);
}

vec4 pointLightSubroutine(vec4 N, vec4 worldPosition, vec3 worldNormal)
{
    // Direction from the surface to the point light
    vec4 L = normalize(pointLight.pointPosition - vertexWorldPosition);
    float NdL = clampedDot(N,L);
//    vec4 cFinal = vec4(NdL);

    // Insert code for Section 3.2 here.
    vec4 V = normalize(cameraPosition - vertexWorldPosition);
    vec4 H = normalize(L + V);
    vec4 cDiff = (1 - material.matMetallic) * fragmentColor;
    vec4 cSpec = mix(0.08 * material.matSpecular, fragmentColor, material.matMetallic);
    float alpha = material.matRoughness * material.matRoughness;

    vec4 d = cDiff / PI;
    vec4 brdf = d * genericLight.diffuseColor;
    
    float D = pow(alpha, 2) / (PI * pow(pow(clampedDot(N, H), 2) * (pow(alpha, 2) - 1.0) + 1.0, 2));
    float k = pow(material.matRoughness + 1.0, 2) / 8.0;
    float G = computeG1(N, L, k) * computeG1(N, V, k);
    vec4 F = cSpec + (1 - cSpec) * pow(2, (-5.55473 * clampedDot(V,H) - 6.98316) * clampedDot(V, H));
    float denom = 4.0 * clampedDot(N, L) * clampedDot(N, V);
    if(denom != 0.0) {
        vec4 s = D * F * G / denom;
        brdf = brdf + s * genericLight.specularColor;
    }
    vec4 cFinal = vec4(NdL) * brdf;

    return cFinal;
}

vec4 directionalLightSubroutine(vec4 N, vec4 worldPosition, vec3 worldNormal)
{
    // Insert code for Section 3.3 here.
    return vec4(0.0);
}

vec4 hemisphereLightSubroutine(vec4 N, vec4 worldPosition, vec3 worldNormal)
{
    // Insert code for Section 3.4 here.
    return vec4(0.0);
}

vec4 spotLightSubroutine(vec4 N, vec4 worldPosition, vec3 worldNormal)
{
    // Insert code for Section 3.5 here.
    return vec4(0.0);
}

vec4 globalLightSubroutine(vec4 worldPosition, vec3 worldNormal)
{
    return vec4(0.0);
}

vec4 attenuateLight(vec4 originalColor)
{
    float lightDistance = length(pointLight.pointPosition - vertexWorldPosition);
    float attenuation = 1.0 / (constantAttenuation + lightDistance * linearAttenuation + lightDistance * lightDistance * quadraticAttenuation);
    return originalColor * attenuation;
}

void main()
{
    // Normal to the surface
    vec4 N = vec4(normalize(vertexWorldNormal), 0.f);

    vec4 lightingColor = vec4(0);
    if (lightingType == 0) {
        lightingColor = globalLightSubroutine(vertexWorldPosition, vertexWorldNormal);
    } else if (lightingType == 1) {
        lightingColor = attenuateLight(pointLightSubroutine(N, vertexWorldPosition, vertexWorldNormal));
    } else if (lightingType == 2) {
        lightingColor = directionalLightSubroutine(N, vertexWorldPosition, vertexWorldNormal);
    } else if (lightingType == 3) {
        lightingColor = hemisphereLightSubroutine(N, vertexWorldPosition, vertexWorldNormal);
    } else if (lightingType == 5) {
        lightingColor = attenuateLight(spotLightSubroutine(N, vertexWorldPosition, vertexWorldNormal));
    }
    finalColor = lightingColor * fragmentColor;
    finalColor.a = 1.0;
}

