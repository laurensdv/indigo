#version 300 es

precision lowp float;

layout (location = 0) in vec4 a_verticesAndCoords; // a_vertices, a_texcoord
layout (location = 1) in vec4 a_transform; // a_translation, a_scale
layout (location = 2) in vec4 a_frameTransform; // a_frameTranslation, a_frameScale
layout (location = 3) in vec4 a_dimensions; // a_ref, a_size
layout (location = 4) in vec4 a_tint;
layout (location = 5) in vec4 a_gradiantPositions; // a_gradiantOverlayFrom, a_gradiantOverlayTo
layout (location = 6) in vec4 a_gradiantOverlayFromColor;
layout (location = 7) in vec4 a_gradiantOverlayToColor;
layout (location = 8) in vec4 a_borderColor;
layout (location = 9) in vec4 a_glowColor;
layout (location = 10) in vec4 a_amounts; // a_outerBorderAmount, a_innerBorderAmount, a_outerGlowAmount, a_innerGlowAmount
layout (location = 11) in vec4 a_rotationAlphaFlipHFlipV; // a_rotation, a_alpha, a_fliph, a_flipv
layout (location = 12) in vec4 a_emissiveNormalOffsets; // a_emissive (vec2), a_normal (vec2)
layout (location = 13) in vec4 a_specularOffsetIsLit; // a_specular (vec2), a_isLit (float), ???
layout (location = 14) in vec4 a_textureAmounts; // albedoAmount (float), emissiveAmount (float), normalAmount (float), specularAmount (float)

uniform mat4 u_projection;

// The varying values are organised this way to help the packer
// squash them into 15 varying vectors
out vec4 v_texcoordEmissiveNormal;
out vec4 v_relativeScreenCoordsIsLitAlpha;
out vec4 v_tint;
out vec4 v_gradiantFromTo;
out vec4 v_gradiantOverlayFromColor;
out vec4 v_gradiantOverlayToColor;
out vec4 v_borderColor;
out vec4 v_glowColor;
out vec4 v_effectAmounts;
out vec4 v_textureAmounts;
out vec2 v_offsetTL;
out vec2 v_offsetTC;
out vec2 v_offsetTR;
out vec2 v_offsetML;
out vec2 v_offsetMC;
out vec2 v_offsetMR;
out vec2 v_offsetBL;
out vec2 v_offsetBC;
out vec2 v_offsetBR;
out vec2 v_texcoordSpecular;

mat4 rotate2d(float angle){
    return mat4(cos(angle), -sin(angle), 0, 0,
                sin(angle), cos(angle), 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1
                );
}

mat4 translate2d(vec2 t){
    return mat4(1, 0, 0, 0,
                0, 1, 0, 0,
                0, 0, 1, 0,
                t.x, t.y, 0, 1
                );
}

mat4 scale2d(vec2 s){
    return mat4(s.x, 0, 0, 0,
                0, s.y, 0, 0,
                0, 0, 1, 0,
                0, 0, 0, 1
                );
}

vec2 scaleTexCoordsWithOffset(vec2 texcoord, vec2 offset){
  mat4 transform = translate2d(offset) * scale2d(a_frameTransform.zw);
  return (transform * vec4(texcoord, 1.0, 1.0)).xy;
}

vec2 scaleTexCoords(vec2 texcoord){
  return scaleTexCoordsWithOffset(texcoord, a_frameTransform.xy);
}

vec2 sizeOfAPixel() {
  return (scale2d(1.0 / a_dimensions.zw) * vec4(1.0)).xy;
}

const vec2[9] gridOffsets = vec2[9](
  vec2(-1.0, -1.0),
  vec2(0.0, -1.0),
  vec2(1.0, -1.0),

  vec2(-1.0, 0.0),
  vec2(0.0, 0.0),
  vec2(1.0, 0.0),

  vec2(-1.0, 1.0),
  vec2(0.0, 1.0),
  vec2(1.0, 1.0)
);

vec2[9] generateTexCoords3x3(vec2 texcoords, vec2 onePixel) {
  return vec2[9](
    scaleTexCoords(texcoords + (onePixel * gridOffsets[0])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[1])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[2])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[3])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[4])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[5])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[6])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[7])),
    scaleTexCoords(texcoords + (onePixel * gridOffsets[8]))
  );
}

void main(void) {

  vec4 vertices = vec4(a_verticesAndCoords.xy, 1.0, 1.0);
  vec2 texcoords = a_verticesAndCoords.zw;
  vec2 ref = a_dimensions.xy;
  vec2 size = a_dimensions.zw;
  vec2 translation = a_transform.xy;
  vec2 scale = a_transform.zw;
  float rotation = a_rotationAlphaFlipHFlipV.x;
  float alpha = a_rotationAlphaFlipHFlipV.y;
  vec2 flip = a_rotationAlphaFlipHFlipV.zw;
  vec2 texcoordsEmissive = a_emissiveNormalOffsets.xy;
  vec2 texcoordsNormal = a_emissiveNormalOffsets.zw;
  vec2 texcoordsSpecular = a_specularOffsetIsLit.xy;
  float isLit = a_specularOffsetIsLit.z;

  vec2 offsets[9] = generateTexCoords3x3(texcoords, sizeOfAPixel());

  vec2 moveToReferencePoint = -(ref / size) + 0.5;

  mat4 transform = 
    translate2d(translation) *
    rotate2d(rotation) *
    scale2d(size * scale) *
    translate2d(moveToReferencePoint) *
    scale2d(flip);

  gl_Position = u_projection * transform * vertices;

  v_texcoordEmissiveNormal = vec4(scaleTexCoordsWithOffset(texcoords, texcoordsEmissive), scaleTexCoordsWithOffset(texcoords, texcoordsNormal));
  v_relativeScreenCoordsIsLitAlpha = vec4(texcoords * size, isLit, alpha);
  v_tint = a_tint;
  v_gradiantFromTo = a_gradiantPositions;
  v_gradiantOverlayFromColor = a_gradiantOverlayFromColor;
  v_gradiantOverlayToColor = a_gradiantOverlayToColor;
  v_borderColor = a_borderColor;
  v_glowColor = a_glowColor;
  v_effectAmounts = a_amounts;
  v_textureAmounts = a_textureAmounts;
  v_offsetTL = offsets[0];
  v_offsetTC = offsets[1];
  v_offsetTR = offsets[2];
  v_offsetML = offsets[3];
  v_offsetMC = offsets[4];
  v_offsetMR = offsets[5];
  v_offsetBL = offsets[6];
  v_offsetBC = offsets[7];
  v_offsetBR = offsets[8];
  v_texcoordSpecular = scaleTexCoordsWithOffset(texcoords, texcoordsSpecular);


}
