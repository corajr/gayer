// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";

var reinit = function (texture,src){return texture(src);};

var triangleSpec = {
  frag: "\n     precision mediump float;\n     uniform vec4 color;\n     void main () {\n         gl_FragColor = color;\n     }\n  ",
  vert: "\n     precision mediump float;\n     attribute vec2 position;\n     void main () {\n       gl_Position = vec4(position, 0, 1);\n     }\n  ",
  attributes: {
    position: /* array */[
      /* array */[
        -1,
        0
      ],
      /* array */[
        0,
        -1
      ],
      /* array */[
        1,
        1
      ]
    ]
  },
  uniforms: {
    color: /* array */[
      1.0,
      0.0,
      0.0,
      1.0
    ]
  },
  count: 3
};

function sobelSpec(regl) {
  return {
          frag: "\n     precision mediump float;\n     uniform sampler2D texture;\n     uniform vec2 resolution;\n     varying vec2 uv;\n\n     void main () {\n\tfloat x = 1.0 / resolution.x;\n\tfloat y = 1.0 / resolution.y;\n\tvec4 horizEdge = vec4( 0.0 );\n\thorizEdge -= texture2D(texture, vec2( uv.x - x, uv.y - y ) ) * 1.0;\n\thorizEdge -= texture2D(texture, vec2( uv.x - x, uv.y     ) ) * 2.0;\n\thorizEdge -= texture2D(texture, vec2( uv.x - x, uv.y + y ) ) * 1.0;\n\thorizEdge += texture2D(texture, vec2( uv.x + x, uv.y - y ) ) * 1.0;\n\thorizEdge += texture2D(texture, vec2( uv.x + x, uv.y     ) ) * 2.0;\n\thorizEdge += texture2D(texture, vec2( uv.x + x, uv.y + y ) ) * 1.0;\n\tvec4 vertEdge = vec4( 0.0 );\n\tvertEdge -= texture2D(texture, vec2( uv.x - x, uv.y - y ) ) * 1.0;\n\tvertEdge -= texture2D(texture, vec2( uv.x    , uv.y - y ) ) * 2.0;\n\tvertEdge -= texture2D(texture, vec2( uv.x + x, uv.y - y ) ) * 1.0;\n\tvertEdge += texture2D(texture, vec2( uv.x - x, uv.y + y ) ) * 1.0;\n\tvertEdge += texture2D(texture, vec2( uv.x    , uv.y + y ) ) * 2.0;\n\tvertEdge += texture2D(texture, vec2( uv.x + x, uv.y + y ) ) * 1.0;\n\tvec3 edge = sqrt((horizEdge.rgb * horizEdge.rgb) + (vertEdge.rgb * vertEdge.rgb));\n\n\tgl_FragColor = vec4( edge, texture2D(texture, uv ).a );\n     }\n     ",
          vert: "\n     precision mediump float;\n     attribute vec2 position;\n     varying vec2 uv;\n     void main () {\n     uv = position;\n     gl_Position = vec4(-(1.0 - 2.0 * position.x), 1.0 - 2.0 * position.y, 0, 1);\n     }\n     ",
          attributes: {
            position: /* array */[
              /* tuple */[
                -2,
                0
              ],
              /* tuple */[
                0,
                -2
              ],
              /* tuple */[
                2,
                2
              ]
            ]
          },
          uniforms: {
            resolution: regl.prop("resolution"),
            texture: regl.prop("texture")
          },
          count: 3
        };
}

function displaceSpec(regl) {
  return {
          frag: "\n     precision mediump float;\n     uniform sampler2D texture;\n     uniform sampler2D displace_map;\n     uniform float maximum;\n     uniform float time;\n     uniform vec2 resolution;\n     varying vec2 uv;\n     const float TAU = 6.28318530718;\n\n     void main () {\n\t   float x = 1.0 / resolution.x;\n\t   float y = 1.0 / resolution.y;\n     float time_e      = time * 0.1;\n     float x_t = 0.0;\n     float y_t = 0.0;\n     vec2 uv_t         = vec2(uv.s + x_t, uv.t + y_t);\n     vec4 displace     = texture2D(displace_map, uv_t);\n     vec2 uv_displaced = vec2(uv.x + (displace.r * maximum * x),\n       uv.y + (displace.b * maximum * y));\n\n     gl_FragColor = texture2D(texture, uv_displaced);\n     }\n     ",
          vert: "\n     precision mediump float;\n     attribute vec2 position;\n     varying vec2 uv;\n     void main () {\n     uv = position;\n     gl_Position = vec4(-(1.0 - 2.0 * position.x), 1.0 - 2.0 * position.y, 0, 1);\n     }\n     ",
          attributes: {
            position: /* array */[
              /* tuple */[
                -2,
                0
              ],
              /* tuple */[
                0,
                -2
              ],
              /* tuple */[
                2,
                2
              ]
            ]
          },
          uniforms: {
            resolution: regl.prop("resolution"),
            texture: regl.prop("texture"),
            displace_map: regl.prop("displace_map"),
            maximum: regl.prop("maximum"),
            time: regl.prop("time")
          },
          count: 3
        };
}

var makeDrawCommand = function (regl,spec){
     var command = regl(spec);
     command.draw = command;
     return command;
     };

function sobelOptions(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "sourceLayer",
                r[/* sourceLayer */0]
              ],
              /* [] */0
            ]);
}

function displacementOptions(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "sourceLayer",
                r[/* displacementSourceLayer */0]
              ],
              /* :: */[
                /* tuple */[
                  "displacementMap",
                  r[/* displacementMap */1]
                ],
                /* [] */0
              ]
            ]);
}

function reglOptions(param) {
  if (param.tag) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "displacement"
                ],
                /* :: */[
                  /* tuple */[
                    "opts",
                    displacementOptions(param[0])
                  ],
                  /* [] */0
                ]
              ]);
  } else {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "sobel"
                ],
                /* :: */[
                  /* tuple */[
                    "opts",
                    sobelOptions(param[0])
                  ],
                  /* [] */0
                ]
              ]);
  }
}

var EncodeReglOptions = /* module */[
  /* sobelOptions */sobelOptions,
  /* displacementOptions */displacementOptions,
  /* reglOptions */reglOptions
];

function sobelOptions$1(json) {
  return /* record */[/* sourceLayer */Json_decode.field("sourceLayer", Json_decode.string, json)];
}

function displacementOptions$1(json) {
  return /* record */[
          /* displacementSourceLayer */Json_decode.field("sourceLayer", Json_decode.string, json),
          /* displacementMap */Json_decode.field("displacementMap", Json_decode.string, json)
        ];
}

function reglOptionsByType(type_, json) {
  switch (type_) {
    case "displacement" : 
        return Json_decode.map((function (o) {
                      return /* Displacement */Block.__(1, [o]);
                    }), (function (param) {
                      return Json_decode.field("opts", displacementOptions$1, param);
                    }), json);
    case "sobel" : 
        return Json_decode.map((function (o) {
                      return /* Sobel */Block.__(0, [o]);
                    }), (function (param) {
                      return Json_decode.field("opts", sobelOptions$1, param);
                    }), json);
    default:
      return /* Sobel */Block.__(0, [/* record */[/* sourceLayer */"root"]]);
  }
}

function reglOptions$1(json) {
  return Json_decode.andThen(reglOptionsByType, (function (param) {
                return Json_decode.field("type", Json_decode.string, param);
              }), json);
}

var DecodeReglOptions = /* module */[
  /* sobelOptions */sobelOptions$1,
  /* displacementOptions */displacementOptions$1,
  /* reglOptionsByType */reglOptionsByType,
  /* reglOptions */reglOptions$1
];

export {
  reinit ,
  triangleSpec ,
  sobelSpec ,
  displaceSpec ,
  makeDrawCommand ,
  EncodeReglOptions ,
  DecodeReglOptions ,
  
}
/* Json_encode Not a pure module */
