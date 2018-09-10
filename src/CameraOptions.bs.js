// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";

function slitscanOptions(json) {
  return Json_decode.andThen((function (type_, json) {
                switch (type_) {
                  case "readPosX" : 
                      return /* ReadPosX */0;
                  case "readPosY" : 
                      return /* ReadPosY */1;
                  case "staticX" : 
                      return Json_decode.map((function (i) {
                                    return /* StaticX */Block.__(0, [i]);
                                  }), (function (param) {
                                    return Json_decode.field("x", Json_decode.$$int, param);
                                  }), json);
                  case "staticY" : 
                      return Json_decode.map((function (i) {
                                    return /* StaticY */Block.__(1, [i]);
                                  }), (function (param) {
                                    return Json_decode.field("y", Json_decode.$$int, param);
                                  }), json);
                  default:
                    return /* StaticX */Block.__(0, [320]);
                }
              }), (function (param) {
                return Json_decode.field("type", Json_decode.string, param);
              }), json);
}

function cameraOptions(json) {
  return /* record */[/* slitscan */Json_decode.optional((function (param) {
                  return Json_decode.field("slitscan", slitscanOptions, param);
                }), json)];
}

var DecodeCameraOptions = /* module */[
  /* slitscanOptions */slitscanOptions,
  /* cameraOptions */cameraOptions
];

function slitscanOptions$1(param) {
  if (typeof param === "number") {
    if (param === 0) {
      return Json_encode.object_(/* :: */[
                  /* tuple */[
                    "type",
                    "readPosX"
                  ],
                  /* [] */0
                ]);
    } else {
      return Json_encode.object_(/* :: */[
                  /* tuple */[
                    "type",
                    "readPosY"
                  ],
                  /* [] */0
                ]);
    }
  } else if (param.tag) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "staticY"
                ],
                /* :: */[
                  /* tuple */[
                    "y",
                    param[0]
                  ],
                  /* [] */0
                ]
              ]);
  } else {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "staticX"
                ],
                /* :: */[
                  /* tuple */[
                    "x",
                    param[0]
                  ],
                  /* [] */0
                ]
              ]);
  }
}

function cameraOptions$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "slitscan",
                Json_encode.nullable(slitscanOptions$1, r[/* slitscan */0])
              ],
              /* [] */0
            ]);
}

var EncodeCameraOptions = /* module */[
  /* slitscanOptions */slitscanOptions$1,
  /* cameraOptions */cameraOptions$1
];

export {
  DecodeCameraOptions ,
  EncodeCameraOptions ,
  
}
/* Json_encode Not a pure module */
