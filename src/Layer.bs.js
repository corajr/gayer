// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Slider$Gayer from "./Slider.bs.js";
import * as MaterialUi_Card from "@jsiebern/bs-material-ui/src/MaterialUi_Card.bs.js";
import * as MaterialUi_CardMedia from "@jsiebern/bs-material-ui/src/MaterialUi_CardMedia.bs.js";
import * as MaterialUi_Typography from "@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js";
import * as MaterialUi_CardContent from "@jsiebern/bs-material-ui/src/MaterialUi_CardContent.bs.js";

function slitscanOptions(json) {
  return /* record */[/* x */Json_decode.field("x", Json_decode.$$int, json)];
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

function slitscanOptions$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "x",
                r[/* x */0]
              ],
              /* [] */0
            ]);
}

function cameraOptions$1(r) {
  var match = r[/* slitscan */0];
  if (match !== undefined) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "slitscan",
                  slitscanOptions$1(match)
                ],
                /* [] */0
              ]);
  } else {
    return Json_encode.object_(/* [] */0);
  }
}

var EncodeCameraOptions = /* module */[
  /* slitscanOptions */slitscanOptions$1,
  /* cameraOptions */cameraOptions$1
];

function layerByType(type_, json) {
  switch (type_) {
    case "analysis" : 
        return /* Analysis */0;
    case "fill" : 
        return Json_decode.map((function (s) {
                      return /* Fill */Block.__(0, [s]);
                    }), (function (param) {
                      return Json_decode.field("style", Json_decode.string, param);
                    }), json);
    case "image" : 
        return Json_decode.map((function (s) {
                      return /* Image */Block.__(2, [s]);
                    }), (function (param) {
                      return Json_decode.field("url", Json_decode.string, param);
                    }), json);
    case "pitchClasses" : 
        return Json_decode.map((function (xs) {
                      return /* PitchClasses */Block.__(3, [Curry._1(Music$Gayer.PitchSet[/* of_list */25], xs)]);
                    }), (function (param) {
                      return Json_decode.field("pc", (function (param) {
                                    return Json_decode.list(Json_decode.$$int, param);
                                  }), param);
                    }), json);
    case "reader" : 
        return Json_decode.map((function (i) {
                      return /* Reader */Block.__(4, [i]);
                    }), (function (param) {
                      return Json_decode.map(Canvas$Gayer.channel_of_int, (function (param) {
                                    return Json_decode.field("channel", Json_decode.$$int, param);
                                  }), param);
                    }), json);
    case "webcam" : 
        return Json_decode.map((function (s) {
                      return /* Webcam */Block.__(1, [s]);
                    }), cameraOptions, json);
    default:
      throw [
            Json_decode.DecodeError,
            "Expected layer content, got " + JSON.stringify(json)
          ];
  }
}

function layerContent(json) {
  return Json_decode.andThen(layerByType, (function (param) {
                return Json_decode.field("type", Json_decode.string, param);
              }), json);
}

function layer(json) {
  return /* record */[
          /* content */Json_decode.field("content", layerContent, json),
          /* alpha */Json_decode.field("alpha", Json_decode.$$float, json),
          /* compositeOperation */Json_decode.map(Canvas$Gayer.compositeOperation_of_string, (function (param) {
                  return Json_decode.field("compositeOperation", Json_decode.string, param);
                }), json)
        ];
}

var DecodeLayer = /* module */[
  /* layerByType */layerByType,
  /* layerContent */layerContent,
  /* layer */layer
];

function layerContent$1(r) {
  if (typeof r === "number") {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "analysis"
                ],
                /* [] */0
              ]);
  } else {
    switch (r.tag | 0) {
      case 0 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "fill"
                      ],
                      /* :: */[
                        /* tuple */[
                          "style",
                          r[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 1 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "webcam"
                      ],
                      /* :: */[
                        /* tuple */[
                          "options",
                          cameraOptions$1(r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 2 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "image"
                      ],
                      /* :: */[
                        /* tuple */[
                          "url",
                          r[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 3 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "pitchClasses"
                      ],
                      /* :: */[
                        /* tuple */[
                          "pc",
                          Json_encode.list((function (prim) {
                                  return prim;
                                }), Curry._1(Music$Gayer.PitchSet[/* elements */19], r[0]))
                        ],
                        /* [] */0
                      ]
                    ]);
      case 4 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "reader"
                      ],
                      /* :: */[
                        /* tuple */[
                          "channel",
                          Canvas$Gayer.int_of_channel(r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      
    }
  }
}

function layer$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "content",
                layerContent$1(r[/* content */0])
              ],
              /* :: */[
                /* tuple */[
                  "alpha",
                  r[/* alpha */1]
                ],
                /* :: */[
                  /* tuple */[
                    "compositeOperation",
                    Canvas$Gayer.string_of_compositeOperation(r[/* compositeOperation */2])
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

var EncodeLayer = /* module */[
  /* layerContent */layerContent$1,
  /* layer */layer$1
];

function renderLayerContent(layerContent, setRef) {
  if (typeof layerContent === "number") {
    return "analysis";
  } else {
    switch (layerContent.tag | 0) {
      case 0 : 
          return "fill: " + layerContent[0];
      case 1 : 
          return React.createElement("video", {
                      ref: setRef,
                      autoPlay: true,
                      height: "120",
                      muted: true,
                      width: "120"
                    });
      case 2 : 
          return React.createElement("img", {
                      ref: setRef,
                      height: "120",
                      src: layerContent[0],
                      width: "120"
                    });
      case 3 : 
          return "pc";
      case 4 : 
          return "reader";
      
    }
  }
}

var component = ReasonReact.statelessComponent("Layer");

function make(layer, changeLayer, $staropt$star, _) {
  var setRef = $staropt$star !== undefined ? $staropt$star : (function () {
        return /* () */0;
      });
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function () {
              return React.createElement("div", undefined, ReasonReact.element(undefined, undefined, MaterialUi_Card.make(undefined, undefined, undefined, undefined, undefined, undefined, {
                                  display: "flex",
                                  justifyContent: "space-between"
                                }, /* array */[
                                  ReasonReact.element(undefined, undefined, MaterialUi_CardMedia.make(undefined, undefined, undefined, "dummy", undefined, undefined, /* array */[renderLayerContent(layer[/* content */0], setRef)])),
                                  ReasonReact.element(undefined, undefined, MaterialUi_CardContent.make(undefined, undefined, undefined, {
                                            height: "100%"
                                          }, /* array */[
                                            React.createElement("div", undefined, ReasonReact.element(undefined, undefined, MaterialUi_Typography.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */["Alpha"])), ReasonReact.element(undefined, undefined, Slider$Gayer.make(undefined, undefined, undefined, undefined, undefined, 1.0, 0.0, 0.1, layer[/* alpha */1], undefined, (function (_, value) {
                                                            return Curry._2(changeLayer, layer, /* record */[
                                                                        /* content */layer[/* content */0],
                                                                        /* alpha */value,
                                                                        /* compositeOperation */layer[/* compositeOperation */2]
                                                                      ]);
                                                          }), undefined, /* array */[]))),
                                            React.createElement("div", undefined, "Composite operation: " + Canvas$Gayer.string_of_compositeOperation(layer[/* compositeOperation */2]))
                                          ]))
                                ])));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  DecodeCameraOptions ,
  EncodeCameraOptions ,
  DecodeLayer ,
  EncodeLayer ,
  renderLayerContent ,
  component ,
  make ,
  
}
/* component Not a pure module */
