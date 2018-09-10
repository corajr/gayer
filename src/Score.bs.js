// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Params$Gayer from "./Params.bs.js";

function transition(json) {
  return Json_decode.map((function () {
                return /* Manual */0;
              }), Json_decode.string, json);
}

function scoreEvent(json) {
  return /* record */[
          /* params */Json_decode.field("params", Params$Gayer.DecodeParams[/* params */0], json),
          /* transition */Json_decode.field("transition", transition, json)
        ];
}

function scoreMetadata(json) {
  return /* record */[
          /* title */Json_decode.field("title", Json_decode.string, json),
          /* authors */Json_decode.field("authors", (function (param) {
                  return Json_decode.list(Json_decode.string, param);
                }), json)
        ];
}

function score(json) {
  return /* record */[
          /* events */Json_decode.field("events", (function (param) {
                  return Json_decode.array(scoreEvent, param);
                }), json),
          /* scoreMetadata */Json_decode.field("meta", scoreMetadata, json)
        ];
}

var DecodeScore = /* module */[
  /* transition */transition,
  /* scoreEvent */scoreEvent,
  /* scoreMetadata */scoreMetadata,
  /* score */score
];

function transition$1() {
  return "manual";
}

function scoreEvent$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "params",
                Params$Gayer.EncodeParams[/* params */0](r[/* params */0])
              ],
              /* :: */[
                /* tuple */[
                  "transition",
                  "manual"
                ],
                /* [] */0
              ]
            ]);
}

function scoreMetadata$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "title",
                r[/* title */0]
              ],
              /* :: */[
                /* tuple */[
                  "authors",
                  Json_encode.list((function (prim) {
                          return prim;
                        }), r[/* authors */1])
                ],
                /* [] */0
              ]
            ]);
}

function score$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "events",
                Json_encode.array(scoreEvent$1, r[/* events */0])
              ],
              /* :: */[
                /* tuple */[
                  "meta",
                  scoreMetadata$1(r[/* scoreMetadata */1])
                ],
                /* [] */0
              ]
            ]);
}

var EncodeScore = /* module */[
  /* transition */transition$1,
  /* scoreEvent */scoreEvent$1,
  /* scoreMetadata */scoreMetadata$1,
  /* score */score$1
];

export {
  DecodeScore ,
  EncodeScore ,
  
}
/* Json_encode Not a pure module */
