// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Dragula$Gayer from "./Dragula.bs.js";
import * as ReactDragula from "react-dragula";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";
import * as RList$Rationale from "rationale/src/RList.js";
import * as LayerControl$Gayer from "../LayerControl.bs.js";

var defaultState_000 = /* cards : record */[/* contents : [] */0];

var defaultState_001 = /* dragContainerRef : record */[/* contents */undefined];

var defaultState_002 = /* dragulaRef : record */[/* contents */undefined];

var defaultState = /* record */[
  defaultState_000,
  defaultState_001,
  defaultState_002
];

var component = ReasonReact.reducerComponent("Container");

function make(cards, onMoveCard, onChangeLayer, onSetRef, layerKeys, layerRefs, rootWidth, rootHeight, saveTick, savedImages, _) {
  var dragulaDecorator = function (theRef, param) {
    var state = param[/* state */1];
    state[/* dragContainerRef */1][0] = (theRef == null) ? undefined : Js_primitive.some(theRef);
    var state$1 = state;
    var onUnmount = param[/* onUnmount */4];
    var match = state$1[/* dragulaRef */2][0];
    var match$1 = state$1[/* dragContainerRef */1][0];
    if (match !== undefined || match$1 === undefined) {
      return /* () */0;
    } else {
      var drake = ReactDragula(/* array */[Js_primitive.valFromOption(match$1)], {
            invalid: (function (_, handle) {
                if (handle.tagName === "BUTTON" || handle.tagName === "CANVAS") {
                  return true;
                } else {
                  return handle.getAttribute("role") === "slider";
                }
              })
          });
      Dragula$Gayer.onDrop(drake, (function (param, param$1, param$2, param$3) {
              var state$2 = state$1;
              var el = param;
              var sibling = param$3;
              var ids = List.map((function (param) {
                      return param[/* id */0];
                    }), state$2[/* cards */0][0]);
              if (el !== undefined) {
                var elId = Js_primitive.valFromOption(el).id;
                var match = RList$Rationale.indexOf(elId, ids);
                if (match !== undefined) {
                  var listMinusEl = RList$Rationale.remove(match, 1, ids);
                  var siblingIndex;
                  if (sibling !== undefined) {
                    var sibId = Js_primitive.valFromOption(sibling).id;
                    siblingIndex = RList$Rationale.indexOf(sibId, listMinusEl);
                  } else {
                    siblingIndex = List.length(listMinusEl);
                  }
                  if (siblingIndex !== undefined) {
                    var state$3 = state$2;
                    var ids$1 = RList$Rationale.insert(siblingIndex, elId, listMinusEl);
                    var idToLayer = List.fold_left((function (acc, param) {
                            return Belt_MapString.set(acc, param[/* id */0], param[/* layer */1]);
                          }), Belt_MapString.empty, state$3[/* cards */0][0]);
                    var newLayers = List.fold_left((function (acc, id) {
                            var match = Belt_MapString.get(idToLayer, id);
                            if (match !== undefined) {
                              return /* :: */[
                                      match,
                                      acc
                                    ];
                            } else {
                              return acc;
                            }
                          }), /* [] */0, ids$1);
                    return Curry._1(onMoveCard, List.rev(newLayers));
                  } else {
                    return /* () */0;
                  }
                } else {
                  return /* () */0;
                }
              } else {
                return /* () */0;
              }
            }));
      Curry._1(onUnmount, (function () {
              try {
                destroy(drake);
                return /* () */0;
              }
              catch (e){
                return /* () */0;
              }
            }));
      state$1[/* dragulaRef */2][0] = Js_primitive.some(drake);
      return /* () */0;
    }
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */(function (self) {
              self[/* state */1][/* cards */0][0] = cards;
              return self[/* state */1];
            }),
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("div", {
                          ref: Curry._1(self[/* handle */0], dragulaDecorator)
                        }, $$Array.of_list(List.map((function (card) {
                                    return React.createElement("div", {
                                                key: card[/* id */0],
                                                id: card[/* id */0],
                                                style: {
                                                  marginBottom: "16px"
                                                }
                                              }, ReasonReact.element(undefined, undefined, LayerControl$Gayer.make(card[/* layer */1], layerKeys, layerRefs, onSetRef, saveTick, onChangeLayer, savedImages, rootWidth, rootHeight, /* array */[])));
                                  }), cards)));
            }),
          /* initialState */(function () {
              return defaultState;
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (_, _$1) {
              return /* NoUpdate */0;
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var RList = 0;

export {
  RList ,
  defaultState ,
  component ,
  make ,
  
}
/* component Not a pure module */
