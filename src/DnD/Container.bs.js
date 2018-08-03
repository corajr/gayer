// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Card$Gayer from "./Card.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Dragula$Gayer from "./Dragula.bs.js";
import * as ReactDragula from "react-dragula";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";

var defaultState_000 = /* dragContainerRef */[/* None */0];

var defaultState_001 = /* dragulaRef */[/* None */0];

var defaultState = /* record */[
  defaultState_000,
  defaultState_001
];

var component = ReasonReact.reducerComponent("Container");

function make(cards, onMoveCard, onSetRef, _) {
  var dropFn = function (_, target, _$1, _$2) {
    if (target) {
      var children = target[0].childNodes;
      var ids = $$Array.map((function (elt) {
              return elt.id;
            }), children);
      var idToLayer = List.fold_left((function (acc, param) {
              return Belt_MapString.set(acc, param[/* id */0], param[/* layer */1]);
            }), Belt_MapString.empty, cards);
      var newLayers = $$Array.fold_left((function (acc, id) {
              var match = Belt_MapString.get(idToLayer, id);
              if (match) {
                return /* :: */[
                        match[0],
                        acc
                      ];
              } else {
                return acc;
              }
            }), /* [] */0, ids);
      return Curry._1(onMoveCard, List.rev(newLayers));
    } else {
      return /* () */0;
    }
  };
  var dragulaDecorator = function (theRef, param) {
    var state = param[/* state */1];
    state[/* dragContainerRef */0][0] = (theRef == null) ? /* None */0 : [theRef];
    var state$1 = state;
    var onUnmount = param[/* onUnmount */4];
    var match = state$1[/* dragulaRef */1][0];
    var match$1 = state$1[/* dragContainerRef */0][0];
    if (match || !match$1) {
      return /* () */0;
    } else {
      var drake = ReactDragula(/* array */[match$1[0]], { });
      console.log(drake);
      Dragula$Gayer.onDrop(drake, dropFn);
      Curry._1(onUnmount, (function () {
              destroy(drake);
              return /* () */0;
            }));
      state$1[/* dragulaRef */1][0] = /* Some */[drake];
      return /* () */0;
    }
  };
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
          /* render */(function (self) {
              return React.createElement("div", {
                          ref: Curry._1(self[/* handle */0], dragulaDecorator)
                        }, $$Array.of_list(List.map((function (card) {
                                    return ReasonReact.element(/* Some */[card[/* id */0]], /* None */0, Card$Gayer.make(card[/* id */0], card[/* layer */1], (function (theRef) {
                                                      return Curry._2(onSetRef, card[/* layer */1], theRef);
                                                    }), /* array */[]));
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
