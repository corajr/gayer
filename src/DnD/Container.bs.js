// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Card$Gayer from "./Card.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as RList$Rationale from "rationale/src/RList.js";
import * as ReactDndHtml5Backend from "react-dnd-html5-backend";
import * as BsReactDnd__DragDropContextProvider from "@ahrefs/bs-react-dnd/src/BsReactDnd__DragDropContextProvider.js";

var component = ReasonReact.statelessComponent("Container");

function make(cards, onMoveCard, _) {
  var handleMoveCard = function (dragId, hoverId) {
    var dragIndex = RList$Rationale.findIndex((function (card) {
            return card[/* id */0] === dragId;
          }), cards);
    var hoverIndex = RList$Rationale.findIndex((function (card) {
            return card[/* id */0] === hoverId;
          }), cards);
    if (dragIndex && hoverIndex) {
      return Curry._2(onMoveCard, dragIndex[0], hoverIndex[0]);
    } else {
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
          /* render */(function () {
              return ReasonReact.element(/* None */0, /* None */0, BsReactDnd__DragDropContextProvider.make(ReactDndHtml5Backend, /* None */0, /* array */[React.createElement("div", {
                                    style: {
                                      width: "400"
                                    }
                                  }, $$Array.of_list(List.map((function (card) {
                                              return ReasonReact.element(/* Some */[String(card[/* id */0])], /* None */0, Card$Gayer.make(card[/* id */0], card[/* text */1], handleMoveCard, /* array */[]));
                                            }), cards)))]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var RList = 0;

export {
  RList ,
  component ,
  make ,
  
}
/* component Not a pure module */
