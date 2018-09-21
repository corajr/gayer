// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as ReactDom from "react-dom";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as MaterialUi_Menu from "@jsiebern/bs-material-ui/src/MaterialUi_Menu.bs.js";
import * as MaterialUi_MenuItem from "@jsiebern/bs-material-ui/src/MaterialUi_MenuItem.bs.js";
import * as LayerGenerator$Gayer from "./LayerGenerator.bs.js";

var component = ReasonReact.statelessComponent("NewLayerMenu-Gayer");

function make(onAdd, open_, anchorEl, _) {
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
              var items = $$Array.map((function (param) {
                      var example = param[1];
                      var s = param[0];
                      return ReasonReact.element(s, undefined, MaterialUi_MenuItem.make(undefined, undefined, undefined, undefined, undefined, undefined, (function () {
                                        return Curry._1(onAdd, example);
                                      }), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[s]));
                    }), LayerGenerator$Gayer.allLayerTypes);
              var match = anchorEl[0];
              if (match !== undefined) {
                return ReasonReact.element(undefined, undefined, MaterialUi_Menu.make(Js_primitive.some(ReactDom.findDOMNode(Js_primitive.valFromOption(match))), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, open_, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[items]));
              } else {
                return ReasonReact.element(undefined, undefined, MaterialUi_Menu.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, open_, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[items]));
              }
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  component ,
  make ,
  
}
/* component Not a pure module */
