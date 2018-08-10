// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Slider$Gayer from "./Slider.bs.js";
import * as MaterialUi_FormGroup from "@jsiebern/bs-material-ui/src/MaterialUi_FormGroup.bs.js";
import * as MaterialUi_Typography from "@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js";

var component = ReasonReact.statelessComponent("FloatSlider");

function make($staropt$star, $staropt$star$1, label, value, updater, _) {
  var min = $staropt$star !== undefined ? $staropt$star : 0.0;
  var max = $staropt$star$1 !== undefined ? $staropt$star$1 : 1.0;
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
              return ReasonReact.element(undefined, undefined, MaterialUi_FormGroup.make(undefined, true, undefined, undefined, /* array */[
                              ReasonReact.element(undefined, undefined, MaterialUi_Typography.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[label + (": " + value.toString())])),
                              ReasonReact.element(undefined, undefined, Slider$Gayer.make(undefined, undefined, undefined, undefined, undefined, max, min, undefined, value, undefined, (function (_, value) {
                                          return Curry._1(updater, value);
                                        }), undefined, /* array */[]))
                            ]));
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
