// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as ReasonReact from "reason-react/src/ReasonReact.js";

var attachVideoStream = function (video,stream){
  video.srcObject = stream;
  return video;
};

var unmute = function (video){video.muted = false;};

var getWidth = function (video){return video.width;};

var getHeight = function (video){return video.height;};

var component = ReasonReact.statelessComponent("Video");

function make(_, url, setAudioRef, setImageRef) {
  var setRef = function (theRef) {
    Curry._1(setImageRef, theRef);
    return Curry._1(setAudioRef, theRef);
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
              return React.createElement("video", {
                          ref: setRef,
                          autoPlay: true,
                          height: "120",
                          loop: true,
                          src: url,
                          width: "120"
                        });
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  attachVideoStream ,
  unmute ,
  getWidth ,
  getHeight ,
  component ,
  make ,
  
}
/* component Not a pure module */
