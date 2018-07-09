// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";

var _getStream = function (constraints){
     if (navigator.mediaDevices.getUserMedia) {
       console.log('getUserMedia supported.');
       return navigator.mediaDevices.getUserMedia(constraints);
     } else {
       console.log('getUserMedia not supported on your browser!');
       return null;
     }
     };

function getAudioVisualStream() {
  return Js_primitive.null_undefined_to_opt(_getStream({
                  audio: true,
                  video: true
                }));
}

function getAudioStream() {
  return Js_primitive.null_undefined_to_opt(_getStream({
                  audio: true,
                  video: false
                }));
}

function getVideoStream() {
  return Js_primitive.null_undefined_to_opt(_getStream({
                  audio: false,
                  video: true
                }));
}

export {
  _getStream ,
  getAudioVisualStream ,
  getAudioStream ,
  getVideoStream ,
  
}
/* No side effect */
