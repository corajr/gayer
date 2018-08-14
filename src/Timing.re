type domHighResTimeStamp = float;

[@bs.val] external window : Dom.window = "window";

[@bs.send]
external requestAnimationFrame :
  (Dom.window, domHighResTimeStamp => unit) => unit =
  "";

let rec callWithRequestAnimationFrame = (f, timestamp) => {
  f(timestamp);
  requestAnimationFrame(window, callWithRequestAnimationFrame(f));
};
