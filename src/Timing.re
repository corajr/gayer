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

let maybeClearTimer = (timerRef: ref(option(Js.Global.intervalId))) =>
  switch (timerRef^) {
  | None => ()
  | Some(id) => Js.Global.clearInterval(id)
  };

let setTimer =
    (
      timerRef: ref(option(Js.Global.intervalId)),
      callback: unit => unit,
      millisPerTick: int,
    ) => {
  maybeClearTimer(timerRef);
  timerRef := Some(Js.Global.setInterval(callback, millisPerTick));
};
