open Canvas;
open KeyboardManager;

type state = {
  canvasRef: ref(option(Dom.element)),
  keyboardManagerState,
};

let keyCodeToY = (height, keyCodeN) => height - (keyCodeN - 8) * 2 - 1;

let makeKeyDownCallback = ({ReasonReact.state}, width, height, e) => {
  switch (key(e)) {
  | "Ctrl"
  | "Escape"
  | _ =>
    ();
    KeyboardEventRe.preventDefault(e);
  };
  switch (state.canvasRef^) {
  | None => ()
  | Some(canvas) =>
    let canvasElement = getFromReact(canvas);
    let ctx = getContext(canvasElement);
    Ctx.setFillStyle(ctx, "white");
    let keyCodeN = keyCode(e);
    if (keyCodeN === 32) {
      Ctx.clearRect(ctx, 0, 0, width, height);
    };
    let keyCodeY = keyCodeToY(height, keyCodeN);
    Ctx.fillRect(ctx, 0, keyCodeY, 1, 1);
  };
};

let makeKeyUpCallback = ({ReasonReact.state}, width, height, e) => {
  KeyboardEventRe.preventDefault(e);
  switch (state.canvasRef^) {
  | None => ()
  | Some(canvas) =>
    let canvasElement = getFromReact(canvas);
    let ctx = getContext(canvasElement);
    let keyCodeY = keyCodeToY(height, keyCode(e));
    Ctx.clearRect(ctx, 0, keyCodeY, width, 1);
  };
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~layerKey, ~layerRefs, ~setRef, ~width=1, ~height=240, _children) => {
  let setCanvasRef = (theRef, {ReasonReact.state}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    setRef(theRef);
  };

  {
    ...component,
    initialState: () => {
      canvasRef: ref(None),
      keyboardManagerState: {
        keyDownListener: ref(None),
        keyUpListener: ref(None),
      },
    },
    didMount: self => {
      let keyDownCallback: keyboardEventCallback =
        makeKeyDownCallback(self, width, height);

      let keyUpCallback: keyboardEventCallback =
        makeKeyUpCallback(self, width, height);

      addKeyListenersToBody(
        self.state.keyboardManagerState,
        ~keyDownListener=keyDownCallback,
        ~keyUpListener=keyUpCallback,
      );
      self.onUnmount(()
        /* remove */
        => removeKeyListenersFromBody(self.state.keyboardManagerState));
    },
    reducer: ((), _state: state) => ReasonReact.NoUpdate,
    render: self =>
      <canvas
        ref=(self.handle(setCanvasRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
