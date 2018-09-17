open Canvas;
open KeyboardManager;

type state = {
  canvasRef: ref(option(Dom.element)),
  keyboardManagerState,
};

let makeCallback = ({ReasonReact.state}, width, height, e) => {
  Js.log(e);
  switch (state.canvasRef^) {
  | None => ()
  | Some(canvas) =>
    let canvasElement = getFromReact(canvas);
    let ctx = getContext(canvasElement);
    Ctx.clearRect(ctx, 0, 0, width, height);
    Ctx.setFillStyle(ctx, "white");
    let keyCodeN = keyCode(e);
    let keyCodeY = height - keyCodeN - 1;
    Ctx.fillRect(ctx, 0, keyCodeY, 1, 1);
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
        listener: ref(None),
      },
    },
    didMount: self => {
      let callback: keyboardEventCallback = makeCallback(self, width, height);

      addKeyDownListenerToBody(self.state.keyboardManagerState, callback);
      self.onUnmount(()
        /* remove */
        => removeKeyDownListenerFromBody(self.state.keyboardManagerState));
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
