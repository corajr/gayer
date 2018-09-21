open Canvas;

let yToKeyCode = KeyboardManager.yToKeyCode;

type state = {canvasRef: ref(option(Dom.element))};
let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~layerKey,
      ~layerRefs,
      ~setRef,
      ~saveTick,
      ~currentFilterValues,
      ~getReadAndWritePos,
      ~width=240,
      ~height=240,
      ~fontSize=12,
      _children,
    ) => {
  let setCanvasRef = (theRef, {ReasonReact.state}) => {
    let maybeCanvas = Js.Nullable.toOption(theRef);
    state.canvasRef := maybeCanvas;
    setRef(theRef);
  };

  {
    ...component,
    reducer: ((), _state: state) => ReasonReact.NoUpdate,
    initialState: () => {canvasRef: ref(None)},
    didMount: self =>
      saveTick(self.onUnmount, layerKey, () =>
        switch (currentFilterValues^, self.state.canvasRef^) {
        | (Some(Audio.Stereo(values, _)), Some(canvas))
        | (Some(Audio.Mono(values)), Some(canvas)) =>
          let writePos = ref(0);
          getReadAndWritePos((_, w) => writePos := w);
          let ctx = getContext(getFromReact(canvas));
          Ctx.setFillStyle(ctx, rgba(0, 0, 0, 0.008));
          Ctx.fillRect(ctx, 0, 0, width, height);
          Ctx.setFont(ctx, Js.Int.toString(fontSize) ++ "px monospace");
          let n = Array.length(values);
          for (i in 0 to n - 1) {
            let v = values[i];
            if (v > 0.1) {
              let keyCodeN = yToKeyCode(height, i);
              let s = String.make(1, Char.chr(keyCodeN));
              Ctx.setFillStyle(ctx, rgba(255, 255, 255, v));
              Ctx.fillText(ctx, s, writePos^, i);
            };
          };
        | _ => ()
        }
      ),
    render: self =>
      <canvas
        ref=(self.handle(setCanvasRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
