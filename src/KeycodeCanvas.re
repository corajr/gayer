open BitwiseOps;
open Canvas;
open KeyboardManager;
open KeycodeUtil;

type state = {
  canvasRef: ref(option(Dom.element)),
  keyboardManagerState,
};

let makeKeyDownCallback = ({ReasonReact.state}, format, width, height, e) => {
  switch (key(e)) {
  | "Space" => KeyboardEventRe.preventDefault(e)
  | _ => ()
  };

  let keyCodeN = keyCode(e);

  switch (state.canvasRef^) {
  | None => ()
  | Some(canvas) =>
    let canvasElement = getFromReact(canvas);
    let ctx = getContext(canvasElement);

    switch (format) {
    | AsciiAsHeight =>
      Ctx.setFillStyle(ctx, "white");
      if (keyCodeN === 32) {
        Ctx.clearRect(ctx, 0, 0, width, height);
      };
      let keyCodeY = keyCodeToY(height, keyCodeN);
      Ctx.fillRect(ctx, 0, keyCodeY, 1, 1);
    | PitchFilter =>
      Ctx.clearRect(ctx, 0, 0, width, height);
      Ctx.setFillStyle(ctx, "black");
      let octaveHeight = height / 10;
      let pixelsPerSemitone = octaveHeight / 12;
      let bits = readAllBitsOfInt(binaryToGray(keyCodeN));
      for (i in 0 to height / 10) {
        let offset = i * octaveHeight;
        Array.iteri(
          (j, b) =>
            if (! b) {
              Ctx.fillRect(
                ctx,
                0,
                offset + j * pixelsPerSemitone,
                1,
                pixelsPerSemitone,
              );
            },
          bits,
        );
      };
    };
  };
};

let makeKeyUpCallback = ({ReasonReact.state}, format, width, height, e) => {
  KeyboardEventRe.preventDefault(e);
  switch (state.canvasRef^) {
  | None => ()
  | Some(canvas) =>
    let canvasElement = getFromReact(canvas);
    let ctx = getContext(canvasElement);
    switch (format) {
    | AsciiAsHeight =>
      let keyCodeY = keyCodeToY(height, keyCode(e));
      Ctx.clearRect(ctx, 0, keyCodeY, width, 1);
    | PitchFilter => Ctx.clearRect(ctx, 0, 0, width, 1)
    };
  };
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~layerKey,
      ~layerRefs,
      ~format,
      ~setRef,
      ~width=1,
      ~height=240,
      _children,
    ) => {
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
        makeKeyDownCallback(self, format, width, height);

      let keyUpCallback: keyboardEventCallback =
        makeKeyUpCallback(self, format, width, height);

      addKeyListenersToBody(
        self.state.keyboardManagerState,
        ~keyDownListener=keyDownCallback,
        ~keyUpListener=keyUpCallback,
      );
      self.onUnmount(()
        /* remove */
        => removeKeyListenersFromBody(self.state.keyboardManagerState));
    },
    willUpdate: ({oldSelf, newSelf}) => {
      removeKeyListenersFromBody(oldSelf.state.keyboardManagerState);
      let keyDownCallback: keyboardEventCallback =
        makeKeyDownCallback(newSelf, format, width, height);

      let keyUpCallback: keyboardEventCallback =
        makeKeyUpCallback(newSelf, format, width, height);

      addKeyListenersToBody(
        newSelf.state.keyboardManagerState,
        ~keyDownListener=keyDownCallback,
        ~keyUpListener=keyUpCallback,
      );
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
