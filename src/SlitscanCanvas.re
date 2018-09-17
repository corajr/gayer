open Canvas;

type state = {canvasRef: ref(option(Dom.element))};

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~setRef,
      ~layerKey,
      ~layerRefs,
      ~sourceKey,
      ~width,
      ~height,
      ~saveTick,
      _children,
    ) => {
  let saveRef = (aRef, {ReasonReact.state}) => {
    setRef(aRef);
    let maybeRef = Js.Nullable.toOption(aRef);
    state.canvasRef := maybeRef;
  };

  let onTick = ({ReasonReact.state}, ()) =>
    switch (state.canvasRef^, Belt.Map.String.get(layerRefs^, sourceKey)) {
    | (Some(canvas), Some(sourceCanvas)) =>
      let sourceImage = getCanvasAsSource(getFromReact(sourceCanvas));
      let canvasElement = getFromReact(canvas);
      let ctx = getContext(canvasElement);

      let xToRead = 320;
      let sourceHeight = 480;

      Ctx.drawImageSourceRectDestRect(
        ctx,
        sourceImage,
        xToRead,
        0,
        1,
        sourceHeight,
        width - 1,
        0,
        1,
        height,
      );

      Ctx.drawImage(ctx, getCanvasAsSource(canvasElement), -1, 0);
    | _ => ()
    };

  {
    ...component,
    initialState: () => {canvasRef: ref(None)},
    reducer: ((), _state) => ReasonReact.NoUpdate,
    didMount: self => saveTick(self.onUnmount, layerKey, onTick(self)),
    render: self =>
      <canvas
        style=(ReactDOMRe.Style.make(~opacity="0.0", ()))
        ref=(self.handle(saveRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
