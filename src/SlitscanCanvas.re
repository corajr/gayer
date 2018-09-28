open Canvas;
open CameraOptions;

type state = {
  canvasRef: ref(option(Dom.element)),
  sourceRectReal: ref(rect),
  destRectReal: ref(rect),
  sourceXDelta: ref(int),
  sourceYDelta: ref(int),
  destXDelta: ref(int),
  destYDelta: ref(int),
};

let onTick =
    (opts, globalDrawContext, layerRefs, width, height, {ReasonReact.state}) => {
  open DrawCommand;

  /* HACK: this obviously isn't right. */
  let sourceDrawContext = {
    maybeCtxRef: ref(None),
    width: 640,
    height: 480,
    variables: Belt.Map.String.empty,
  };

  /* HACK: this obviously isn't right. */
  let destDrawContext = globalDrawContext;

  let sourceRect: Canvas.rect = {
    x: getLength(sourceDrawContext, opts.sourceRect.x),
    y: getLength(sourceDrawContext, opts.sourceRect.y),
    w: getLength(sourceDrawContext, opts.sourceRect.w),
    h: getLength(sourceDrawContext, opts.sourceRect.h),
  };

  let sourceWidth = getLength(sourceDrawContext, Width);
  let sourceHeight = getLength(sourceDrawContext, Height);

  let destRect: Canvas.rect = {
    x: getLength(destDrawContext, opts.destRect.x),
    y: getLength(destDrawContext, opts.destRect.y),
    w: getLength(destDrawContext, opts.destRect.w),
    h: getLength(destDrawContext, opts.destRect.h),
  };

  let destWidth = getLength(destDrawContext, Width);
  let destHeight = getLength(destDrawContext, Height);

  state.sourceRectReal := sourceRect;
  state.sourceXDelta := getLength(sourceDrawContext, opts.sourceXDelta);
  state.sourceYDelta := getLength(sourceDrawContext, opts.sourceYDelta);

  state.destRectReal := destRect;
  state.destXDelta := getLength(destDrawContext, opts.destXDelta);
  state.destYDelta := getLength(destDrawContext, opts.destYDelta);

  _t =>
    switch (
      state.canvasRef^,
      Belt.Map.String.get(layerRefs^, opts.sourceLayerKey),
    ) {
    | (Some(canvas), Some(sourceCanvas)) =>
      open DrawCommand;
      let sourceImage = getCanvasAsSource(getFromReact(sourceCanvas));
      let canvasElement = getFromReact(canvas);
      let ctx = getContext(canvasElement);

      let currentSourceRect = state.sourceRectReal^;
      state.sourceRectReal :=
        {
          ...currentSourceRect,
          x: wrapCoord(currentSourceRect.x, state.sourceXDelta^, sourceWidth),
          y:
            wrapCoord(currentSourceRect.y, state.sourceYDelta^, sourceHeight),
        };

      let currentDestRect = state.destRectReal^;
      state.destRectReal :=
        {
          ...currentDestRect,
          x: wrapCoord(currentDestRect.x, state.destXDelta^, destWidth),
          y: wrapCoord(currentDestRect.y, state.destYDelta^, destHeight),
        };

      Ctx.drawImageSourceRectDestRect(
        ctx,
        sourceImage,
        currentSourceRect.x,
        currentSourceRect.y,
        currentSourceRect.w,
        currentSourceRect.h,
        currentDestRect.x,
        currentDestRect.y,
        currentDestRect.w,
        currentDestRect.h,
      );
    | _ => ()
    };
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~setRef,
      ~layerKey,
      ~layerRefs,
      ~width,
      ~height,
      ~saveTick,
      ~globalDrawContext,
      ~opts,
      _children,
    ) => {
  let saveRef = (aRef, {ReasonReact.state}) => {
    setRef(aRef);
    let maybeRef = Js.Nullable.toOption(aRef);
    state.canvasRef := maybeRef;
  };

  {
    ...component,
    initialState: () => {
      canvasRef: ref(None),
      sourceRectReal: ref({x: 0, y: 0, w: 0, h: 0}),
      destRectReal: ref({x: 0, y: 0, w: 0, h: 0}),
      sourceXDelta: ref(0),
      sourceYDelta: ref(0),
      destXDelta: ref(0),
      destYDelta: ref(0),
    },
    reducer: ((), _state) => ReasonReact.NoUpdate,
    didMount: self =>
      saveTick(
        self.onUnmount,
        layerKey,
        onTick(opts, globalDrawContext, layerRefs, width, height, self),
      ),
    willUpdate: ({oldSelf, newSelf}) =>
      saveTick(
        _f => (),
        layerKey,
        onTick(opts, globalDrawContext, layerRefs, width, height, newSelf),
      ),
    render: self =>
      <canvas
        style=(ReactDOMRe.Style.make(~opacity="0.0", ()))
        ref=(self.handle(saveRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
