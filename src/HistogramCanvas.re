open Canvas;
open Color;

type state = {canvasRef: ref(option(Dom.element))};

let component = ReasonReact.reducerComponent("HistogramCanvas");

let make =
    (
      ~setRef,
      ~layerKey,
      ~layerRefs,
      ~saveTick,
      ~rootHeight,
      ~getReadAndWritePos,
      ~width,
      ~height,
      _children,
    ) => {
  let saveRef = (aRef, {ReasonReact.state}) => {
    setRef(aRef);
    let maybeRef = Js.Nullable.toOption(aRef);
    state.canvasRef := maybeRef;
  };

  let onTick = ({ReasonReact.state}, ()) =>
    switch (Belt.Map.String.get(layerRefs^, "root"), state.canvasRef^) {
    | (Some(rootCanvas), Some(histogramCanvas)) =>
      let rootCtx = getContext(getFromReact(rootCanvas));
      let ctx = getContext(getFromReact(histogramCanvas));
      let xToRead = ref(0);
      getReadAndWritePos((toRead, _) => xToRead := toRead);
      let slice = Ctx.getImageData(rootCtx, xToRead^, 0, 1, rootHeight);

      let bins = 12;
      let binFn = ({r, g, b, a}) => {
        let (h, s, v) = rgbToHsvFloat(r, g, b);
        let bin = int_of_float(h *. 11.0);
        (bin, s);
      };

      let histogram = imageDataToHistogram(bins, binFn, slice);
      let n = height / bins;
      for (i in 0 to n - 1) {
        let offset = i * bins;
        for (j in 0 to bins - 1) {
          let color =
            Js.Int.toStringWithRadix(
              int_of_float(255.0 *. histogram[j]),
              16,
            );
          Ctx.setFillStyle(ctx, "#" ++ color ++ color ++ color);
          Ctx.fillRect(ctx, 0, offset + j, 1, 1);
        };
      };

    | _ => ()
    };

  {
    ...component,
    initialState: () => {canvasRef: ref(None)},
    reducer: ((), _state) => ReasonReact.NoUpdate,
    didMount: self => saveTick(layerKey, onTick(self)),
    render: self =>
      <canvas
        style=(ReactDOMRe.Style.make(~opacity="0.0", ()))
        ref=(self.handle(saveRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
