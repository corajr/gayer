open Canvas;
open Color;
open ImageDataUtil;

type state = {canvasRef: ref(option(Dom.element))};

let component = ReasonReact.reducerComponent("HistogramCanvas");

let make =
    (
      ~setRef,
      ~layerKey,
      ~layerRefs,
      ~saveTick,
      ~rootHeight,
      ~readPos,
      ~width,
      ~height,
      _children,
    ) => {
  let saveRef = (aRef, {ReasonReact.state}) => {
    setRef(aRef);
    let maybeRef = Js.Nullable.toOption(aRef);
    state.canvasRef := maybeRef;
  };

  let onTick = ({ReasonReact.state}, _t) =>
    switch (Belt.Map.String.get(layerRefs^, "root"), state.canvasRef^) {
    | (Some(rootCanvas), Some(histogramCanvas)) =>
      let rootCtx = getContext(getFromReact(rootCanvas));
      let ctx = getContext(getFromReact(histogramCanvas));
      let slice = Ctx.getImageData(rootCtx, readPos^, 0, 1, rootHeight);

      let rootOctaveHeight = rootHeight / 10;
      let bins = 120;
      let binFn = (i, {r, g, b}) => {
        let (h, s, _) = rgbToHslFloat(r, g, b);
        let octave = (rootHeight - i - 1) / rootOctaveHeight;
        let octaveHeight = 12;
        let offsetInOctave = int_of_float(h *. 11.0);
        let bin = octave * octaveHeight + offsetInOctave;
        (bin, s);
      };

      let histogram =
        imageDataToHistogram(~binCount=bins, ~binFn, ~divideBy=10.0, slice);

      let img = makeImageDataFromFloats(histogram, 1, height);
      Ctx.putImageData(ctx, img, 0, 0);

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
        width="1"
        height=(Js.Int.toString(height))
      />,
  };
};
