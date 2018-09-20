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

      let bins = 120;
      let binFn = ({r, g, b, a}) => {
        let (h, s, l) = rgbToHslFloat(r, g, b);
        let octave = int_of_float(l *. 9.0);
        let octaveHeight = 12;
        let offsetInOctave = int_of_float(h *. 11.0);
        let bin = octave * octaveHeight + offsetInOctave;
        (bin, s);
      };

      let histogram =
        imageDataToHistogram(~binCount=bins, ~binFn, ~divideBy=10.0, slice);

      /* let img = makeImageDataFromFloats(histogram, 1, height); */
      /* Ctx.putImageData(ctx, img, 0, 0); */

      let n = height / bins;
      for (i in 0 to n - 1) {
        let offset = (n - i - 1) * bins;
        for (j in 0 to bins - 1) {
          let h = float_of_int(j mod 12) /. 12.0;
          let s = histogram[j];
          /* let s = Js.Math.pow_float(histogram[j], 2.0); */
          let l = float_of_int(j / 10) /. 10.0;

          let yPos = offset + (bins - j - 1);

          let color = hsl(h, s, s > 0.05 ? l : 0.0);
          Ctx.setFillStyle(ctx, color);
          Ctx.fillRect(ctx, 0, yPos, 1, 1);
        };
      };

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
