open Audio;
open AudioGraph;
open Canvas;
open ImageDataUtil;
open RawAudio;
open TypedArray;

type state = {audioBuffer: ref(option(audioBuffer))};

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~layerKey,
      ~layerRefs,
      ~audioCtx,
      ~audioGraph,
      ~saveTick,
      ~rawAudioFormat,
      _children,
    ) => {
  ...component,
  initialState: () => {audioBuffer: ref(None)},
  reducer: ((), _state: state) => ReasonReact.NoUpdate,
  didMount: self => {
    let {x, y, w, h, encoding, sampleRate} = rawAudioFormat;
    let buffer = createBuffer(audioCtx, 1, w * h, sampleRate);
    self.state.audioBuffer := Some(buffer);

    saveTick(self.onUnmount, layerKey, _t =>
      switch (
        self.state.audioBuffer^,
        getNode("compressor", audioGraph^),
        Belt.Map.String.get(layerRefs^, "root"),
      ) {
      | (Some(buffer), Some(sink), Some(canvas)) =>
        let ctx = getContext(getFromReact(canvas));
        let imageData = Ctx.getImageData(ctx, x, y, w, h);
        switch (encoding) {
        | Float =>
          let rawImgData = toFloat32Array(dataGet(imageData));
          copyToChannel(buffer, rawImgData, 0, 0);
        | Int8(c) =>
          let floats = imageDataToFloat32Array(imageData, c);
          copyToChannel(buffer, floats, 0, 0);
        };
        let node = createBufferSource(audioCtx);
        bufferSet(node, buffer);
        connect(node, sink);
        startAudioBufferSourceNode(node);
      | _ => ()
      }
    );
  },
  render: self => <div />,
};
