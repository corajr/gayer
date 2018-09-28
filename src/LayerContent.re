open Audio;
open AudioGraph;
open Layer;

let component = ReasonReact.statelessComponent("LayerContent");

let make =
    (
      ~layerKey,
      ~audioCtx,
      ~audioGraph,
      ~layerRefs,
      ~setRef,
      ~saveTick,
      ~millisPerTick,
      ~width,
      ~height,
      ~getReadAndWritePos,
      ~globalDrawContext,
      ~currentFilterValues,
      ~layerContent,
      _children,
    ) => {
  ...component,
  render: self =>
    <div key=layerKey>
      (
        switch (layerContent) {
        | Webcam => <video ref=setRef autoPlay=true muted=true />
        | Slitscan(opts) =>
          <SlitscanCanvas
            layerKey
            layerRefs
            setRef
            saveTick
            globalDrawContext
            opts
            width
            height
          />
        | Image(url) =>
          <img
            ref=setRef
            src=url
            width="120"
            height="120"
            /* width=(Js.Int.toString(width)) */
            /* height=(Js.Int.toString(height)) */
          />
        | Video(url) => <VideoFile layerKey setRef url audioCtx audioGraph />
        | Analysis(options) =>
          open Canvas.DrawCommand;
          let (w, h) =
            switch (options.analysisSize) {
            | CircularBuffer({w, h}) => (w, h)
            | History({w, h}) => (w, h)
            | DestRect({w, h}) => (w, h)
            };

          let analysisWidth = getLength(globalDrawContext, w);
          let analysisHeight = getLength(globalDrawContext, h);
          <AnalysisCanvas
            layerKey
            width=analysisWidth
            height=analysisHeight
            audioCtx
            audioGraph
            options
            millisPerTick
            saveTick
            saveRef=setRef
          />;
        | MIDIKeyboard => <MIDICanvas setRef height />
        | KeycodeWriter(format) =>
          <KeycodeCanvas layerKey layerRefs format setRef />
        | KeycodeReader(format) =>
          <KeycodeReaderCanvas
            layerKey
            layerRefs
            format
            setRef
            saveTick
            currentFilterValues
            getReadAndWritePos
          />
        | Draw(cmds) =>
          <DrawCommandCanvas
            cmds
            layerKey
            layerRefs
            setRef
            saveTick
            width
            height
          />
        | HandDrawn => <HandDrawnCanvas setRef width height />
        | RawAudioWriter({x, y, w, h, encoding}) =>
          <RawAudioCanvas
            layerKey
            layerRefs
            audioCtx
            audioGraph
            setRef
            saveTick
            encoding
            samples=(w * h)
            width=w
            height=h
            x
            y
          />
        | Histogram =>
          <HistogramCanvas
            setRef
            layerRefs
            layerKey
            width=1
            rootHeight=height
            height=120
            getReadAndWritePos
            saveTick
          />
        | Regl(opts) =>
          <ReglCanvas setRef layerRefs opts width height saveTick layerKey />
        | RawAudioReader(rawAudioFormat) =>
          <RawAudioReader
            layerKey
            layerRefs
            saveTick
            rawAudioFormat
            audioCtx
            audioGraph
          />
        | Text(_)
        | DrawGlobal(_)
        | PitchClasses(_)
        | Fill(_)
        | Reader(_) => ReasonReact.null
        }
      )
    </div>,
};
