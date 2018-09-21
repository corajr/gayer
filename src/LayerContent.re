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
            sourceKey=opts.sourceLayerKey
            setRef
            saveTick
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
        | Analysis(source) =>
          <AnalysisCanvas
            layerKey
            size=height
            audioCtx
            audioGraph
            input=source
            millisPerTick
            saveTick
            saveRef=setRef
          />
        | MIDIKeyboard => <MIDICanvas saveRef=setRef height />
        | KeycodeWriter => <KeycodeCanvas layerKey layerRefs setRef />
        | KeycodeReader =>
          <KeycodeReaderCanvas
            layerKey
            layerRefs
            setRef
            saveTick
            currentFilterValues
            getReadAndWritePos
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
        | Regl =>
          <ReglCanvas setRef layerRefs width height saveTick layerKey />
        | RawAudioReader(rawAudioFormat) =>
          <RawAudioReader
            layerKey
            layerRefs
            saveTick
            rawAudioFormat
            audioCtx
            audioGraph
          />
        | Draw(_)
        | PitchClasses(_)
        | Fill(_)
        | Reader(_) => ReasonReact.null
        }
      )
    </div>,
};
