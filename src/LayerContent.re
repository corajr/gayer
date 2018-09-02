open Audio;
open AudioGraph;
open Layer;

let component = ReasonReact.statelessComponent("LayerContent");

let make =
    (
      ~layerKey,
      ~audioCtx,
      ~audioGraph,
      ~setRef,
      ~saveTick,
      ~millisPerTick,
      ~width,
      ~height,
      ~layerContent,
      _children,
    ) => {
  ...component,
  render: self =>
    <div key=layerKey>
      (
        switch (layerContent) {
        | Webcam(_) =>
          <video
            ref=setRef
            autoPlay=true
            muted=true
            width="120"
            height="120"
            /* width=(Js.Int.toString(width)) */
            /* height=(Js.Int.toString(height)) */
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
        | Video(url) =>
          <video
            ref=setRef
            src=url
            width="120"
            height="120"
            autoPlay=true
            loop=true
            muted=true
          />
        | Analysis(source) =>
          <AnalysisCanvas
            layerKey
            size=height
            audioCtx
            audioGraph
            input=source
            millisPerTick
            saveRef=setRef
          />
        | MIDIKeyboard => <MIDICanvas saveRef=setRef />
        | Draw(_)
        | PitchClasses(_)
        | Fill(_)
        | Reader(_) => ReasonReact.null
        }
      )
    </div>,
};
