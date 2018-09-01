open Audio;
open Canvas;
open Layer;

type state = {
  audioNodes: ref(Belt.Map.String.t(audioNode)),
  canvasRefs: ref(Belt.Map.String.t(canvasImageSource)),
};

let defaultState = {
  audioNodes: ref(Belt.Map.String.empty),
  canvasRefs: ref(Belt.Map.String.empty),
};

let renderLayerContent =
    (
      key,
      getAudio,
      setRef,
      setTick,
      millisPerTick,
      width,
      height,
      layerContent,
    ) =>
  <div key>
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
        let (audioCtx, input) = getAudio(source);
        <AnalysisCanvas
          size=height
          audioCtx
          input
          millisPerTick
          saveRef=setRef
          saveTick=setTick
        />;
      | MIDIKeyboard => <MIDICanvas saveRef=setRef />
      | Draw(_)
      | PitchClasses(_)
      | Fill(_)
      | Reader(_) => ReasonReact.null
      }
    )
  </div>;

let component = ReasonReact.reducerComponent("MediaProvider");

let make =
    (
      ~layers,
      ~rootWidth,
      ~rootHeight,
      ~onSetRef,
      ~getAudio,
      ~saveTick,
      ~millisPerAudioTick,
      _children,
    ) => {
  ...component,
  initialState: () => defaultState,
  reducer: ((), _state: state) => ReasonReact.NoUpdate,
  render: self =>
    <div
      style=(
        ReactDOMRe.Style.make(~visibility="hidden", ~position="absolute", ())
      )>
      (
        List.map(
          layer =>
            renderLayerContent(
              Js.Json.stringify(EncodeLayer.layerContent(layer.content)),
              getAudio,
              onSetRef(layer),
              saveTick,
              millisPerAudioTick,
              rootWidth,
              rootHeight,
              layer.content,
            ),
          layers,
        )
        |> Array.of_list
        |> ReasonReact.array
      )
    </div>,
};
