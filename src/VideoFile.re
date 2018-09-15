open Audio;
open AudioGraph;
open Video;

type state = {
  videoRef: ref(option(Dom.element)),
  audioNode: ref(option(audioNode)),
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~audioCtx, ~audioGraph, ~layerKey, ~setRef, ~url, _children) => {
  let setVideoRef = (theRef, {ReasonReact.state}) => {
    state.videoRef := Js.Nullable.toOption(theRef);
    setRef(theRef);

    switch (state.videoRef^, state.audioNode^) {
    | (Some(video), None) =>
      unmute(video);
      let node = createMediaElementSource(audioCtx, video);
      state.audioNode := Some(node);
      audioGraph := audioGraph^ |> addNode((url, node)) |> updateConnections;
    | _ => ()
    };
  };

  {
    ...component,
    initialState: () => {videoRef: ref(None), audioNode: ref(None)},
    reducer: ((), _state) => ReasonReact.NoUpdate,
    render: self =>
      <video
        ref=(self.handle(setVideoRef))
        src=url
        loop=true
        autoPlay=true
        muted=true
      />,
  };
};
