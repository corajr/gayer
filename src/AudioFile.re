open Audio;
open AudioGraph;

type state = {
  audioRef: ref(option(Dom.element)),
  audioNode: ref(option(audioNode)),
};

let component = ReasonReact.reducerComponent("AudioFile");

let make = (~audioCtx, ~audioGraph, ~audioKey, ~url, _children) => {
  let setAudioRef = (theRef, {ReasonReact.state}) => {
    state.audioRef := Js.Nullable.toOption(theRef);

    switch (state.audioRef^, state.audioNode^) {
    | (Some(audio), None) =>
      let node = createMediaElementSource(audioCtx, audio);
      state.audioNode := Some(node);
      audioGraph :=
        audioGraph^ |> addNode((audioKey, node)) |> updateConnections;
    | _ => ()
    };
  };

  {
    ...component,
    initialState: () => {audioRef: ref(None), audioNode: ref(None)},
    reducer: ((), _state) => ReasonReact.NoUpdate,
    didMount: self =>
      self.onUnmount(() =>
        audioGraph :=
          audioGraph^
          |> removeNode(audioKey)
          |> removeAllEdgesInvolvingNode(audioKey)
          |> updateConnections
      ),
    render: self =>
      <audio
        ref=(self.handle(setAudioRef))
        src=url
        loop=true
        autoPlay=true
        controls=true
      />,
  };
};
