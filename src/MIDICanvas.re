open Canvas;
open MIDI;
open WebMIDI;

type action =
  | MIDIEventReceived(midiEvent);

type state = {
  canvasRef: ref(option(Dom.element)),
  midiState: ref(midiState),
};

let defaultState = () => {
  canvasRef: ref(None),
  midiState: ref(MIDI.defaultState),
};

let drawMidiNotes = (canvasRenderingContext2D, state) => {
  let outputImageData =
    makeImageDataFromFloats(state.midiState^.notesOn, 1, 128);

  Ctx.putImageData(canvasRenderingContext2D, outputImageData, 0, 0);
};

let component = ReasonReact.reducerComponent("MIDICanvas");

let make = (~saveRef, _children) => {
  let setCanvasRef = (theRef, {ReasonReact.state}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    saveRef(theRef);
  };

  {
    ...component,
    initialState: defaultState,
    didMount: self =>
      WebMIDI.enable(
        webmidi,
        () => {
          Js.log("MIDI Inputs: ");
          Array.iter(Js.log, inputs(webmidi));
          Js.log("MIDI Outputs: ");
          Array.iter(Js.log, outputs(webmidi));
          /* TODO: Select component for midi inputs */
          let input = inputs(webmidi)[1];
          addListener(input, WebMidiNoteOn, All, e =>
            self.send(MIDIEventReceived(e))
          );
          addListener(input, WebMidiNoteOff, All, e =>
            self.send(MIDIEventReceived(e))
          );
        },
      ),
    reducer: (action, state) =>
      switch (action) {
      | MIDIEventReceived(event) =>
        switch (state.canvasRef^) {
        | None => ReasonReact.NoUpdate
        | Some(canvas) =>
          ReasonReact.SideEffects(
            (
              _self => {
                MIDI.update(state.midiState^, event);
                Js.log(state.midiState^.notesOn);
                let canvasElement = getFromReact(canvas);
                let ctx = getContext(canvasElement);
                drawMidiNotes(ctx, state);
              }
            ),
          )
        }
      },
    render: self =>
      <canvas ref=(self.handle(setCanvasRef)) width="1" height="128" />,
  };
};
