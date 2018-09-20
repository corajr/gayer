open Canvas;
open MIDI;
open ImageDataUtil;
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

type noteNumberToColorMapping = noteNumber => color;

let oneRainbow: noteNumberToColorMapping =
  noteNumber =>
    "hsl("
    ++ string_of_float(float_of_int(noteNumber) *. (300.0 /. 127.0))
    ++ "deg,100%,50%)";

let makeNoteColors: noteNumberToColorMapping => list(DrawCommand.command) =
  getFillStyleForNumber => {
    open DrawCommand;
    let noteDrawCommands = ref([]);
    for (i in 127 downto 0) {
      noteDrawCommands :=
        [
          FillRect({
            x: Pixels(0),
            y: Pixels(i),
            w: Pixels(1),
            h: Pixels(1),
          }),
          SetFillStyle(getFillStyleForNumber(i)),
          ...noteDrawCommands^,
        ];
    };
    noteDrawCommands^;
  };

let drawMidiNotesImg = (canvasRenderingContext2D, state) => {
  let outputImageData =
    makeImageDataFromFloats(state.midiState^.notesOn, 1, 128);

  Ctx.putImageData(canvasRenderingContext2D, outputImageData, 0, 0);
};

let drawMidiNotes = (ctx, height, noteToY, state) => {
  let notesOn = state.midiState^.notesOn;
  for (i in 127 downto 0) {
    let v = notesOn[i];
    if (v > 0.0) {
      Ctx.setFillStyle(
        ctx,
        "rgba(255,255,255," ++ Js.Float.toString(v) ++ ")",
      );
      Ctx.fillRect(ctx, 0, noteToY(i), 1, 1);
    };
  };
};

let component = ReasonReact.reducerComponent("MIDICanvas");

let make = (~height, ~saveRef, _children) => {
  let setCanvasRef = (theRef, {ReasonReact.state}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    saveRef(theRef);
  };

  let noteToY = note => {
    let pixelsPerNote = height / 120;
    (124 - note) * pixelsPerNote;
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
          addListener(input, WebMidiEventType.NoteOn, All, e =>
            self.send(MIDIEventReceived(e))
          );
          addListener(input, WebMidiEventType.NoteOff, All, e =>
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
                let canvasElement = getFromReact(canvas);
                let ctx = getContext(canvasElement);
                Ctx.clearRect(ctx, 0, 0, 1, height);
                drawMidiNotes(ctx, height, noteToY, state);
              }
            ),
          )
        }
      },
    render: self =>
      <canvas
        ref=(self.handle(setCanvasRef))
        width="1"
        height=(Js.Int.toString(height))
      />,
  };
};
