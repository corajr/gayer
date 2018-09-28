open MIDI;
open WebMIDI;

type state = {controlInput: ref(option(input))};

let defaultState = () => {controlInput: ref(None)};

let component = ReasonReact.reducerComponent("MIDICanvas");

let make = (~onChange, _children) => {
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
        self.state.controlInput := Some(input);
        addListener(
          input,
          WebMidiEventType.ControlChange,
          All,
          fun
          | ControlChange(c) => onChange(c)
          | _ => (),
        );
        self.onUnmount(() =>
          removeListener(input, WebMidiEventType.ControlChange)
        );
      },
    ),
  reducer: ((), _state) => ReasonReact.NoUpdate,
  render: self => <div />,
};
