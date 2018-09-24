open MIDI;

type t;

[@bs.deriving abstract]
type webMidiNote =
  pri {
    name: string,
    octave: int,
    number: noteNumber,
  };

[@bs.deriving abstract]
type webMidiEvent =
  pri {
    channel: midiChannel,
    data: array(int), /* Uint8Array */
    note: webMidiNote,
    rawVelocity: int,
    timestamp: float,
    [@bs.as "type"]
    webMidiEventType: string,
    velocity: float,
  };

module WebMidiEventType = {
  type t =
    | NoteOn
    | NoteOff
    | PitchBend;

  let eventType_of_string: string => t =
    fun
    | "noteon" => NoteOn
    | "noteoff" => NoteOff
    | "pitchbend" => PitchBend
    | _ => NoteOff;

  let string_of_eventType: t => string =
    fun
    | NoteOn => "noteon"
    | NoteOff => "noteoff"
    | PitchBend => "pitchbend";
};

[@bs.deriving abstract]
type inputOrOutput =
  pri {
    connection: string,
    id: string,
    manufacturer: string,
    name: string,
    state: string,
    [@bs.as "type"]
    ioType_: string,
  };

type input = inputOrOutput;
type output = inputOrOutput;

[@bs.module] external webmidi : t = "webmidi";

[@bs.send] external enable : (t, unit => unit) => unit = "";

[@bs.get] external inputs : t => array(input) = "";
[@bs.get] external outputs : t => array(output) = "";

[@bs.send] external getInputByName : (t, string) => input = "";
[@bs.send] external getInputById : (t, string) => input = "";
[@bs.send] external getOutputByName : (t, string) => output = "";
[@bs.send] external getOutputById : (t, string) => output = "";

type webMidiChannel =
  | MidiChannel(midiChannel)
  | All;

let webMidiChannelToJs =
  fun
  | MidiChannel(channel) => `Int(channel)
  | All => `Str("all");

let midiEvent_of_webMidiEvent: webMidiEvent => midiEvent =
  webMidiEvent => {
    let eventType =
      WebMidiEventType.eventType_of_string(
        webMidiEvent |. webMidiEventTypeGet,
      );
    let noteNumber = webMidiEvent |. noteGet |. numberGet;
    let velocity = webMidiEvent |. velocityGet;
    switch (eventType) {
    | WebMidiEventType.NoteOn => NoteOn((noteNumber, velocity))
    | WebMidiEventType.NoteOff => NoteOff((noteNumber, 0.0))
    | WebMidiEventType.PitchBend => NoteOff((noteNumber, 0.0))
    };
  };

[@bs.send]
external _addListener :
  (
    inputOrOutput,
    string,
    [@bs.unwrap] [ | `Str(string) | `Int(int)],
    webMidiEvent => unit
  ) =>
  unit =
  "addListener";

let addListener =
    (
      ~inputOrOutput,
      ~eventType: WebMidiEventType.t,
      ~channel: webMidiChannel,
      ~callback: midiEvent => unit,
    ) =>
  _addListener(
    inputOrOutput,
    WebMidiEventType.string_of_eventType(eventType),
    webMidiChannelToJs(channel),
    webMidiEvent => {
      /* Js.log(webMidiEvent); */
      let midiEvent = midiEvent_of_webMidiEvent(webMidiEvent);
      callback(midiEvent);
    },
  );

[@bs.send]
external _removeListener : (inputOrOutput, string) => unit = "removeListener";

let removeListener = (~inputOrOutput, ~eventType: WebMidiEventType.t) =>
  _removeListener(
    inputOrOutput,
    WebMidiEventType.string_of_eventType(eventType),
  );

let onWebMidiStart: t => unit = [%bs.raw
  webMIDIre => {|
var WebMIDI = webMIDIre;
WebMidi.enable(function () {

    // Viewing available inputs and outputs
    console.log(WebMidi.inputs);
    console.log(WebMidi.outputs);

    // Retrieve an input by name, id or index
    var input = WebMidi.getInputByName("My Awesome Keyboard");
    // OR...
    // input = WebMidi.getInputById("1809568182");
    input = WebMidi.inputs[0];

    // Listen for a 'note on' message on all channels
    input.addListener('noteon', 'all',
        function (e) {
            console.log("Received 'noteon' message (" + e.note.name + e.note.octave + ").");
        }
    );

    // Listen to pitch bend message on channel 3
    input.addListener('pitchbend', 3,
        function (e) {
            console.log("Received 'pitchbend' message.", e);
        }
    );

    // Listen to control change message on all channels
    input.addListener('controlchange', "all",
        function (e) {
            console.log("Received 'controlchange' message.", e);
        }
    );

    // Remove all listeners for 'noteoff' on all channels
    input.removeListener('noteoff');

    // Remove all listeners on the input
    input.removeListener();

});

|}
];
