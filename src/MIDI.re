type midiChannel = int;
type noteNumber = int;
type velocity = float;

type midiNoteAndVelocity = (noteNumber, velocity);

type midiChannelEvent =
  | NoteOn(midiNoteAndVelocity)
  | NoteOff(midiNoteAndVelocity)
  | ControlChange((int, float));

type midiEvent = midiChannelEvent;

type midiState = {notesOn: array(velocity)};

let defaultState = {notesOn: Array.make(128, 0.0)};

let update: (midiState, midiEvent) => unit =
  (midiState, event) =>
    switch (event) {
    | NoteOn((noteNumber, velocity)) =>
      midiState.notesOn[noteNumber] = velocity
    | NoteOff((noteNumber, _)) => midiState.notesOn[noteNumber] = 0.0
    | ControlChange(_) => ()
    };
