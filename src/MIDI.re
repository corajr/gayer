type noteNumber = int;
type velocity = float;

type action =
  | NoteOn(noteNumber, velocity)
  | NoteOff(noteNumber, velocity);

type state = {notesOn: array(velocity)};

let defaultState = {notesOn: Array.make(128, 0.0)};

let update: (state, action) => state =
  (state, event) =>
    switch (event) {
    | NoteOn(noteNumber, velocity) =>
      state.notesOn[noteNumber] = velocity;
      state;
    | NoteOff(noteNumber, _) =>
      state.notesOn[noteNumber] = 0.0;
      state;
    };
