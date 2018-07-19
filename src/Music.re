type pitch = int;

module PitchSet =
  Set.Make({
    type t = pitch;
    let compare = compare;
  });

let filterByPitchSet =
    (~pitchClasses: PitchSet.t, ~filterValues: array(float)) =>
  Array.mapi(
    (i, v) => PitchSet.mem(i mod 12, pitchClasses) ? v : 0.0,
    filterValues,
  );

/* Chords */

/* Confusingly {0,4,7} sounds minor and {0, 3, 7} sounds major... */
let majorChord = PitchSet.of_list([0, 4, 7]);
let minorChord = PitchSet.of_list([0, 3, 7]);
let major7 = PitchSet.of_list([0, 4, 7, 11]);
let minor7 = PitchSet.of_list([0, 4, 7, 10]);

/* Scales */
let cMajor = PitchSet.of_list([0, 2, 4, 5, 7, 9, 11]);
let cMinor = PitchSet.of_list([0, 2, 3, 5, 7, 8, 10]);
let pentatonic = PitchSet.of_list([0, 2, 5, 7, 9]);
let wholetone = PitchSet.of_list([0, 2, 4, 6, 8, 10]);
let allPitches = PitchSet.of_list([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
