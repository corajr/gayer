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
