type rawAudioEncoding =
  | Float
  | Int8(Canvas.channel);

type rawAudioFormat = {
  x: int,
  y: int,
  w: int,
  h: int,
  encoding: rawAudioEncoding,
  sampleRate: int,
};
