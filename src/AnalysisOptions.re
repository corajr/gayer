open Audio.AudioInput;
open Canvas.DrawCommand;

type analysisOptions = {
  input: audioInputSetting,
  destRect: rect,
};

let defaultAnalysisOptions = {
  input: Mic,
  destRect: {
    x: Add(Width, Pixels(-1)),
    y: Pixels(0),
    w: Pixels(1),
    h: Height,
  },
};

module DecodeAnalysisOptions = {
  let analysisOptions = json =>
    Json.Decode.{
      input: json |> field("input", DecodeAudioInput.audioInputSetting),
      destRect: json |> field("destRect", DecodeDrawCommand.rect),
    };
};

module EncodeAnalysisOptions = {
  let analysisOptions = r =>
    Json.Encode.(
      object_([
        ("input", EncodeAudioInput.audioInputSetting(r.input)),
        ("destRect", EncodeDrawCommand.rect(r.destRect)),
      ])
    );
};
