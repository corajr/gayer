open Audio.AudioInput;
open Canvas.DrawCommand;
open ReaderType;

type analysisOptions = {
  input: audioInputSetting,
  readerType,
  keepHistory: bool,
  destRect: rect,
};

let defaultAnalysisOptions = {
  input: Mic,
  keepHistory: false,
  readerType: Channel(R),
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
      readerType: json |> field("readerType", DecodeReaderType.readerType),
      keepHistory: json |> field("keepHistory", bool),
      destRect: json |> field("destRect", DecodeDrawCommand.rect),
    };
};

module EncodeAnalysisOptions = {
  let analysisOptions = r =>
    Json.Encode.(
      object_([
        ("input", EncodeAudioInput.audioInputSetting(r.input)),
        ("readerType", EncodeReaderType.readerType(r.readerType)),
        ("keepHistory", bool(r.keepHistory)),
        ("destRect", EncodeDrawCommand.rect(r.destRect)),
      ])
    );
};
