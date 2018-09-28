open Audio.AudioInput;
open DrawCommand;
open ReaderType;

type historyParams = {
  w: length,
  h: length,
};

type analysisSize =
  | Slit
  | CircularBuffer(historyParams)
  | History(historyParams)
  | DestRect(rect);

let string_of_analysisSize =
  fun
  | Slit => "slit"
  | CircularBuffer(_) => "circular buffer"
  | History(_) => "history"
  | DestRect(_) => "";

let analysisSize_of_string =
  fun
  | "slit" => Slit
  | "circular buffer" => CircularBuffer({w: Width, h: Height})
  | _ => History({w: Width, h: Height});

type analysisOptions = {
  input: audioInputSetting,
  readerType,
  analysisSize,
};

let defaultAnalysisOptions = {
  input: Mic,
  readerType: Channel(R),
  analysisSize: Slit,
};

let destRect =
  DestRect({
    x: Add(Width, Pixels(-1)),
    y: Pixels(0),
    w: Pixels(1),
    h: Height,
  });

module DecodeAnalysisOptions = {
  let analysisSizeByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "slit" => Slit
      | "history" =>
        json
        |> field2(
             (w, h) => History({w, h}),
             "w",
             DecodeDrawCommand.length,
             "h",
             DecodeDrawCommand.length,
           )
      | "circular-buffer" =>
        json
        |> field2(
             (w, h) => CircularBuffer({w, h}),
             "w",
             DecodeDrawCommand.length,
             "h",
             DecodeDrawCommand.length,
           )
      | "dest-rect" =>
        json |> map(r => DestRect(r), field("rect", DecodeDrawCommand.rect))
      | _ => History({w: Width, h: Height})
      }
    );

  let analysisSize = json =>
    Json.Decode.(
      json |> (field("type", string) |> andThen(analysisSizeByType))
    );

  let analysisOptions = json =>
    Json.Decode.{
      input: json |> field("input", DecodeAudioInput.audioInputSetting),
      readerType: json |> field("readerType", DecodeReaderType.readerType),
      analysisSize: json |> field("analysisSize", analysisSize),
    };
};

module EncodeAnalysisOptions = {
  let analysisSize =
    Json.Encode.(
      fun
      | Slit => object_([("type", string("slit"))])
      | CircularBuffer({w, h}) =>
        object_([
          ("type", string("circular-buffer")),
          ("w", EncodeDrawCommand.length(w)),
          ("h", EncodeDrawCommand.length(h)),
        ])
      | History({w, h}) =>
        object_([
          ("type", string("history")),
          ("w", EncodeDrawCommand.length(w)),
          ("h", EncodeDrawCommand.length(h)),
        ])
      | DestRect(rect) =>
        object_([
          ("type", string("dest-rect")),
          ("rect", EncodeDrawCommand.rect(rect)),
        ])
    );

  let analysisOptions = r =>
    Json.Encode.(
      object_([
        ("input", EncodeAudioInput.audioInputSetting(r.input)),
        ("readerType", EncodeReaderType.readerType(r.readerType)),
        ("analysisSize", analysisSize(r.analysisSize)),
      ])
    );
};
