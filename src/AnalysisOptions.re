open Audio.AudioInput;
open Canvas.DrawCommand;
open ReaderType;

type historyParams = {
  w: length,
  h: length,
};

type analysisSize =
  | WithHistory(historyParams)
  | DestRect(rect);

type analysisOptions = {
  input: audioInputSetting,
  readerType,
  analysisSize,
};

let defaultAnalysisOptions = {
  input: Mic,
  readerType: Channel(R),
  analysisSize: WithHistory({w: Width, h: Height}),
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
      | "with-history" =>
        json
        |> field2(
             (w, h) => WithHistory({w, h}),
             "w",
             DecodeDrawCommand.length,
             "h",
             DecodeDrawCommand.length,
           )
      | "dest-rect" =>
        json |> map(r => DestRect(r), field("rect", DecodeDrawCommand.rect))
      | _ => WithHistory({w: Width, h: Height})
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
      | WithHistory({w, h}) =>
        object_([
          ("type", string("with-history")),
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
