open Audio;
open AudioGraph;
open Canvas;
open Layer;

let component = ReasonReact.statelessComponent("MediaProvider");

let make =
    (
      ~layers,
      ~rootWidth,
      ~rootHeight,
      ~onSetRef,
      ~getAudio,
      ~audioGraph,
      ~audioCtx,
      ~layerRefs,
      ~saveTick,
      ~getReadAndWritePos,
      ~millisPerAudioTick,
      _children,
    ) => {
  ...component,
  render: self =>
    <div style=(ReactDOMRe.Style.make(~position="absolute", ()))>
      (
        List.map(
          layer => {
            let key =
              Js.Json.stringify(EncodeLayer.layerContent(layer.content));
            let maybeAudio =
              switch (layer.content) {
              | Analysis(source) =>
                switch (source) {
                | AudioFile(url) =>
                  <AudioFile
                    audioCtx
                    audioGraph
                    audioKey=(key ++ "input")
                    url
                  />
                | _ =>
                  let (_, maybeInput) = getAudio(source);
                  switch (maybeInput) {
                  | Some(input) =>
                    audioGraph :=
                      audioGraph^
                      |> addNode((key ++ "input", input))
                      |> updateConnections;
                    ReasonReact.null;
                  | None => ReasonReact.null
                  };
                }
              | RawAudioWriter(_) =>
                let (_, maybeInput) = getAudio(Mic);
                switch (maybeInput) {
                | Some(input) =>
                  audioGraph :=
                    audioGraph^
                    |> addNode((key ++ "input", input))
                    |> updateConnections;
                  ReasonReact.null;
                | None => ReasonReact.null
                };
              | _ => ReasonReact.null
              };

            <div
              style=(
                switch (layer.content) {
                | HandDrawn =>
                  ReactDOMRe.Style.make(
                    ~position="absolute",
                    ~zIndex="10",
                    ~border="1px solid black",
                    (),
                  )
                | _ =>
                  ReactDOMRe.Style.make(
                    ~position="absolute",
                    ~visibility="hidden",
                    (),
                  )
                }
              )>
              maybeAudio
              <LayerContent
                audioCtx
                audioGraph
                layerKey=key
                layerRefs
                setRef=(onSetRef(layer))
                saveTick
                millisPerTick=millisPerAudioTick
                width=rootWidth
                height=rootHeight
                getReadAndWritePos
                layerContent=layer.content
              />
            </div>;
          },
          layers,
        )
        |> Array.of_list
        |> ReasonReact.array
      )
    </div>,
};
