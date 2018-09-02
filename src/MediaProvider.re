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
      ~saveTick,
      ~millisPerAudioTick,
      _children,
    ) => {
  ...component,
  render: self =>
    <div
      style=(
        ReactDOMRe.Style.make(~visibility="hidden", ~position="absolute", ())
      )>
      (
        List.map(
          layer => {
            let key =
              Js.Json.stringify(EncodeLayer.layerContent(layer.content));
            switch (layer.content) {
            | Analysis(source) =>
              let (_, maybeInput) = getAudio(source);
              switch (maybeInput) {
              | Some(input) =>
                audioGraph :=
                  audioGraph^
                  |> addNode((key ++ "input", input))
                  |> updateConnections
              | None => ()
              };
              ();
            | _ => ()
            };

            <LayerContent
              audioCtx
              audioGraph
              layerKey=key
              setRef=(onSetRef(layer))
              saveTick
              millisPerTick=millisPerAudioTick
              width=rootWidth
              height=rootHeight
              layerContent=layer.content
            />;
          },
          layers,
        )
        |> Array.of_list
        |> ReasonReact.array
      )
    </div>,
};
