open Canvas;

type readerType =
  | Channel(channel)
  | Saturation;

module DecodeReaderType = {
  let readerTypeByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "saturation" => Saturation
      | "channel" =>
        json |> map(c => Channel(channel_of_int(c)), field("channel", int))
      | _ => Channel(R)
      }
    );

  let readerType = json =>
    Json.Decode.(
      json |> (field("type", string) |> andThen(readerTypeByType))
    );
};

module EncodeReaderType = {
  let readerType =
    Json.Encode.(
      fun
      | Saturation => object_([("type", string("saturation"))])
      | Channel(c) =>
        object_([
          ("type", string("channel")),
          ("channel", int(int_of_channel(c))),
        ])
    );
};

let allReaderTypes = [|
  Channel(R),
  Channel(G),
  Channel(B),
  Channel(A),
  Saturation,
|];

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~readerType, ~onChangeSetting, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(
                `String(
                  Js.Json.stringify(EncodeReaderType.readerType(readerType)),
                )
              )
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            let json = Json.parseOrRaise(s);
            let newSetting = DecodeReaderType.readerType(json);
            onChangeSetting(newSetting);
          }
        )>
        (
          ReasonReact.array(
            allReaderTypes
            |> Array.map(readerType => {
                 let s =
                   Js.Json.stringify(
                     EncodeReaderType.readerType(readerType),
                   );
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>;
               }),
          )
        )
      </Select>
    ),
};
