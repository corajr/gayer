let allImages = [|
  "media/four_seasons.jpg",
  "media/harmony.jpg",
  "media/hilbert_curve.png",
  "media/hubble_ultra_deep_field.jpg",
  "media/is_it_a_crime_large.png",
  "media/cyber_winnie.png",
  "media/les_tres_riches_heures.jpg",
  "media/suleiman.jpg",
|];

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~url, ~savedImages, ~onChange, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(`String(url))
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            switch (Belt.Map.String.get(savedImages, s)) {
            | Some(x) => onChange(x)
            | None => onChange(s)
            };
          }
        )>
        (
          ReasonReact.array(
            allImages
            |> Array.append(Belt.Map.String.keysToArray(savedImages))
            |> Array.map(s =>
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>
               ),
          )
        )
      </Select>
    ),
};
