open Canvas;
open UserMedia;

let attachVideoStream = [%bs.raw
  stream => {|
  var video = document.getElementsByTagName("video")[0];
  video.srcObject = stream;
  return video;
|}
];

let turnOnVideo: unit => Js.Promise.t(option(canvasImageSource)) =
  () =>
    switch (getVideoStream()) {
    | None => Js.Promise.resolve(None)
    | Some(stream) =>
      stream
      |> Js.Promise.then_(stream =>
           Js.Promise.resolve(Some(attachVideoStream(stream)))
         )
    };
