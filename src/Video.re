open Canvas;
open UserMedia;

let attachVideoStream: (Dom.element, mediaStream) => canvasImageSource = [%bs.raw
  (video, stream) => {|
  video.srcObject = stream;
  return video;
|}
];

let unmute = [%bs.raw video => "video.muted = false;"];

let component = ReasonReact.statelessComponent("Video");

let make = (~audioCtx, ~url, ~setAudioRef, ~setImageRef) => {
  let setRef = theRef => {
    setImageRef(theRef);
    setAudioRef(theRef);
  };

  {
    ...component,
    render: self =>
      <video
        ref=setRef
        src=url
        width="120"
        height="120"
        autoPlay=true
        loop=true
      />,
  };
};
