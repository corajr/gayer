open Canvas;
open UserMedia;

let attachVideoStream = [%bs.raw
  (video, stream) => {|
  video.srcObject = stream;
  return video;
|}
];
