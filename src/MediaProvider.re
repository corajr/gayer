open Audio;
open Canvas;

type url = string;

type externalMedia =
  | AudioFile(url)
  | Image(url)
  | Video(url);

type state = {
  audioNodes: ref(Belt.Map.String.t(audioNode)),
  canvasRefs: ref(Belt.Map.String.t(canvasImageSource)),
};

let defaultState = {
  audioNodes: ref(Belt.Map.String.empty),
  canvasRefs: ref(Belt.Map.String.empty),
};

let component = ReasonReact.reducerComponent("MediaProvider");

let make = (~audioCtx, ~media, _children) => {
  let renderAudioFile = (self, url) => <audio src=url />;
  let renderImage = (self, url) => <img src=url />;
  let renderVideo = (self, url) => <video src=url />;

  let renderMedia = (self, media) =>
    switch (media) {
    | AudioFile(url) => renderAudioFile(self, url)
    | Image(url) => renderImage(self, url)
    | Video(url) => renderVideo(self, url)
    };

  {
    ...component,
    initialState: () => defaultState,
    reducer: ((), _state: state) => ReasonReact.NoUpdate,
    render: self =>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~visibility="hidden",
            ~position="absolute",
            (),
          )
        )>
        (Array.map(renderMedia(self), media) |> ReasonReact.array)
      </div>,
  };
};
