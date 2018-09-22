/* Modified from https://github.com/reasonml-community/reason-maze/blob/master/src/FFI/Canvas.re */

type canvasRenderingContext2D;
type ctx = canvasRenderingContext2D;
type canvasElement;

type canvasImageSource;

type image;

let defaultSize = 240;

[@bs.deriving abstract]
type imageData =
  pri {
    data: array(int),
    width: int,
    height: int,
  };

type channel =
  | R
  | G
  | B
  | A;

let int_of_channel = channel =>
  switch (channel) {
  | R => 0
  | G => 1
  | B => 2
  | A => 3
  };

let channel_of_int = int =>
  switch (int) {
  | 0 => R
  | 1 => G
  | 2 => B
  | 3 => A
  | _ => R
  };

type pixel = {
  r: float,
  g: float,
  b: float,
  a: float,
};

type dim = int;

type rotation = float;

let tau: rotation = Js.Math._PI *. 2.0;

let degreesToRadians: float => rotation = degrees => degrees *. (360.0 /. tau);

type rect = {
  x: dim,
  y: dim,
  w: dim,
  h: dim,
};

type compositeOperation =
  | SourceOver
  | SourceIn
  | SourceOut
  | SourceAtop
  | DestinationOver
  | DestinationIn
  | DestinationOut
  | DestinationAtop
  | Lighter
  | Copy
  | Xor
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | ColorDodge
  | ColorBurn
  | HardLight
  | SoftLight
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity;

let string_of_compositeOperation: compositeOperation => string =
  fun
  | SourceOver => "source-over"
  | SourceIn => "source-in"
  | SourceOut => "source-out"
  | SourceAtop => "source-atop"
  | DestinationOver => "destination-over"
  | DestinationIn => "destination-in"
  | DestinationOut => "destination-out"
  | DestinationAtop => "destination-atop"
  | Lighter => "lighter"
  | Copy => "copy"
  | Xor => "xor"
  | Multiply => "multiply"
  | Screen => "screen"
  | Overlay => "overlay"
  | Darken => "darken"
  | Lighten => "lighten"
  | ColorDodge => "color-dodge"
  | ColorBurn => "color-burn"
  | HardLight => "hard-light"
  | SoftLight => "soft-light"
  | Difference => "difference"
  | Exclusion => "exclusion"
  | Hue => "hue"
  | Saturation => "saturation"
  | Color => "color"
  | Luminosity => "luminosity";

let compositeOperation_of_string: string => compositeOperation =
  fun
  | "source-over" => SourceOver
  | "source-in" => SourceIn
  | "source-out" => SourceOut
  | "source-atop" => SourceAtop
  | "destination-over" => DestinationOver
  | "destination-in" => DestinationIn
  | "destination-out" => DestinationOut
  | "destination-atop" => DestinationAtop
  | "lighter" => Lighter
  | "copy" => Copy
  | "xor" => Xor
  | "multiply" => Multiply
  | "screen" => Screen
  | "overlay" => Overlay
  | "darken" => Darken
  | "lighten" => Lighten
  | "color-dodge" => ColorDodge
  | "color-burn" => ColorBurn
  | "hard-light" => HardLight
  | "soft-light" => SoftLight
  | "difference" => Difference
  | "exclusion" => Exclusion
  | "hue" => Hue
  | "saturation" => Saturation
  | "color" => Color
  | "luminosity" => Luminosity
  | _ => SourceOver;

type transformMatrix = {
  horizontalScaling: float,
  horizontalSkewing: float,
  verticalSkewing: float,
  verticalScaling: float,
  horizontalMoving: float,
  verticalMoving: float,
};

let defaultTransform: transformMatrix = {
  horizontalScaling: 1.0,
  horizontalSkewing: 0.0,
  verticalSkewing: 0.0,
  verticalScaling: 1.0,
  horizontalMoving: 0.0,
  verticalMoving: 0.0,
};

/* Only supported in Chrome and Firefox. */

type percentage = int;

type length = string;
type degree = string;
type color = string;

let rgba: (int, int, int, float) => color =
  (r, g, b, a) =>
    "rgba("
    ++ Js.Int.toString(r)
    ++ ","
    ++ Js.Int.toString(g)
    ++ ","
    ++ Js.Int.toString(b)
    ++ ","
    ++ Js.Float.toString(a)
    ++ ")";

type filter =
  | Url(string)
  | Blur(length)
  | Brightness(percentage)
  | Contrast(percentage)
  | DropShadow(length, length, length, color)
  | Grayscale(percentage)
  | HueRotate(degree)
  | Invert(percentage)
  | Opacity(percentage)
  | Saturate(percentage)
  | Sepia(percentage)
  | NoFilter;

let string_of_filter: filter => string =
  fun
  | Url(url) => "url(" ++ url ++ ")"
  | Blur(length) => "blur(" ++ length ++ ")"
  | Brightness(percentage) =>
    "brightness(" ++ Js.Int.toString(percentage) ++ "%)"
  | Contrast(percentage) =>
    "contrast(" ++ Js.Int.toString(percentage) ++ "%)"
  | DropShadow(offsetX, offsetY, blurRadius, color) =>
    "drop-shadow("
    ++ Rationale.RList.join(" ", [offsetX, offsetY, blurRadius, color])
    ++ ")"
  | Grayscale(percentage) =>
    "grayscale(" ++ Js.Int.toString(percentage) ++ "%)"
  | HueRotate(degree) => "hue-rotate(" ++ degree ++ ")"
  | Invert(percentage) => "invert(" ++ Js.Int.toString(percentage) ++ "%)"
  | Opacity(percentage) => "opacity(" ++ Js.Int.toString(percentage) ++ "%)"
  | Saturate(percentage) =>
    "saturate(" ++ Js.Int.toString(percentage) ++ "%)"
  | Sepia(percentage) => "sepia(" ++ Js.Int.toString(percentage) ++ "%)"
  | NoFilter => "none";

external getFromReact : Dom.element => canvasElement = "%identity";

[@bs.get] external canvasWidth : canvasElement => int = "width";
[@bs.get] external canvasHeight : canvasElement => int = "height";

[@bs.send] external toDataURL : canvasElement => string = "";

[@bs.send]
external getContext :
  (canvasElement, [@bs.as "2d"] _) => canvasRenderingContext2D =
  "getContext";

external getCanvasImageSource : image => canvasImageSource = "%identity";

[@bs.new]
external createImageData : (array(int), int, int) => imageData = "ImageData";

/* canvas api methods */
module Ctx = {
  [@bs.get] external canvas : ctx => canvasElement = "";

  [@bs.set]
  external _setGlobalCompositeOperation : (ctx, string) => unit =
    "globalCompositeOperation";

  let setGlobalCompositeOperation = (ctx, compositeOperation) =>
    _setGlobalCompositeOperation(
      ctx,
      string_of_compositeOperation(compositeOperation),
    );

  [@bs.set] external setGlobalAlpha : (ctx, float) => unit = "globalAlpha";

  [@bs.set] external setFillStyle : (ctx, string) => unit = "fillStyle";

  [@bs.set] external setStrokeStyle : (ctx, string) => unit = "strokeStyle";

  [@bs.set] external setStrokeWidth : (ctx, dim) => unit = "lineWidth";

  [@bs.set] external setLineWidth : (ctx, dim) => unit = "lineWidth";

  [@bs.set] external setLineCap : (ctx, string) => unit = "lineCap";

  [@bs.set] external setFont : (ctx, string) => unit = "font";

  [@bs.set] external setTextAlign : (ctx, string) => unit = "textAlign";

  [@bs.set] external setTextBaseline : (ctx, string) => unit = "textBaseline";

  [@bs.set] external _setFilter : (ctx, string) => unit = "filter";

  let setFilter: (ctx, list(filter)) => unit =
    (ctx, filters) =>
      _setFilter(
        ctx,
        Rationale.RList.join(" ", List.map(string_of_filter, filters)),
      );

  [@bs.send]
  external _transform : (ctx, float, float, float, float, float, float) => unit =
    "transform";

  let transform: (ctx, transformMatrix) => unit =
    (
      ctx,
      {
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
      },
    ) =>
      _transform(
        ctx,
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
      );

  [@bs.send]
  external _setTransform :
    (ctx, float, float, float, float, float, float) => unit =
    "setTransform";

  let setTransform: (ctx, transformMatrix) => unit =
    (
      ctx,
      {
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
      },
    ) =>
      _setTransform(
        ctx,
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
      );

  [@bs.send] external rotate : (ctx, rotation) => unit = "";

  /* void ctx.drawImage(image, dx, dy); */
  [@bs.send]
  external drawImage : (ctx, canvasImageSource, dim, dim) => unit = "";

  /* void ctx.drawImage(image, dx, dy, dWidth, dHeight); */
  [@bs.send]
  external drawImageDestRect :
    (ctx, canvasImageSource, dim, dim, dim, dim) => unit =
    "drawImage";

  /* void ctx.drawImage(image, sx, sy, sWidth, sHeight, dx, dy, dWidth, dHeight); */
  [@bs.send]
  external drawImageSourceRectDestRect :
    (ctx, canvasImageSource, dim, dim, dim, dim, dim, dim, dim, dim) => unit =
    "drawImage";

  [@bs.send] external fillRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send] external fillText : (ctx, string, dim, dim) => unit = "";

  [@bs.send] external strokeText : (ctx, string, dim, dim) => unit = "";

  [@bs.send] external strokeRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send] external clearRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send]
  external ellipse : (ctx, dim, dim, dim, dim, float, float, float) => unit =
    "";

  let circle = (ctx, x, y, r) =>
    ellipse(ctx, x, y, r, r, 0.0, 0.0, 2.0 *. [%bs.raw "Math.PI"]);

  [@bs.send] external moveTo : (ctx, dim, dim) => unit = "";

  [@bs.send] external arc : (ctx, dim, dim, dim, dim, dim) => unit = "";

  [@bs.send]
  external arc_rev :
    (ctx, dim, dim, dim, dim, dim, [@bs.as {json|true|json}] _) => unit =
    "arc";

  let moveToPos = (ctx, (x, y)) => moveTo(ctx, x, y);

  [@bs.send] external lineTo : (ctx, dim, dim) => unit = "";

  let lineToPos = (ctx, (x, y)) => lineTo(ctx, x, y);

  [@bs.send] external fill : ctx => unit = "";

  [@bs.send] external beginPath : ctx => unit = "";

  [@bs.send] external closePath : ctx => unit = "";

  [@bs.send] external stroke : ctx => unit = "";

  let line = (ctx, (x, y), (a, b)) => {
    beginPath(ctx);
    moveTo(ctx, x, y);
    lineTo(ctx, a, b);
    stroke(ctx);
  };

  /* https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getImageData */
  /* getImageData(sx, sy, sw, sh); */
  [@bs.send]
  external getImageData : (ctx, dim, dim, dim, dim) => imageData = "";

  /* https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/putImageData */
  /* putImageData(imageData, dx, dy); */
  [@bs.send] external putImageData : (ctx, imageData, dim, dim) => unit = "";
};

external getCanvasAsSource : canvasElement => canvasImageSource = "%identity";

external getElementAsImageSource : Dom.element => canvasImageSource =
  "%identity";

let simpleDrawImage =
    (
      ~ctx: ctx,
      ~image: image,
      ~compositeOperation: compositeOperation,
      ~alphaValue: float,
    ) => {
  Ctx.setGlobalCompositeOperation(ctx, compositeOperation);
  Ctx.setGlobalAlpha(ctx, alphaValue);
  Ctx.drawImage(ctx, getCanvasImageSource(image), 0, 0);
};

let loadImage: (~url: string, ~onLoad: canvasImageSource => unit) => unit = [%bs.raw
  (src, onLoad) => {|
     var img = new Image;

     img.crossOrigin = "Anonymous";

     img.onload = function() {
       onLoad(img);
     }

     img.src = src;
     // make sure the load event fires for cached images too
     if ( img.complete || img.complete === undefined ) {
     img.src = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==";
     img.src = src;
}
     |}
];

let clamp = (~minVal: int=0, ~maxVal: int=255, i: int) : int =>
  min(maxVal, max(minVal, i));

let wrapCoord: (int, int, int) => int =
  (index, delta, size) => {
    let newCoord = index + delta;
    if (newCoord >= 0 && newCoord < size) {
      newCoord;
    } else if (newCoord >= 0) {
      newCoord mod size;
    } else {
      size - abs(newCoord mod size);
    };
  };

let binsPerSemitone: int => int = height => height / 120;

module DrawCommand = {
  type drawContext = {
    ctx: canvasRenderingContext2D,
    variables: Belt.Map.String.t(int),
  };

  type imgSource =
    | Self;

  type length =
    | Constant(int)
    | Variable(string, int)
    | Pixels(int)
    | Note(int)
    | Width
    | Height
    | Negate(length)
    | Add(length, length)
    | Divide(length, length)
    | Multiply(length, length);

  type rect = {
    x: length,
    y: length,
    w: length,
    h: length,
  };

  type command =
    | SetFont(string)
    | SetTextAlign(string)
    | SetTextBaseline(string)
    | SetFillStyle(string)
    | SetStrokeStyle(string)
    | FillRect(rect)
    | FillText(string, length, length)
    | StrokeText(string, length, length)
    | Rotate(rotation)
    | Translate(length, length)
    | DrawImage(imgSource, rect)
    | DrawImageSourceDest(imgSource, rect, rect);

  let field2 = (f, a, aDec, b, bDec, json) =>
    Json.Decode.(
      json
      |> (
        field(a, aDec) |> andThen(a => map(b => f(a, b), field(b, bDec)))
      )
    );

  module EncodeDrawCommand = {
    let imgSource = _r => Json.Encode.string("self");
    let rec length =
      Json.Encode.(
        fun
        | Constant(v) =>
          object_([("type", string("constant")), ("v", int(v))])
        | Variable(s, i) =>
          object_([
            ("type", string("var")),
            ("name", string(s)),
            ("default", int(i)),
          ])
        | Pixels(i) => object_([("type", string("px")), ("i", int(i))])
        | Note(i) => object_([("type", string("note")), ("note", int(i))])
        | Width => object_([("type", string("width"))])
        | Height => object_([("type", string("height"))])
        | Negate(x) => object_([("type", string("-")), ("x", length(x))])
        | Multiply(a, b) =>
          object_([
            ("type", string("*")),
            ("a", length(a)),
            ("b", length(b)),
          ])
        | Divide(a, b) =>
          object_([
            ("type", string("/")),
            ("a", length(a)),
            ("b", length(b)),
          ])
        | Add(a, b) =>
          object_([
            ("type", string("+")),
            ("a", length(a)),
            ("b", length(b)),
          ])
      );

    let rect = r =>
      Json.Encode.(
        object_([
          ("x", length(r.x)),
          ("y", length(r.y)),
          ("w", length(r.w)),
          ("h", length(r.h)),
        ])
      );

    let command =
      Json.Encode.(
        fun
        | SetFont(s) =>
          object_([("type", string("SetFont")), ("font", string(s))])
        | SetTextAlign(s) =>
          object_([
            ("type", string("SetTextAlign")),
            ("textAlign", string(s)),
          ])
        | SetTextBaseline(s) =>
          object_([
            ("type", string("SetTextBaseline")),
            ("textBaseline", string(s)),
          ])
        | SetFillStyle(s) =>
          object_([
            ("type", string("SetFillStyle")),
            ("style", string(s)),
          ])
        | SetStrokeStyle(s) =>
          object_([
            ("type", string("SetStrokeStyle")),
            ("style", string(s)),
          ])
        | FillRect(r) =>
          object_([("type", string("FillRect")), ("rect", rect(r))])
        | FillText(s, x, y) =>
          object_([
            ("type", string("FillText")),
            ("text", string(s)),
            ("x", length(x)),
            ("y", length(y)),
          ])
        | StrokeText(s, x, y) =>
          object_([
            ("type", string("StrokeText")),
            ("text", string(s)),
            ("x", length(x)),
            ("y", length(y)),
          ])
        | Rotate(r) =>
          object_([("type", string("Rotate")), ("rad", float(r))])
        | Translate(x, y) =>
          object_([
            ("type", string("Translate")),
            ("x", length(x)),
            ("y", length(y)),
          ])
        | DrawImage(src, r) =>
          object_([
            ("type", string("DrawImage")),
            ("src", imgSource(src)),
            ("destRect", rect(r)),
          ])
        | DrawImageSourceDest(src, srcR, destR) =>
          object_([
            ("type", string("DrawImageSourceDest")),
            ("src", imgSource(src)),
            ("srcRect", rect(srcR)),
            ("destRect", rect(destR)),
          ])
      );
  };

  module DecodeDrawCommand = {
    let imgSource = json =>
      Json.Decode.(
        switch (string(json)) {
        | _ => Self
        }
      );

    let rec length = json => {
      let lengthByType = (type_, json) =>
        Json.Decode.(
          switch (type_) {
          | "constant" => json |> map(i => Constant(i), field("v", int))
          | "px" => json |> map(i => Pixels(i), field("i", int))
          | "width" => Width
          | "height" => Height
          | "+" =>
            json |> field2((a, b) => Add(a, b), "a", length, "b", length)
          | "*" =>
            json
            |> field2((a, b) => Multiply(a, b), "a", length, "b", length)
          | "/" =>
            json |> field2((a, b) => Divide(a, b), "a", length, "b", length)

          | "var" =>
            json
            |> field2(
                 (a, b) => Variable(a, b),
                 "name",
                 string,
                 "default",
                 int,
               )
          | "-" => json |> map(x => Negate(x), field("x", length))

          | "note" => json |> map(i => Note(i), field("note", int))
          | _ =>
            raise @@
            DecodeError(
              "Expected length type, got " ++ Js.Json.stringify(json),
            )
          }
        );

      json |> Json.Decode.(field("type", string) |> andThen(lengthByType));
    };

    let rect = json =>
      Json.Decode.{
        x: json |> field("x", length),
        y: json |> field("y", length),
        w: json |> field("w", length),
        h: json |> field("h", length),
      };

    let commandByType: (string, Js.Json.t) => command =
      (type_, json) =>
        Json.Decode.(
          switch (type_) {
          | "SetFillStyle" =>
            json |> map(s => SetFillStyle(s), field("style", string))
          | "SetFont" => json |> map(s => SetFont(s), field("font", string))
          | "SetTextAlign" =>
            json |> map(s => SetTextAlign(s), field("textAlign", string))
          | "SetTextBaseline" =>
            json
            |> map(s => SetTextBaseline(s), field("textBaseline", string))
          | "SetStrokeStyle" =>
            json |> map(s => SetStrokeStyle(s), field("style", string))
          | "FillText" =>
            json
            |> (
              field("text", string)
              |> andThen(s =>
                   field("x", length)
                   |> andThen(x =>
                        map(y => FillText(s, x, y), field("y", length))
                      )
                 )
            )
          | "StrokeText" =>
            json
            |> (
              field("text", string)
              |> andThen(s =>
                   field("x", length)
                   |> andThen(x =>
                        map(y => StrokeText(s, x, y), field("y", length))
                      )
                 )
            )
          | "DrawImage" =>
            json
            |> (
              field("src", imgSource)
              |> andThen(src =>
                   map(
                     rect_ => DrawImage(src, rect_),
                     field("destRect", rect),
                   )
                 )
            )
          | "DrawImageSourceDest" =>
            json
            |> (
              field("src", imgSource)
              |> andThen(src =>
                   field("srcRect", rect)
                   |> andThen(srcR =>
                        map(
                          destR => DrawImageSourceDest(src, srcR, destR),
                          field("destRect", rect),
                        )
                      )
                 )
            )
          | "Translate" =>
            json
            |> (
              field("x", length)
              |> andThen(x => map(y => Translate(x, y), field("y", length)))
            )

          | "FillRect" => json |> map(r => FillRect(r), field("rect", rect))
          | "Rotate" => json |> map(r => Rotate(r), field("rad", float))
          | _ =>
            raise @@
            DecodeError(
              "Expected layer content, got " ++ Js.Json.stringify(json),
            )
          }
        );

    let command: Js.Json.t => command =
      json =>
        json |> Json.Decode.(field("type", string) |> andThen(commandByType));
  };

  let rec getLength: (drawContext, length) => int =
    (drawCtx, len) =>
      switch (len) {
      | Constant(v) => v
      | Variable(s, i) =>
        Belt.Map.String.getWithDefault(drawCtx.variables, s, i)
      | Pixels(i) => i
      | Note(i) =>
        let height = canvasHeight(Ctx.canvas(drawCtx.ctx));
        let pixelsPerSemitone = binsPerSemitone(height);
        height - i * pixelsPerSemitone;
      | Width => canvasWidth(Ctx.canvas(drawCtx.ctx))
      | Height => canvasHeight(Ctx.canvas(drawCtx.ctx))
      | Negate(x) => - getLength(drawCtx, x)
      | Add(a, b) => getLength(drawCtx, a) + getLength(drawCtx, b)
      | Multiply(a, b) => getLength(drawCtx, a) * getLength(drawCtx, b)
      | Divide(a, b) => getLength(drawCtx, a) / getLength(drawCtx, b)
      };

  let drawCommand: (drawContext, command) => unit =
    (drawContext, cmd) => {
      let {ctx} = drawContext;
      switch (cmd) {
      | SetFont(font) => Ctx.setFont(ctx, font)
      | SetTextAlign(textAlign) => Ctx.setTextAlign(ctx, textAlign)
      | SetTextBaseline(textBaseline) =>
        Ctx.setTextBaseline(ctx, textBaseline)
      | SetFillStyle(style) => Ctx.setFillStyle(ctx, style)
      | SetStrokeStyle(style) => Ctx.setStrokeStyle(ctx, style)
      | Translate(x, y) =>
        Ctx.transform(
          ctx,
          {
            ...defaultTransform,
            horizontalMoving: float_of_int(getLength(drawContext, x)),
            verticalMoving: float_of_int(getLength(drawContext, y)),
          },
        )
      | Rotate(r) => Ctx.rotate(ctx, r)
      | FillRect({x, y, w, h}) =>
        Ctx.fillRect(
          ctx,
          getLength(drawContext, x),
          getLength(drawContext, y),
          getLength(drawContext, w),
          getLength(drawContext, h),
        )
      | FillText(s, x, y) =>
        Ctx.fillText(
          ctx,
          s,
          getLength(drawContext, x),
          getLength(drawContext, y),
        )
      | StrokeText(s, x, y) =>
        Ctx.strokeText(
          ctx,
          s,
          getLength(drawContext, x),
          getLength(drawContext, y),
        )
      | DrawImage(src, {x, y, w, h}) =>
        switch (src) {
        | Self =>
          Ctx.drawImageDestRect(
            ctx,
            getCanvasAsSource(Ctx.canvas(ctx)),
            getLength(drawContext, x),
            getLength(drawContext, y),
            getLength(drawContext, w),
            getLength(drawContext, h),
          )
        }
      | DrawImageSourceDest(
          src,
          {x: srcX, y: srcY, w: srcW, h: srcH},
          {x, y, w, h},
        ) =>
        switch (src) {
        | Self =>
          Ctx.drawImageSourceRectDestRect(
            ctx,
            getCanvasAsSource(Ctx.canvas(ctx)),
            getLength(drawContext, srcX),
            getLength(drawContext, srcY),
            getLength(drawContext, srcW),
            getLength(drawContext, srcH),
            getLength(drawContext, x),
            getLength(drawContext, y),
            getLength(drawContext, w),
            getLength(drawContext, h),
          )
        }
      };
    };

  let drawCommands: (drawContext, list(command)) => unit =
    (drawContext, cmds) => List.iter(drawCommand(drawContext), cmds);
};
