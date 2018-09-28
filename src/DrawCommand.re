open Canvas;

type drawContext = {
  maybeCtxRef: ref(option(canvasRenderingContext2D)),
  layerRefs: ref(Belt.Map.String.t(Dom.element)),
  width: int,
  height: int,
  variables: Belt.Map.String.t(int),
};

type imageData = {
  data: array(int),
  w: int,
  h: int,
};

type imgSource =
  | Self
  | LayerByKey(string)
  | ImageData(imageData);

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
  | Rotate(Canvas.rotation)
  | Translate(length, length)
  | DrawImage(imgSource, rect)
  | DrawImageSourceDest(imgSource, rect, rect);

let field2 = (f, a, aDec, b, bDec, json) =>
  Json.Decode.(
    json
    |> (field(a, aDec) |> andThen(a => map(b => f(a, b), field(b, bDec))))
  );

module EncodeDrawCommand = {
  let imgSource =
    Json.Encode.(
      fun
      | Self => object_([("type", string("self"))])
      | LayerByKey(k) =>
        object_([("type", string("self")), ("key", string(k))])
      | ImageData({data, w, h}) =>
        object_([
          ("type", string("image")),
          ("data", array(int, data)),
          ("w", int(w)),
          ("h", int(h)),
        ])
    );

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
        object_([("type", string("SetFillStyle")), ("style", string(s))])
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
      switch (json |> field("type", string)) {
      | "self" => Self
      | "layer" => LayerByKey(json |> field("key", string))
      | "imageData" =>
        ImageData({
          data: json |> field("data", array(int)),
          w: json |> field("w", int),
          h: json |> field("h", int),
        })
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
          json |> field2((a, b) => Multiply(a, b), "a", length, "b", length)
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
      let height = drawCtx.height;
      let pixelsPerSemitone = binsPerSemitone(height);
      height - i * pixelsPerSemitone;
    | Width => drawCtx.width
    | Height => drawCtx.height
    | Negate(x) => - getLength(drawCtx, x)
    | Add(a, b) => getLength(drawCtx, a) + getLength(drawCtx, b)
    | Multiply(a, b) => getLength(drawCtx, a) * getLength(drawCtx, b)
    | Divide(a, b) => getLength(drawCtx, a) / getLength(drawCtx, b)
    };

let nullImage =
  imageDataAsSource(
    createImageData(
      TypedArray.uint8ClampedArrayAsArray(
        TypedArray.createUint8ClampedArray(4),
      ),
      1,
      1,
    ),
  );

let getSource = (drawContext, src) : canvasImageSource =>
  Belt.Option.getWithDefault(
    switch (src) {
    | Self =>
      Belt.Option.map(drawContext.maybeCtxRef^, c =>
        getCanvasAsSource(Ctx.canvas(c))
      )
    | LayerByKey(s) =>
      Belt.Option.map(Belt.Map.String.get(drawContext.layerRefs^, s), d =>
        getCanvasAsSource(getFromReact(d))
      )
    | ImageData({data, w, h}) =>
      Some(imageDataAsSource(createImageData(data, w, h)))
    },
    nullImage,
  );

let drawCommand: (drawContext, command) => unit =
  (drawContext, cmd) =>
    switch (drawContext.maybeCtxRef^) {
    | Some(ctx) =>
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
        let realSrc = getSource(drawContext, src);
        Ctx.drawImageDestRect(
          ctx,
          realSrc,
          getLength(drawContext, x),
          getLength(drawContext, y),
          getLength(drawContext, w),
          getLength(drawContext, h),
        );
      | DrawImageSourceDest(
          src,
          {x: srcX, y: srcY, w: srcW, h: srcH},
          {x, y, w, h},
        ) =>
        let realSrc = getSource(drawContext, src);
        Ctx.drawImageSourceRectDestRect(
          ctx,
          realSrc,
          getLength(drawContext, srcX),
          getLength(drawContext, srcY),
          getLength(drawContext, srcW),
          getLength(drawContext, srcH),
          getLength(drawContext, x),
          getLength(drawContext, y),
          getLength(drawContext, w),
          getLength(drawContext, h),
        );
      }
    | None => ()
    };

let drawCommands: (drawContext, list(command)) => unit =
  (drawContext, cmds) => List.iter(drawCommand(drawContext), cmds);
