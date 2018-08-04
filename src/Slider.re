module Classes = {
  type classesType =
    | Root(string)
    | Container(string)
    | Track(string)
    | TrackBefore(string)
    | TrackAfter(string)
    | Thumb(string)
    | Vertical(string)
    | Disabled(string)
    | Jumped(string)
    | Focused(string)
    | Activated(string);
  type t = list(classesType);
  let to_string =
    fun
    | Root(_) => "root"
    | Container(_) => "container"
    | Track(_) => "track"
    | TrackBefore(_) => "trackBefore"
    | TrackAfter(_) => "trackAfter"
    | Thumb(_) => "thumb"
    | Vertical(_) => "vertical"
    | Disabled(_) => "disabled"
    | Jumped(_) => "jumped"
    | Focused(_) => "focused"
    | Activated(_) => "activated";
  let to_obj = listOfClasses =>
    listOfClasses
    |. Belt.List.reduce(
         Js.Dict.empty(),
         (obj, classType) => {
           switch (classType) {
           | Root(className)
           | Container(className)
           | Track(className)
           | TrackBefore(className)
           | TrackAfter(className)
           | Thumb(className)
           | Vertical(className)
           | Disabled(className)
           | Jumped(className)
           | Focused(className)
           | Activated(className) =>
             Js.Dict.set(obj, to_string(classType), className)
           };
           obj;
         },
       );
};

[@bs.obj]
external makeProps :
  (
    ~className: string=?,
    ~component: 'union_rr91=?,
    ~disabled: bool=?,
    ~reverse: bool=?,
    ~vertical: bool=?,
    ~max: float=?,
    ~min: float=?,
    ~step: float=?,
    ~value: float=?,
    ~onChange: 'any_rch7=?,
    ~classes: Js.Dict.t(string)=?,
    ~style: ReactDOMRe.Style.t=?,
    unit
  ) =>
  _ =
  "";

[@bs.module "@material-ui/lab/Slider/Slider"]
external reactClass : ReasonReact.reactClass = "default";

let make =
    (
      ~className: option(string)=?,
      ~component:
         option(
           [
             | `String(string)
             | `Callback('genericCallback)
             | `ObjectGeneric(Js.t({..}))
           ],
         )=?,
      ~disabled: option(bool)=?,
      ~reverse: option(bool)=?,
      ~vertical: option(bool)=?,
      ~max: option(float)=?,
      ~min: option(float)=?,
      ~step: option(float)=?,
      ~value: option(float)=?,
      ~classes: option(Classes.t)=?,
      ~onChange: option((ReactEventRe.Form.t, float) => unit)=?,
      ~style: option(ReactDOMRe.Style.t)=?,
      children,
    ) =>
  ReasonReact.wrapJsForReason(
    ~reactClass,
    ~props=
      makeProps(
        ~className?,
        ~component=?
          component |. Belt.Option.map(v => MaterialUi_Helpers.unwrapValue(v)),
        ~disabled?,
        ~reverse?,
        ~vertical?,
        ~max?,
        ~min?,
        ~step?,
        ~value?,
        ~onChange?,
        ~classes=?Belt.Option.map(classes, v => Classes.to_obj(v)),
        ~style?,
        (),
      ),
    children,
  );
