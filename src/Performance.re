open Timing;

type t;

[@bs.val] external performance : t = "window.performance";

[@bs.send] external now : t => domHighResTimeStamp = "";

[@bs.deriving abstract]
type performanceEntry =
  pri {
    entryType: string,
    name: string,
    startTime: domHighResTimeStamp,
    duration: domHighResTimeStamp,
  };

[@bs.send] external mark : (t, string) => unit = "";

[@bs.send] external measure : (t, string, string, string) => unit = "";

[@bs.send]
external getEntriesByName : (t, string) => array(performanceEntry) = "";

[@bs.send] external clearMarks : t => unit = "";
[@bs.send] external clearMarksByName : (t, string) => unit = "clearMarks";

[@bs.send] external clearMeasures : t => unit = "";
[@bs.send]
external clearMeasuresByName : (t, string) => unit = "clearMeasures";
