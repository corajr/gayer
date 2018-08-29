open Audio;

type nodeId = string;
type nodeSet = Belt.Set.String.t;

type audioGraph = {
  nodes: Belt.Map.String.t(audioNode),
  edges: Belt.Map.String.t(nodeSet),
};

let addNode =
    (graph: audioGraph, (key, node): (nodeId, audioNode))
    : audioGraph => {
  ...graph,
  nodes: Belt.Map.String.set(graph.nodes, key, node),
};

let removeNode = (graph: audioGraph, key: nodeId) : audioGraph => {
  ...graph,
  nodes: Belt.Map.String.remove(graph.nodes, key),
};

let addEdge =
    (graph: audioGraph, (sourceId, targetId): (nodeId, nodeId))
    : audioGraph => {
  ...graph,
  edges:
    Belt.Map.String.update(
      graph.edges,
      sourceId,
      fun
      | None => Some(Belt.Set.String.fromArray([|targetId|]))
      | Some(s) => Some(Belt.Set.String.add(s, targetId)),
    ),
};

let removeEdge =
    (graph: audioGraph, (sourceId, targetId): (nodeId, nodeId))
    : audioGraph => {
  ...graph,
  edges:
    Belt.Map.String.update(
      graph.edges,
      sourceId,
      fun
      | None => None
      | Some(s) => {
          let newS = Belt.Set.String.remove(s, targetId);
          Belt.Set.String.isEmpty(newS) ? None : Some(newS);
        },
    ),
};
