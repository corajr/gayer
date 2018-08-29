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

let removeAllEdgesInvolvingNode = (graph: audioGraph, key: nodeId) => {
  let edgesWithoutNodeAsSource = Belt.Map.String.remove(graph.edges, key);

  let edgesWithoutNodeAsTarget =
    Belt.Map.String.map(edgesWithoutNodeAsSource, s =>
      Belt.Set.String.remove(s, key)
    );

  let nonEmptyEdges =
    Belt.Map.String.keep(edgesWithoutNodeAsTarget, (_, s) =>
      ! Belt.Set.String.isEmpty(s)
    );

  {...graph, edges: nonEmptyEdges};
};

let removeNode = (graph: audioGraph, key: nodeId) : audioGraph =>
  removeAllEdgesInvolvingNode(
    {...graph, nodes: Belt.Map.String.remove(graph.nodes, key)},
    key,
  );

let addEdge =
    (graph: audioGraph, (sourceId, targetId): (nodeId, nodeId))
    : audioGraph => {
  let maybeSource = Belt.Map.String.get(graph.nodes, sourceId);
  let maybeTarget = Belt.Map.String.get(graph.nodes, targetId);

  switch (maybeSource, maybeTarget) {
  | (Some(source), Some(target)) => connect(source, target)
  | _ => ()
  };

  {
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
};

let removeEdge =
    (graph: audioGraph, (sourceId, targetId): (nodeId, nodeId))
    : audioGraph => {
  let maybeSource = Belt.Map.String.get(graph.nodes, sourceId);
  let maybeTarget = Belt.Map.String.get(graph.nodes, targetId);

  switch (maybeSource, maybeTarget) {
  | (Some(source), Some(target)) => disconnect(source, target)
  | _ => ()
  };

  {
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
};
