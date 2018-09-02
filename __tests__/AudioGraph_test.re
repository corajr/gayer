open Jest;
open Expect;

open Audio;
open AudioGraph;

let g = emptyAudioGraph;

let mockAudioNode: string => audioNode = [%bs.raw
  id => "return {'id': id, 'connect': (n) => 1, 'disconnect': (n) => 0 }"
];

let n1: audioNode = mockAudioNode("n1");
let n2: audioNode = mockAudioNode("n2");

let g1 = {...g, nodes: Belt.Map.String.fromArray([|("0", n1)|])};

let g2 = {
  ...g,
  nodes: Belt.Map.String.fromArray([|("0", n1), ("1", n2)|]),
};

let edge = ("0", "1", 0, 0);

let g2withEdge = {...g2, edges: Belt.Set.add(emptyEdgeSet, edge)};

describe("addNode", () => {
  test("adds to empty graph", () =>
    expect(addNode(("0", n1), g)) |> toEqual(g1)
  );

  test("adding same ID is idempotent", () =>
    expect(addNode(("0", n1), g1)) |> toEqual(g1)
  );
});

describe("removeAllEdgesInvolvingNode", () => {
  test("does nothing if node is absent", () =>
    expect(removeAllEdgesInvolvingNode("2", g2withEdge))
    |> toEqual(g2withEdge)
  );

  test("removes the edges where node is source", () =>
    expect(removeAllEdgesInvolvingNode("0", g2withEdge)) |> toEqual(g2)
  );

  test("removes the edges where node is target", () =>
    expect(removeAllEdgesInvolvingNode("1", g2withEdge)) |> toEqual(g2)
  );
});

describe("removeNode", () => {
  test("does nothing if node is absent", () =>
    expect(removeNode("0", g)) |> toEqual(g)
  );
  test("removes a node by ID", () =>
    expect(removeNode("0", g1)) |> toEqual(g)
  );
});

describe("addEdge", () => {
  test("connects two nodes", () =>
    expect(addEdge(edge, g2)) |> toEqual(g2withEdge)
  );

  test("adding same edge is idempotent", () =>
    expect(addEdge(edge, g2withEdge)) |> toEqual(g2withEdge)
  );
});

describe("removeEdge", () => {
  test("does nothing if edge is absent", () =>
    expect(removeEdge(edge, g2)) |> toEqual(g2)
  );

  test("removes an edge by source and target IDs", () =>
    expect(removeEdge(edge, g2withEdge)) |> toEqual(g2)
  );
});

describe("updateConnections", () => {
  test("does nothing if no edges", () =>
    expect(updateConnections(g2)) |> toEqual(g2)
  );

  test("adds connections that are not yet made", () =>
    expect(updateConnections(g2withEdge))
    |> toEqual({
         ...g2withEdge,
         actuallyConnectedEdges: Belt.Set.add(emptyEdgeSet, edge),
       })
  );

  test("removes connections that are no longer desired", () =>
    expect(
      updateConnections({
        ...g2,
        actuallyConnectedEdges: Belt.Set.add(emptyEdgeSet, edge),
      }),
    )
    |> toEqual(g2)
  );
});
