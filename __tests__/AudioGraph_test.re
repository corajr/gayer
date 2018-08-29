open Jest;
open Expect;

open Audio;
open AudioGraph;

let g = {nodes: Belt.Map.String.empty, edges: Belt.Map.String.empty};

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

let g2withEdge = {
  ...g2,
  edges:
    Belt.Map.String.fromArray([|
      ("0", Belt.Set.String.fromArray([|"1"|])),
    |]),
};

describe("addNode", () => {
  test("adds to empty graph", () =>
    expect(addNode(g, ("0", n1))) |> toEqual(g1)
  );

  test("adding same ID is idempotent", () =>
    expect(addNode(g1, ("0", n1))) |> toEqual(g1)
  );
});

describe("removeAllEdgesInvolvingNode", () => {
  test("does nothing if node is absent", () =>
    expect(removeAllEdgesInvolvingNode(g2withEdge, "2"))
    |> toEqual(g2withEdge)
  );

  test("removes the edges where node is source", () =>
    expect(removeAllEdgesInvolvingNode(g2withEdge, "0")) |> toEqual(g2)
  );

  test("removes the edges where node is target", () =>
    expect(removeAllEdgesInvolvingNode(g2withEdge, "1")) |> toEqual(g2)
  );
});

describe("removeNode", () => {
  test("does nothing if node is absent", () =>
    expect(removeNode(g, "0")) |> toEqual(g)
  );
  test("removes a node by ID", () =>
    expect(removeNode(g1, "0")) |> toEqual(g)
  );
  test("removes any associated edges", () =>
    expect(removeNode(g2withEdge, "1")) |> toEqual(g1)
  );
});

describe("addEdge", () => {
  test("connects two nodes", () =>
    expect(addEdge(g2, ("0", "1"))) |> toEqual(g2withEdge)
  );

  test("adding same edge is idempotent", () =>
    expect(addEdge(g2withEdge, ("0", "1"))) |> toEqual(g2withEdge)
  );
});

describe("removeEdge", () => {
  test("does nothing if edge is absent", () =>
    expect(removeEdge(g2, ("0", "1"))) |> toEqual(g2)
  );

  test("removes an edge by source and target IDs", () =>
    expect(removeEdge(g2withEdge, ("0", "1"))) |> toEqual(g2)
  );
});
