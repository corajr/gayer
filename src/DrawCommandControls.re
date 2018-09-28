let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~cmd, ~onChange, _children) => {
  ...component,
  render: self => <div />,
};
