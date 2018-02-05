let component = ReasonReact.statelessComponent("Card");

let make = (~symbol, ~flipped, ~handleClick, _children) => {
  ...component,
  render: _self => {
    let contents = flipped ? symbol : "?";
    let handler = flipped ? _e => () : handleClick;
    <div onClick=handler className="card">
      (ReasonReact.stringToElement(contents))
    </div>;
  }
};
