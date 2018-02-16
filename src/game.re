type action =
  | Click(int)
  | Unflip;

type card = {
  symbol: string,
  mutable flipped: bool
};

type state = {
  cards: list(card),
  flipped: (option(int), option(int))
};

let shuffle = items => {
  Random.init(int_of_float(Js.Date.now()));
  let rands = List.map(item => (Random.int(List.length(items)), item), items);
  let sorted =
    List.sort(
      ((aRand, _), (bRand, _)) =>
        if (aRand < bRand) {
          (-1);
        } else if (aRand > bRand) {
          1;
        } else {
          0;
        },
      rands
    );
  List.map(((_, item)) => item, sorted);
};

let double = items => List.append(items, items);

let ofCards = items => List.map(item => {symbol: item, flipped: false}, items);

let component = ReasonReact.reducerComponent("Game");

let setElementAt = (~index: int, ~value: 'a, theList: list('a)) =>
  List.mapi(
    (i, v) =>
      if (i == index) {
        value;
      } else {
        v;
      },
    theList
  );

let make = (~cards, _children) => {
  ...component,
  initialState: () => {
    cards: cards |> double |> shuffle |> ofCards,
    flipped: (None, None)
  },
  reducer: (action, state) =>
    switch (action, state.flipped) {
    | (Click(index), (None, None)) =>
      let cards = state.cards;
      let newCards =
        setElementAt(
          ~index,
          ~value={...List.nth(cards, index), flipped: true},
          cards
        );
      ReasonReact.Update({cards: newCards, flipped: (Some(index), None)});
    | (Click(index), (Some(flipped), None))
    | (Click(index), (None, Some(flipped))) =>
      let cards = state.cards;
      let newCards =
        setElementAt(
          ~index,
          ~value={...List.nth(cards, index), flipped: true},
          cards
        );
      if (List.nth(cards, index).symbol != List.nth(cards, flipped).symbol) {
        ReasonReact.UpdateWithSideEffects(
          {cards: newCards, flipped: (Some(flipped), Some(index))},
          (
            ({send}) => {
              let _id = Js.Global.setTimeout(() => send(Unflip), 300);
              ();
            }
          )
        );
      } else {
        ReasonReact.Update({cards: newCards, flipped: (None, None)});
      };
    | (Click(_), _) => ReasonReact.NoUpdate
    | (Unflip, (Some(flippedA), Some(flippedB))) =>
      let cards = state.cards;
      let newCards =
        setElementAt(
          ~index=flippedA,
          ~value={...List.nth(cards, flippedA), flipped: false},
          cards
        );
      let newCards =
        setElementAt(
          ~index=flippedB,
          ~value={...List.nth(newCards, flippedB), flipped: false},
          newCards
        );
      ReasonReact.Update({cards: newCards, flipped: (None, None)});
    | (Unflip, _) => ReasonReact.NoUpdate
    },
  render: ({state, send}) =>
    List.mapi(
      (i, card) =>
        <Card
          key=(string_of_int(i))
          symbol=card.symbol
          flipped=card.flipped
          handleClick=(_evt => send(Click(i)))
        />,
      state.cards
    )
    |> Array.of_list
    |> ReasonReact.arrayToElement
};
