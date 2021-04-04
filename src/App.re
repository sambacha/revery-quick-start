/* Imports */

open Revery;
open Revery.UI;
open Revery.UI.Components;

/* Component styles */

module Styles = {
  open Style;

  let caret = (~line, ~character) => [
    position(`Absolute),
    left(character),
    top(line),
  ];

  let text = [color(Colors.black)];
  let textContainer = (~lineHeight) => [height(lineHeight)];

  let container = [flexGrow(1)];

  let editorContainer = [
    flexGrow(1),
    cursor(MouseCursors.text),
    padding(5),
  ];
};

/* Types */

module CaretSize = {
  type t = {
    width: int,
    height: int,
  };

  let make = (~width, ~height) => {width, height};
};

module CaretPosition = {
  type t = {
    line: int,
    character: int,
  };

  let make = (~line=0, ~character=0, ()) => {line, character};
};

/* Caret */

let getCaretSize = (~fontSize, ~fontFamily, ~fontWeight, ()) => {
  let {width, height}: Draw.Text.dimensions =
    Draw.Text.dimensions(~fontFamily, ~fontSize, ~fontWeight, "~");

  CaretSize.make(
    ~width=width |> int_of_float,
    ~height=height |> int_of_float,
  );
};

let getCaretEditorLocation =
    (
      ~fontSize,
      ~fontFamily,
      ~fontWeight,
      ~lineHeight,
      caretPosition: CaretPosition.t,
    ) => {
  let line = (caretPosition.line |> float_of_int) *. fontSize *. lineHeight;
  let character =
    (caretPosition.character |> float_of_int)
    *. Draw.Text.dimensions(~fontFamily, ~fontSize, ~fontWeight, "~").width;

  CaretPosition.make(
    ~line=line |> int_of_float,
    ~character=character |> int_of_float,
    (),
  );
};

/* States */

type editorState = {
  text: string,
  caretPosition: CaretPosition.t,
};

/* Actions */

type editorAction =
  | TextInput(string)
  | MoveCaretTo(CaretPosition.t);

/* Reducers */

let editorReducer = (action, state) =>
  switch (action) {
  | TextInput(text) => {...state, text}
  | MoveCaretTo(caretPosition) => {...state, caretPosition}
  };

/* Components */

let caret =
    (
      ~fontSize,
      ~fontFamily,
      ~fontWeight,
      ~lineHeight,
      ~caretPosition: CaretPosition.t,
      (),
    ) => {
  let {width, height}: CaretSize.t =
    getCaretSize(~fontSize, ~fontFamily, ~fontWeight, ());

  let {line, character}: CaretPosition.t =
    caretPosition
    |> getCaretEditorLocation(
         ~fontSize,
         ~fontFamily,
         ~fontWeight,
         ~lineHeight,
       );

  <View style={Styles.caret(~line, ~character)}>
    <Container width height color=Colors.black />
  </View>;
};

let%component main =
              (
                ~fontSize=14.,
                ~fontFamily=Font.Family.defaultMono,
                ~fontWeight=Font.Weight.Normal,
                ~lineHeight=1.5,
                (),
              ) => {
  let%hook (text, setText) = Hooks.state("");
  let%hook (caretPosition, setCaretPosition) =
    Hooks.state(CaretPosition.make());

  let onTextInput = (event: NodeEvents.textInputEventParams) => {
    setText(text => text ++ event.text);
    setCaretPosition(caretPosition =>
      CaretPosition.make(
        ~line=caretPosition.line,
        ~character=caretPosition.character + String.length(event.text),
        (),
      )
    );
  };

  let onKeyDown = (event: NodeEvents.keyEventParams) =>
    if (event.keycode == Key.Keycode.return) {
      setText(text => text ++ "\n");
      setCaretPosition(caretPosition =>
        CaretPosition.make(~line=caretPosition.line + 1, ())
      );
    };

  <Clickable style=Styles.editorContainer onTextInput onKeyDown>
    <ScrollView style=Styles.container bounce=false>
      {text
       |> Str.split_delim("\n" |> Str.regexp)
       |> List.map(text =>
            <View
              style={Styles.textContainer(
                ~lineHeight=fontSize *. lineHeight |> int_of_float,
              )}>
              <Text style=Styles.text fontSize fontFamily fontWeight text />
            </View>
          )
       |> React.listToElement}
      <caret fontSize fontFamily fontWeight lineHeight caretPosition />
    </ScrollView>
  </Clickable>;
};

/* Appliaction initialization */

let init = app => {
  Revery.App.initConsole();

  Timber.App.enable();
  Timber.App.setLevel(Timber.Level.perf);

  let win =
    App.createWindow(
      app,
      "Kilo",
      ~createOptions=
        WindowCreateOptions.create(
          ~backgroundColor=Colors.white,
          ~width=512,
          ~height=384,
          (),
        ),
    );

  let _update: Revery.UI.renderFunction = UI.start(win, <main />);
  ();
};

App.start(init);
