open ReUtils;

let usageMsg =
  "esy x app -f <frequency> -w <window_size> <algorithm_1> [<algorithm_2>] ...\n"
  ++ "Algorithms available: \n"
  ++ "'lin' - linear approximation [ax + b]\n"
  ++ "'sqr' - square polinomial approximation [ax^2 + bx + c]\n"
  ++ "'exp' - exponential approximation [ae^(bx)]\n"
  ++ "'log' - logarithmic approximation [aln(x) + b]\n"
  ++ "'pow' - power function approximation [ax^b]\n";

let algorithms = ref([]);
let frequency = ref(0.5);
let windowSize = ref(5);

let anonFun = algorithm => algorithms := [algorithm, ...algorithms^];

let speclist = [
  (
    "-f",
    Arg.Set_float(frequency),
    "Set distance between resulting values (default: 0.5)",
  ),
  (
    "-w",
    Arg.Set_int(windowSize),
    "Set size of window on incoming data that will be used for approximation (default: 5)",
  ),
];

let () = Arg.parse(speclist, anonFun, usageMsg);

let nameToFunction = name => {
  ReApproximation.(
    switch (name) {
    | "lin" => applyApproximation(linearApproximation, linearCount)
    | "sqr" => applyApproximation(squarePolinomialApproximation, squareCount)
    | "exp" => applyApproximation(exponentialApproximation, exponentialCount)
    | "log" => applyApproximation(logarithmicApproximation, logarithmicCount)
    | "pow" => applyApproximation(powerFunctionApproximation, powerCount)
    | _ => applyApproximation((_, _) => [0., 0.], (_, _) => 0.)
    }
  );
};

let parsePoints = lst => {
  let rec aux = (lst, acc) => {
    switch (lst) {
    | [hd, ...tl] =>
      let point = ReString.splitOnChar(',', hd);
      switch (point) {
      | [x, y] =>
        aux(tl, [(ReFloat.ofString(x), ReFloat.ofString(y)), ...acc])
      | _ => raise(NotFound)
      };
    | [] => acc
    };
  };
  [] |> aux(lst) |> List.rev;
};

let iterateOverMethods = (methods, xList, yList, xWantedList) => {
  let rec aux = (methods, xList, yList, xWantedList, acc) =>
    switch (methods) {
    | [name, ...tl] =>
      let f = name |> nameToFunction;
      aux(
        tl,
        xList,
        yList,
        xWantedList,
        [(name, f(xList, yList, xWantedList)), ...acc],
      );
    | [] => acc
    };

  aux(methods, xList, yList, xWantedList, []);
};

let generateXWanted = (fromX, toX, step) => {
  let amount = (toX -. fromX) /. step +. 1. |> ReFloat.toInt;
  Seq.init(amount, index => (index |> ReFloat.ofInt) *. step +. fromX)
  |> ReList.ofSeq;
};

let splitListInHalf = lst => {
  let rec aux = (lst, acc) => {
    switch (lst) {
    | [hd, ..._] when List.length(lst) - List.length(acc) <= 1 => (
        List.rev([hd, ...acc]),
        lst,
      )
    | [hd, ...tl] => aux(tl, [hd, ...acc])
    | [] => ([], [])
    };
  };

  aux(lst, []);
};

type modeType =
  | First
  | Basic
  | Last;

let takeWindowFromBack = lst => {
  let rec aux = (lst, acc) =>
    switch (lst) {
    | _ when List.length(acc) == windowSize^ => acc
    | [hd, ...tl] => aux(tl, [hd, ...acc])
    | _ => acc
    };
  aux(List.rev(lst), []);
};

let mainRoutine = (acc, mode) => {
  let rec iterateResults = (results, mode) => {
    let rec iterateYList =
      fun
      | [(x, y), ...tl] => {
          printString(
            "("
            ++ (x |> ReFloat.toString)
            ++ ","
            ++ (Float.round(y *. 100.) /. 100. |> ReFloat.toString)
            ++ "), ",
          );
          iterateYList(tl);
        }
      | [] => printEndline("");

    switch (results) {
    | [(name, yList), ...tl] =>
      printEndline(name ++ ":");
      switch (mode) {
      | First =>
        let (b, _) = splitListInHalf(yList);
        iterateYList(b);
      | Basic =>
        iterateYList([List.nth(yList, (List.length(yList) - 1) / 2)])
      | Last =>
        let (_, e) = splitListInHalf(yList);
        iterateYList(e);
      };
      printEndline("================================================");
      iterateResults(tl, mode);
    | [] => ()
    };
  };

  let window = takeWindowFromBack(acc);
  let points =
    try(parsePoints(window)) {
    | NotFound =>
      printEndline("Wrong format! Shoud be: `<X1>,<Y1>;<X2>,<Y2>;...`");
      [];
    | Failure(_) =>
      printEndline("Invalid floats!");
      [];
    };
  let (xList, yList) = List.split(points);
  let xWanted =
    if (List.length(xList) >= 1) {
      generateXWanted(
        xList |> List.hd,
        xList |> List.rev |> List.hd,
        frequency^,
      );
    } else {
      [0.];
    };

  let results = iterateOverMethods(algorithms^, xList, yList, xWanted);
  iterateResults(results, mode);
};

let maybeReadLine = () =>
  try(Some(readLine())) {
  | EndOfFile => None
  };

let rec mainLoop = acc => {
  switch (maybeReadLine()) {
  | Some(line) =>
    let points = List.concat([acc, ReString.splitOnChar(';', line)]);
    mainRoutine(points, Basic);
    mainLoop(points);
  | None =>
    mainRoutine(acc, Last);
    printEndline("Have a nice day!");
  };
};

let rec firstWindowCollection = acc => {
  switch (acc) {
  | [] =>
    printEndline(
      "Start entering points in the form of: `<X1>,<Y1>;<X2>,<Y2>;...`",
    )
  | _ => ()
  };

  switch (maybeReadLine()) {
  | Some(line) =>
    let points = List.concat([acc, ReString.splitOnChar(';', line)]);
    if (List.length(points) >= windowSize^) {
      let firstWindow = List.rev(takeWindowFromBack(List.rev(points)));
      mainRoutine(firstWindow, First);
      mainRoutine(points, Basic);
      mainLoop(points);
    } else {
      firstWindowCollection(points);
    };
  | None => printEndline("Have a nice day!")
  };
};

firstWindowCollection([]);
