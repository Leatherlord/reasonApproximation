open ReUtils;

let usageMsg = "esy x extra -f <frequency>\n";
let frequency = ref(0.1);
let anonFun = _ => ();

let speclist = [
  (
    "-f",
    Arg.Set_float(frequency),
    "Set distance between resulting values (default: 0.1)",
  ),
];

let () = Arg.parse(speclist, anonFun, usageMsg);

let maybeReadLine = () =>
  try(Some(readLine())) {
  | EndOfFile => None
  };

let generateXWanted = (fromX, toX, step) => {
  let amount = (toX -. fromX) /. step +. 1. |> ReFloat.toInt;
  Seq.init(amount, index => (index |> ReFloat.ofInt) *. step +. fromX)
  |> ReList.ofSeq;
};

let takeWindowFromBack = (windowSize, lst) => {
  let rec aux = (lst, acc) =>
    switch (lst) {
    | _ when List.length(acc) == windowSize => acc
    | [hd, ...tl] => aux(tl, [hd, ...acc])
    | _ => acc
    };
  aux(List.rev(lst), []);
};

let linearInterpolation = (x0, y0, x1, y1, x) =>
  (y0 *. (x1 -. x) +. y1 *. (x -. x0)) /. (x1 -. x0);

let counter = (first, second, prev) => {
  switch (first, second) {
  | ([x0, y0], [x1, y1]) =>
    let xList =
      switch (prev) {
      | ("", _) =>
        generateXWanted(x0, x1, frequency^)
        |> List.rev
        |> (
          (x1 -. x0)
          /. frequency^
          /. 2.
          |> Float.round
          |> ReFloat.toInt
          |> takeWindowFromBack
        )
        |> List.rev
      | (_, "") =>
        generateXWanted(x0, x1, frequency^)
        |> (
          (x1 -. x0)
          /. frequency^
          /. 2.
          |> Float.round
          |> ReFloat.toInt
          |> takeWindowFromBack
        )
      | (_, _) => [(x1 +. x0) /. 2.]
      };
    let transformer = linearInterpolation(x0, y0, x1, y1);
    let results = xList |> List.map(transformer);
    (xList, results);
  | _ => ([], [])
  };
};

let printer = (xList, yList) => {
  let rec aux = (xList, yList) =>
    switch (xList, yList) {
    | ([x, ...xtl], [y, ...ytl]) =>
      printString(
        "("
        ++ (Float.round(x *. 100.) /. 100. |> ReFloat.toString)
        ++ ","
        ++ (Float.round(y *. 100.) /. 100. |> ReFloat.toString)
        ++ "),",
      );
      aux(xtl, ytl);
    | _ => ()
    };
  aux(xList, yList);
  printEndline("");
};

let rec parser = (acc, prev) => {
  switch (acc) {
  | [hd, ...tl] =>
    switch (prev) {
    | ("", "") => reader(tl, ("", hd))
    | (_, pr) =>
      let first =
        pr |> ReString.splitOnChar(',') |> List.map(ReFloat.ofString);
      let second =
        hd |> ReString.splitOnChar(',') |> List.map(ReFloat.ofString);
      let (xList, yList) = counter(first, second, prev);
      printer(xList, yList);
      reader(tl, (pr, hd));
    }
  | [] =>
    let (f, s) = prev;
    let first = f |> ReString.splitOnChar(',') |> List.map(ReFloat.ofString);
    let second =
      s |> ReString.splitOnChar(',') |> List.map(ReFloat.ofString);
    let (xList, yList) = counter(first, second, (s, ""));
    printer(xList, yList);
    reader([], ("", ""));
  };
}
and reader = (acc, prev) => {
  switch (acc) {
  | [_, ..._] => parser(acc, prev)
  | [] =>
    switch (maybeReadLine()) {
    | Some(line) =>
      let points = List.concat([acc, ReString.splitOnChar(';', line)]);
      parser(points, prev);
    | None =>
      switch (prev) {
      | ("", "") => printEndline("Have a good one!")
      | _ => parser(acc, prev)
      }
    }
  };
};

reader([], ("", ""));
