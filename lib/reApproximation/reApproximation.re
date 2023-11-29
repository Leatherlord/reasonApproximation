open Owl;

let sumSomeLists = list => {
  let rec mulAux = (list, acc) => {
    switch (list) {
    | [[hhd, ..._], ...tl] => mulAux(tl, acc *. hhd)
    | _ => acc
    };
  };

  let rec cutAux = list => {
    switch (list) {
    | [[_, ...htl], ...tl] => [htl, ...cutAux(tl)]
    | _ => []
    };
  };

  let rec countAux = (list, acc) => {
    switch (list) {
    | [[], ..._] => acc
    | [_, ..._] => mulAux(list, 1.0) +. acc |> countAux(cutAux(list))
    | _ => acc
    };
  };

  countAux(list, 0.0);
};

let pearsonC = (xList, yList) => {
  let xAvg = sumSomeLists([xList]) /. (xList |> List.length |> ReFloat.ofInt);
  let yAvg = sumSomeLists([yList]) /. (yList |> List.length |> ReFloat.ofInt);

  let rec aux = (xList, yList, topSum, botSum1, botSum2) => {
    switch (xList, yList) {
    | ([xhd, ...xtl], [yhd, ...ytl]) =>
      let xSub = xhd -. xAvg;
      let ySub = yhd -. yAvg;
      aux(
        xtl,
        ytl,
        xSub *. ySub +. topSum,
        xSub *. xSub +. botSum1,
        ySub *. ySub +. botSum2,
      );
    | (_, _) => (topSum, botSum1, botSum2)
    };
  };

  let (topSum, botSum1, botSum2) = aux(xList, yList, 0.0, 0.0, 0.0);
  topSum /. Float.sqrt(botSum1 *. botSum2);
};

let linearApproximation = (xList, yList) => {
  let sX = sumSomeLists([xList]);
  let sXX = sumSomeLists([xList, xList]);
  let sY = sumSomeLists([yList]);
  let sXY = sumSomeLists([xList, yList]);
  let size = xList |> List.length |> ReFloat.ofInt;

  let delta = sXX *. size -. sX *. sX;
  let delta1 = sXY *. size -. sX *. sY;
  let delta2 = sXX *. sY -. sX *. sXY;

  let a = delta1 /. delta;
  let b = delta2 /. delta;

  [a, b];
};

let linearCount = (params, x) => {
  switch (params) {
  | [a, b, ..._] => x *. a +. b
  | [a] => x *. a
  | [] => x
  };
};

let squarePolinomialApproximation = (xList, yList) => {
  let sX = sumSomeLists([xList]);
  let sX2 = sumSomeLists([xList, xList]);
  let sX3 = sumSomeLists([xList, xList, xList]);
  let sX4 = sumSomeLists([xList, xList, xList, xList]);
  let sY = sumSomeLists([yList]);
  let sXY = sumSomeLists([xList, yList]);
  let sX2Y = sumSomeLists([xList, xList, yList]);
  let size = xList |> List.length |> ReFloat.ofInt;

  let lhs =
    Mat.of_array([|size, sX, sX2, sX, sX2, sX3, sX2, sX3, sX4|], 3, 3);
  let rhs = Mat.of_array([|sY, sXY, sX2Y|], 3, 1);

  let solution = Linalg.D.linsolve(lhs, rhs);

  [
    Owl_dense_matrix_d.get(solution, 2, 0),
    Owl_dense_matrix_d.get(solution, 1, 0),
    Owl_dense_matrix_d.get(solution, 0, 0),
  ];
};

let squareCount = (params, x) => {
  switch (params) {
  | [a] => x *. x *. a
  | [a, ...rest] => x *. x *. a +. linearCount(rest, x)
  | [] => x *. x
  };
};

let exponentialApproximation = (xList, yList) => {
  let yLogList = yList |> List.map(Float.log);
  switch (linearApproximation(xList, yLogList)) {
  | [a, b, ..._] => [Float.exp(b), a]
  | _ => [1., 0.]
  };
};

let exponentialCount = (params, x) => {
  switch (params) {
  | [a, b, ..._] => (x *. b |> Float.exp) *. a
  | [a] => (x |> Float.exp) *. a
  | [] => x |> Float.exp
  };
};

let logarithmicApproximation = (xList, yList) => {
  let xLogList = xList |> List.map(Float.log);
  linearApproximation(xLogList, yList);
};

let logarithmicCount = (params, x) => {
  switch (params) {
  | [a, b, ..._] => (x |> Float.log) *. a +. b
  | [a] => (x |> Float.log) *. a
  | [] => x |> Float.log
  };
};

let powerFunctionApproximation = (xList, yList) => {
  let yLogList = yList |> List.map(Float.log);
  let xLogList = xList |> List.map(Float.log);
  switch (linearApproximation(xLogList, yLogList)) {
  | [a, b] => [Float.exp(b), a]
  | _ => [0., 0.]
  };
};

let powerCount = (params, x) => {
  switch (params) {
  | [a, b, ..._] => (b |> Float.pow(x)) *. a
  | [a] => x *. a
  | [] => x
  };
};

let applyApproximation =
    (
      approximation: (list(float), list(float)) => list(float),
      counter: (list(float), float) => float,
      xApprList,
      yApprList,
      xWantedList,
    ) => {
  let params = approximation(xApprList, yApprList);

  let rec aux = (lst, acc) => {
    switch (lst) {
    | [hd, ...tl] => aux(tl, [(hd, counter(params, hd)), ...acc])
    | [] => List.rev(acc)
    };
  };

  aux(xWantedList, []);
};
