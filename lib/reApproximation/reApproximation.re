// open ReUtils;
open Owl;

let hello = () => {
  print_endline("Hello!");
};

let sumSomeLists = list => {
  let rec mulAux = (list, acc) => {
    switch (list) {
    | [[hhd, ..._], ...tl] => mulAux(tl, Float.mul(acc, hhd))
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
    | [_, ..._] =>
      Float.add(mulAux(list, 1.0), acc) |> countAux(cutAux(list))
    | _ => acc
    };
  };

  countAux(list, 0.0);
};

let pearsonC = (xList, yList) => {
  let xAvg =
    xList |> List.length |> Float.of_int |> Float.div(sumSomeLists([xList]));
  let yAvg =
    yList |> List.length |> Float.of_int |> Float.div(sumSomeLists([yList]));

  let rec aux = (xList, yList, topSum, botSum1, botSum2) => {
    switch (xList, yList) {
    | ([xhd, ...xtl], [yhd, ...ytl]) =>
      let xSub = Float.sub(xhd, xAvg);
      let ySub = Float.sub(yhd, yAvg);
      aux(
        xtl,
        ytl,
        Float.mul(xSub, ySub) |> Float.add(topSum),
        Float.mul(xSub, xSub) |> Float.add(botSum1),
        Float.mul(ySub, ySub) |> Float.add(botSum2),
      );
    | (_, _) => (topSum, botSum1, botSum2)
    };
  };

  let (topSum, botSum1, botSum2) = aux(xList, yList, 0.0, 0.0, 0.0);
  Float.sqrt(Float.mul(botSum1, botSum2)) |> Float.div(topSum);
};

let linearApproximation = (xList, yList) => {
  let sX = sumSomeLists([xList]);
  let sXX = sumSomeLists([xList, xList]);
  let sY = sumSomeLists([yList]);
  let sXY = sumSomeLists([xList, yList]);
  let size = xList |> List.length |> Float.of_int;

  let delta = Float.sub(Float.mul(sXX, size), Float.mul(sX, sX));
  let delta1 = Float.sub(Float.mul(sXY, size), Float.mul(sX, sY));
  let delta2 = Float.sub(Float.mul(sXX, sY), Float.mul(sX, sXY));

  let a = Float.div(delta1, delta);
  let b = Float.div(delta2, delta);

  (a, b);
};

let squarePolinomialApproximation = (xList, yList) => {
  let sX = sumSomeLists([xList]);
  let sX2 = sumSomeLists([xList, xList]);
  let sX3 = sumSomeLists([xList, xList, xList]);
  let sX4 = sumSomeLists([xList, xList, xList, xList]);
  let sY = sumSomeLists([yList]);
  let sXY = sumSomeLists([xList, yList]);
  let sX2Y = sumSomeLists([xList, xList, yList]);
  let size = xList |> List.length |> Float.of_int;

  let lhs =
    Mat.of_array([|size, sX, sX2, sX, sX2, sX3, sX2, sX3, sX4|], 3, 3);
  let rhs = Mat.of_array([|sY, sXY, sX2Y|], 3, 1);

  let solution = Linalg.D.linsolve(lhs, rhs);

  (
    Owl_dense_matrix_d.get(solution, 2, 0),
    Owl_dense_matrix_d.get(solution, 0, 0),
    Owl_dense_matrix_d.get(solution, 1, 0),
  );
};

let exponentialApproximation = (xList, yList) => {
  let yLogList = yList |> List.map(Float.log);
  let (a, b) = linearApproximation(xList, yLogList);
  (Float.exp(b), a);
};

let logarithmicApproximation = (xList, yList) => {
  let xLogList = xList |> List.map(Float.log);
  linearApproximation(xLogList, yList);
};

let powerFunctionApproximation = (xList, yList) => {
  let yLogList = yList |> List.map(Float.log);
  let xLogList = xList |> List.map(Float.log);
  let (a, b) = linearApproximation(xLogList, yLogList);
  (Float.exp(b), a);
};
