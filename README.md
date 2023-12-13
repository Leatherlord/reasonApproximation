# ReasonML: Approximation algorithms for stdin|stdout
## Functional Programming course third assignment. 
Recreation of Computational Mathematics assignment: 5 approximations algorithms implemented in ReasonML with basic console UI

# Лабораторная работа №3 по предмету: «Функциональное программирование»

Выполнил:
Бойко В. А.

В рамках данной лабораторной работы мной были реализованы 5 различных аппроксимирующих функций, а так же консольный интерфейс для взаимодействия с приложением и ввода данных для расссчетов.

Код самых интересных функций приведен ниже:

```Reason
/* Basic linear approximation */
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

/* Square polinomial approximation that uses Owl package to solve linear equasion */
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

/* Takes an approximation function and a counter function and applies them to data */
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

/* Generates X values in a certain range with a certain step */
let generateXWanted = (fromX, toX, step) => {
  let amount = (toX -. fromX) /. step +. 1. |> ReFloat.toInt;
  Seq.init(amount, index => (index |> ReFloat.ofInt) *. step +. fromX)
  |> ReList.ofSeq;
};

/* Function that takes a short name of an algorithm and returns partially applied approximation that only needs data now */
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

/* Main routine, takes list of points, converts them to Floats and applies all requested approximation algorithms to them */
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

/* Main reading loop, calling for mainRoutine() while getting new data from stdin */
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
```

Содержимое файла data:
```
1.1,2.73;2.3,5.12;3.7,7.74;4.5,8.91;5.4,10.59;6.8,12.75;7.5,13.43
```

Результаты работы программы:

```
cat data | esy x app -w 7 lin sqr exp log pow

Start entering points in the form of: `<X1>,<Y1>;<X2>,<Y2>;...`
lin:
(1.1,3.07), (1.6,3.91), (2.1,4.76), (2.6,5.6), (3.1,6.44), (3.6,7.28), (4.1,8.13), 
================================================
sqr:
(1.1,2.72), (1.6,3.74), (2.1,4.73), (2.6,5.69), (3.1,6.62), (3.6,7.52), (4.1,8.39), 
================================================
exp:
(1.1,3.53), (1.6,3.97), (2.1,4.47), (2.6,5.03), (3.1,5.65), (3.6,6.35), (4.1,7.14), 
================================================
log:
(1.1,1.74), (1.6,3.85), (2.1,5.39), (2.6,6.6), (3.1,7.59), (3.6,8.44), (4.1,9.17), 
================================================
pow:
(1.1,2.75), (1.6,3.77), (2.1,4.73), (2.6,5.66), (3.1,6.56), (3.6,7.44), (4.1,8.29), 
================================================
lin:
(4.1,8.13), 
================================================
sqr:
(4.1,8.39), 
================================================
exp:
(4.1,7.14), 
================================================
log:
(4.1,9.17), 
================================================
pow:
(4.1,8.29), 
================================================
lin:
(4.1,8.13), (4.6,8.97), (5.1,9.81), (5.6,10.65), (6.1,11.5), (6.6,12.34), (7.1,13.18), 
================================================
sqr:
(4.1,8.39), (4.6,9.24), (5.1,10.05), (5.6,10.83), (6.1,11.59), (6.6,12.31), (7.1,13.01), 
================================================
exp:
(4.1,7.14), (4.6,8.03), (5.1,9.03), (5.6,10.16), (6.1,11.42), (6.6,12.84), (7.1,14.44), 
================================================
log:
(4.1,9.17), (4.6,9.82), (5.1,10.4), (5.6,10.93), (6.1,11.42), (6.6,11.86), (7.1,12.27), 
================================================
pow:
(4.1,8.29), (4.6,9.13), (5.1,9.96), (5.6,10.77), (6.1,11.57), (6.6,12.36), (7.1,13.14), 
================================================
Have a nice day!
```

Результаты, переведенные в Desmos (сравнение алгоритмов с введенными значениями):

- Линейная аппроксимация `(ax + b)`:


![image](https://github.com/Leatherlord/reasonApproximation/assets/70820064/53e294d1-4e53-47fc-8ae1-845487e0fa9d)

- Квадратичный полином `(ax^2 + bx + c)`:


![image](https://github.com/Leatherlord/reasonApproximation/assets/70820064/1c3104c9-5269-4292-9455-811cfdde8ce2)

- Экспонента `(ae^(bx))`:


![image](https://github.com/Leatherlord/reasonApproximation/assets/70820064/b18219d1-a35a-4cf7-be70-5d3d294e1ef8)

- Логарифмическая функция `(aln(x) + b)`:


![image](https://github.com/Leatherlord/reasonApproximation/assets/70820064/517d1214-a550-420e-869f-144e35724a78)

- Степенная функция `(ax^b)`:


![image](https://github.com/Leatherlord/reasonApproximation/assets/70820064/78db1408-903f-4704-93cd-f27d990b32e2)


# Доп. задание

В качестве дополнительного задания мной была реализована "игрушечная" реализация линейной интерполяции:

```Reason
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
```

Результаты ее работы:

```
selfofly@selfofly-with-oblepikha:~/Documents/FunctionalProgramming/reasonApproximation$ cat data | esy x extra

(1.1,2.73),(1.2,2.93),(1.3,3.13),(1.4,3.33),(1.5,3.53),(1.6,3.73),
(3.,6.43),
(4.1,8.32),
(4.95,9.75),
(6.1,11.67),
(7.15,13.09),
(7.2,13.14),(7.3,13.24),(7.4,13.33),(7.5,13.43),
Have a good one!

```

# Заключение

При выполнении этой работы сразу чувствуется, что ты вышел из "зоны комфорта" - постоянно какие-то преобразования в строки, из строк, конкатенации и всякая лабудень. Классический набор детского парсера. И код сразу становится грязнее, когда наполняется такими вещами. Поэтому особое эстетическое удовольствие доставляет то, что работа с математикой полностью отделена от работы с вводом/выводом - в математическом модуле сохраняется чистота и опрятность хорошего функционального кода, и смотреть на него одно удовольствие. В целом, мне не очень понравилось, что язык плохо преспособлен для работы с вводом/выводом - как минимум нет нормального форматированного вывода в стандартной библиотеке. Хотя оно и понятно, ведь это грязная работа, которая в функциональных кругах не особо приветствуется.
