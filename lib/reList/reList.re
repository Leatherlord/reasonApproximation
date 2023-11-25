let rec foldLeft = (f, acc, lst) =>
  switch (lst) {
  | [] => acc
  | [hd, ...tl] => foldLeft(f, f(acc, hd), tl)
  };

let rec foldRight = (f, lst, acc) =>
  switch (lst) {
  | [] => acc
  | [hd, ...tl] => f(hd, foldRight(f, tl, acc))
  };

let rec exists = check =>
  fun
  | [] => false
  | [hd, ...tl] => check(hd) || exists(check, tl);

let rec find = check =>
  fun
  | [] => raise(Not_found)
  | [hd, ..._] when check(hd) => hd
  | [_, ...tl] => find(check, tl);

let rec reverseAppend = (first, second) =>
  switch (first) {
  | [] => second
  | [hd, ...tl] => reverseAppend(tl, [hd, ...second])
  };

let reverse = lst => reverseAppend(lst, []);

let filter = check => {
  let rec collect = acc =>
    fun
    | [] => reverse(acc)
    | [hd, ...tl] when check(hd) => collect([hd, ...acc], tl)
    | [_, ...tl] => collect(acc, tl);
  collect([]);
};

let rec map = f =>
  fun
  | [] => []
  | [hd, ...tl] => [f(hd), ...map(f, tl)];

let ofSeq = List.of_seq;
