ReApproximation.hello();

let firstList = [1.1, 2.3, 3.7, 4.5, 5.4, 6.8, 7.5];
let secondList = [2.73, 5.12, 7.74, 8.91, 10.59, 12.75, 13.43];

let (a, b) =
  ReApproximation.powerFunctionApproximation(firstList, secondList);

print_endline(Float.to_string(a));
print_endline(Float.to_string(b));
// print_endline(Float.to_string(a2));
