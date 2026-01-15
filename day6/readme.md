# Solution approach

Scan from right for operators on the bottom row. Each operator marks a problem boundary, and for each problem, we extract numbers row wise for part 1 or column-wise for part 2; then apply the operator.

## Hardcaml implementation

The hardware is a simple math accumulator that receives pre parsed numbers from the testbench. It accumulates within each problem using the specified operator, then sums all problem results.

The testbench uses `hardcaml_step_testbench` (from the recent [blog post](https://blog.janestreet.com/fun-with-algebraic-effects-hardcaml/) ) effectful API (`Hardcaml_step_testbench_effectful.Functional.Cyclesim`) which allows writing testbenches with regular for loops instead of monadic style, much cleaner for this kind of streaming
