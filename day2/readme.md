# Solution approach

Invalid IDs form arithmetic sequences and  we compute intersections of these sequences with input ranges and sum with the standard 12th standard formulas :)

## HardCaml implementation

The testbench precomputes intersection parameters for each pattern and ranger. The hardware receives lower, upper, step, n and computes the arithmetic sequence sum combinationally - three 64-bit multipliers, one right shift, and one adder. One cycle per pair.
