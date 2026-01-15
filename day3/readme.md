# Solution approach

A greedy bubble sort approach is uses - we start with the rightmost N digits, then scan from left to right, swappin any larger digit into position.

## Hardcaml implementation

We use a parallel tree based max finder with O(log n) comparison depth. All comparisons at each tree level execute simultaneously, and the testbench streams digits to the hardware, which stores them and runs N selection cycles per bank.
