# Solution approach

A roll is accessible if it has fewer than 4 adjacent rolls. For Part 2, we use a worklist algorithm. We start with initially accessible rolls, remove them sequentiaally, and add any neighbors that become accessible

## Hardcaml implementation

We used block RAM for grid storage  - 5 bits per cell: 1 `is_roll` + 4 `neighbor_count`;  and worklist. Testbench precomputes neighbor counts. Processing takes 8 cycles per removal to check all neighbors.
