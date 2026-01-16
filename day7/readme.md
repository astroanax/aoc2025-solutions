# Solution approach

This is a beam simulation with splitter graph. Beam starts at S going down, hits ^ splitters which split it left and right. In part 1, we count unique splitters hit, and in part 2 count total timelines using memoized DFS where each split creates two branches.

## Hardcaml implementation

Uses bottom-up dynamic programming instead of recursive DFS. Since beams only travel downward, splitters can be processed in reverse row order (bottom to top) and each splitter's timeline count depends only on splitters below it.

The hardware stores splitters in RAM sorted by row. Processing starts from the highest-row splitter and works backward. for each splitter at r, c, hardware searches forward in the list for the first splitter with row > r in columns c-1 and c+1, reads their memoized timeline counts, and computes current = left + right.

State machine does
1. Loading: receive splitter positions from testbench
2. DP loop: for each splitter (bottom to top), search left/right children and compute memo
3. Find entry: locate first splitter in start column
4. Read result: output memoized timeline count
