# Solution approach

We sort and merge overlapping ranges into non overlapping intervals, then use binary search on sorted ids to count how many fall within any range. For part 2, we just sum merged range lengths.

## Hardcaml implementation

We store merged ranges in registers for parallel access. For each incoming id, all range checks execute simultaneously in one cycle using a tree of OR gates. O(1) latency ftw (i think)
