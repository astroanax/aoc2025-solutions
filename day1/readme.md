# Solution approach

Rather than simulating the dial moving tick-by-tick, we compute the result of each instruction mathematically in a single step. The number of times we cross zero can be calculated directly using division.

For right rotation, the new position is `(pos + dist) mod 100`, and the number of wraps is `floor((pos + dist) / 100)`. For left rotation, we have three cases - starting exactly at 0, crossing 0, or staying above 0.

## Hardcaml implementation

The hardware processes one instruction per clock cycle using fully combinational arithmetic. Since the maximum value is `pos + dist = 99 + 999 = 1098`, we only need to handle quotients 0-11. This is implemented as a chain of 12 comparators feeding into nested muxes, making division happen in a single cycle.
