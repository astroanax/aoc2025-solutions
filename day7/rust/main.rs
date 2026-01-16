// tachyon beam splitter sim
// beam goes down, ^ splits left and right
// part2 counts timelines using dp on splitter graph

use std::io::{self, Read};
use std::collections::HashMap;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    
    let grid: Vec<&[u8]> = input.lines()
        .filter(|l| !l.is_empty())
        .map(|l| l.as_bytes())
        .collect();
    
    let rows = grid.len();
    let cols = if rows > 0 { grid[0].len() } else { 0 };
    
    let mut start_col = 0;
    for r in 0..rows {
        for c in 0..cols {
            if grid[r][c] == b'S' { start_col = c; }
        }
    }
    
    // part1: count unique splitters hit
    fn count_splitters(
        start_row: usize, col: usize, rows: usize, cols: usize,
        grid: &[&[u8]], visited: &mut HashMap<(usize, usize), bool>, count: &mut u64,
    ) {
        for r in (start_row + 1)..rows {
            if grid[r][col] == b'^' {
                if !visited.contains_key(&(r, col)) {
                    visited.insert((r, col), true);
                    *count += 1;
                    if col > 0 { count_splitters(r, col - 1, rows, cols, grid, visited, count); }
                    if col + 1 < cols { count_splitters(r, col + 1, rows, cols, grid, visited, count); }
                }
                return;
            }
        }
    }
    
    // part2: count timelines with memoization
    // beam exits = 1 timeline, beam splits = sum of left + right timelines
    fn count_timelines(
        start_row: usize, col: usize, rows: usize, cols: usize,
        grid: &[&[u8]], memo: &mut HashMap<(usize, usize), u64>,
    ) -> u64 {
        for r in (start_row + 1)..rows {
            if grid[r][col] == b'^' {
                if let Some(&cached) = memo.get(&(r, col)) { return cached; }
                let mut total = 0;
                if col > 0 { total += count_timelines(r, col - 1, rows, cols, grid, memo); }
                if col + 1 < cols { total += count_timelines(r, col + 1, rows, cols, grid, memo); }
                memo.insert((r, col), total);
                return total;
            }
        }
        1
    }
    
    let mut visited: HashMap<(usize, usize), bool> = HashMap::new();
    let mut part1 = 0;
    count_splitters(0, start_col, rows, cols, &grid, &mut visited, &mut part1);
    
    let mut memo: HashMap<(usize, usize), u64> = HashMap::new();
    let part2 = count_timelines(0, start_col, rows, cols, &grid, &mut memo);
    
    println!("{}", part1);
    println!("{}", part2);
}
