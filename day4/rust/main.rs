// Day 4: printing department - paper roll accessibility using worklist algorithm

use std::io::{self, Read};

const DIRS: [(i32, i32); 8] = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)];

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    
    let grid: Vec<&[u8]> = input.lines().filter(|l| !l.is_empty()).map(|l| l.as_bytes()).collect();
    let rows = grid.len() as i32;
    let cols = if rows > 0 { grid[0].len() as i32 } else { 0 };
    
    let mut neighbor_count: Vec<Vec<u8>> = vec![vec![0; cols as usize]; rows as usize];
    let mut is_roll: Vec<Vec<bool>> = vec![vec![false; cols as usize]; rows as usize];
    
    for r in 0..rows {
        for c in 0..cols {
            if grid[r as usize][c as usize] == b'@' {
                is_roll[r as usize][c as usize] = true;
                let mut count = 0u8;
                for (dr, dc) in DIRS {
                    let nr = r + dr;
                    let nc = c + dc;
                    if nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr as usize][nc as usize] == b'@' {
                        count += 1;
                    }
                }
                neighbor_count[r as usize][c as usize] = count;
            }
        }
    }
    
    let mut worklist: Vec<(usize, usize)> = Vec::new();
    for r in 0..rows as usize {
        for c in 0..cols as usize {
            if is_roll[r][c] && neighbor_count[r][c] < 4 {
                worklist.push((r, c));
            }
        }
    }
    
    let part1 = worklist.len();
    let mut removed = 0;
    
    while let Some((r, c)) = worklist.pop() {
        if !is_roll[r][c] { continue; }
        is_roll[r][c] = false;
        removed += 1;
        
        for (dr, dc) in DIRS {
            let nr = r as i32 + dr;
            let nc = c as i32 + dc;
            if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
                let (nr, nc) = (nr as usize, nc as usize);
                if is_roll[nr][nc] {
                    if neighbor_count[nr][nc] == 4 { worklist.push((nr, nc)); }
                    neighbor_count[nr][nc] -= 1;
                }
            }
        }
    }
    
    println!("{}", part1);
    println!("{}", removed);
}
