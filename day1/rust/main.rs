use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    
    let instructions: Vec<(char, i32)> = input
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| {
            let line = line.trim();
            // first char is direction, rest is the number
            let dir = line.chars().next().unwrap();
            let dist = line[1..].parse().unwrap();
            (dir, dist)
        })
        .collect();
    
    let part1 = solve_part1(&instructions);
    
    let part2 = solve_part2(&instructions);
    
    println!("{}", part1);
    println!("{}", part2);
}

fn solve_part1(instructions: &[(char, i32)]) -> i32 {
    let mut pos = 50; // starting position
    let mut zeros = 0;
    
    for &(dir, dist) in instructions {
        pos = match dir {
            'R' => (pos + dist) % 100,
            'L' => {
                let new_pos = pos - dist;
                // keep adding 100 until we're positive
                ((new_pos % 100) + 100) % 100
            }
            _ => pos,
        };
        
        if pos == 0 {
            zeros += 1;
        }
    }
    
    zeros
}

fn solve_part2(instructions: &[(char, i32)]) -> i32 {
    let mut pos = 50;
    let mut zero_hits = 0;
    
    for &(dir, dist) in instructions {
        match dir {
            'R' => {
                // moving right
                let next = pos + dist;
                zero_hits += next / 100;
                pos = next % 100;
            }
            'L' => {
                // moving left
                if pos == 0 {
                    zero_hits += dist / 100;
                    pos = (100 - (dist % 100)) % 100;
                } else if dist >= pos {
                    // wrap through 0
                    let after_zero = dist - pos;
                    zero_hits += 1 + (after_zero / 100);
                    let remainder = after_zero % 100;
                    pos = if remainder == 0 { 0 } else { 100 - remainder };
                } else {
                    // move backwards
                    pos -= dist;
                }
            }
            _ => {}
        }
    }
    
    zero_hits
}
