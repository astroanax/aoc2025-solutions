// Day 3: lobby - battery bank selection using bubble sort approach

use std::io::{self, Read};

fn max_joltage(bank: &str) -> u64 {
    let bytes = bank.as_bytes();
    let n = bytes.len();
    if n < 2 { return 0; }
    
    let mut batteries = [bytes[n - 2], bytes[n - 1]];
    
    for i in (0..n-2).rev() {
        let mut next = bytes[i];
        for j in 0..2 {
            if next < batteries[j] { break; }
            std::mem::swap(&mut next, &mut batteries[j]);
        }
    }
    
    let tens = (batteries[0] - b'0') as u64;
    let units = (batteries[1] - b'0') as u64;
    tens * 10 + units
}

fn part1(input: &str) -> u64 {
    input.lines().map(max_joltage).sum()
}

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let result = part1(&input);
    println!("{}", result);
}
