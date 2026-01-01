// Day 2: gift shop - finding invalid product IDs where digit patterns repeat

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    
    let ranges = parse_input(&input);
    let part1 = solve_part1(&ranges);
    let part2 = solve_part2(&ranges);
    
    println!("{}", part1);
    println!("{}", part2);
}

fn parse_input(input: &str) -> Vec<(u64, u64)> {
    input.trim().split(',')
        .map(|range| {
            let parts: Vec<&str> = range.trim().split('-').collect();
            (parts[0].parse().unwrap(), parts[1].parse().unwrap())
        })
        .collect()
}

fn solve_part1(ranges: &[(u64, u64)]) -> u64 {
    let patterns = vec![(2, 1), (4, 2), (6, 3), (8, 4), (10, 5)];
    sum_invalid_ids(&patterns, ranges)
}

fn solve_part2(ranges: &[(u64, u64)]) -> u64 {
    let first = vec![(2, 1), (4, 2), (6, 3), (8, 4), (10, 5)];
    let second = vec![(3, 1), (5, 1), (6, 2), (7, 1), (9, 3), (10, 2)];
    let third = vec![(6, 1), (10, 1)];
    sum_invalid_ids(&first, ranges) + sum_invalid_ids(&second, ranges) - sum_invalid_ids(&third, ranges)
}

fn sum_invalid_ids(patterns: &[(u32, u32)], ranges: &[(u64, u64)]) -> u64 {
    let mut total = 0;
    
    for &(total_digits, pattern_size) in patterns {
        let digits_power = 10_u64.pow(total_digits);
        let size_power = 10_u64.pow(pattern_size);
        let step = (digits_power - 1) / (size_power - 1);
        let start = step * (size_power / 10);
        let end = step * (size_power - 1);
        
        for &(from, to) in ranges {
            let lower = next_multiple_of(from, step).max(start);
            let upper = to.min(end);
            
            if lower <= upper {
                let n = (upper - lower) / step;
                let triangular = n * (n + 1) / 2;
                total += lower * (n + 1) + step * triangular;
            }
        }
    }
    total
}

fn next_multiple_of(n: u64, divisor: u64) -> u64 {
    let remainder = n % divisor;
    if remainder == 0 { n } else { n + (divisor - remainder) }
}
