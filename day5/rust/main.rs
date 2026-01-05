// Day 5: cafeteria - ingredient freshness checking with range merging and binary search

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let (ranges_section, ids_section) = input.split_once("\n\n").unwrap();

    let mut ranges: Vec<(u64, u64)> = ranges_section.lines()
        .filter(|l| !l.is_empty())
        .map(|line| {
            let (a, b) = line.split_once('-').unwrap();
            (a.parse().unwrap(), b.parse::<u64>().unwrap() + 1)
        })
        .collect();

    let mut ids: Vec<u64> = ids_section.lines()
        .filter(|l| !l.is_empty())
        .map(|l| l.parse().unwrap())
        .collect();

    ranges.sort_unstable();
    ids.sort_unstable();

    let mut merged: Vec<(u64, u64)> = Vec::new();
    let mut current = (0u64, 0u64);
    
    for (start, end) in ranges {
        if start <= current.1 {
            current.1 = current.1.max(end);
        } else {
            if current.1 > current.0 { merged.push(current); }
            current = (start, end);
        }
    }
    if current.1 > current.0 { merged.push(current); }

    let position = |id: u64| -> usize { ids.binary_search(&id).unwrap_or_else(|e| e) };
    let part1: usize = merged.iter().map(|(start, end)| position(*end) - position(*start)).sum();
    let part2: u64 = merged.iter().map(|(start, end)| end - start).sum();

    println!("{}", part1);
    println!("{}", part2);
}
