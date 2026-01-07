/* Day 6: trash compactor - math problems arranged in a grid.
   Each problem has numbers stacked vertically with an operator (+, *) at bottom.
   Part 1: parse numbers row-wise. Part 2: parse numbers column-wise. */

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        println!("0");
        println!("0");
        return;
    }

    /* Find operator row (last non-empty row with + or *) */
    let mut op_row_idx = lines.len() - 1;
    while op_row_idx > 0 && lines[op_row_idx].trim().is_empty() {
        op_row_idx -= 1;
    }

    let data_rows = &lines[0..op_row_idx];
    let op_row = lines[op_row_idx];

    if data_rows.is_empty() {
        println!("0");
        println!("0");
        return;
    }

    let width = lines.iter().map(|l| l.len()).max().unwrap_or(0);
    let height = data_rows.len();

    /* Convert to byte grid, padding short lines */
    let grid: Vec<Vec<u8>> = data_rows
        .iter()
        .map(|line| {
            let mut row: Vec<u8> = line.bytes().collect();
            row.resize(width, b' ');
            row
        })
        .collect();

    let op_bytes: Vec<u8> = {
        let mut row: Vec<u8> = op_row.bytes().collect();
        row.resize(width, b' ');
        row
    };

    /* Scan right-to-left; each operator marks a problem boundary */
    let mut part1: u64 = 0;
    let mut part2: u64 = 0;
    let mut right = width;

    for left in (0..width).rev() {
        let op = op_bytes[left];
        if op == b' ' {
            continue;
        }

        let is_plus = op == b'+';

        let row_result: u64 = if is_plus {
            (0..height)
                .map(|y| {
                    (left..right).fold(0u64, |num, x| {
                        let ch = grid[y][x];
                        if ch.is_ascii_digit() {
                            num * 10 + (ch - b'0') as u64
                        } else {
                            num
                        }
                    })
                })
                .sum()
        } else {
            (0..height)
                .map(|y| {
                    (left..right).fold(0u64, |num, x| {
                        let ch = grid[y][x];
                        if ch.is_ascii_digit() {
                            num * 10 + (ch - b'0') as u64
                        } else {
                            num
                        }
                    })
                })
                .product()
        };

        let col_result: u64 = if is_plus {
            (left..right)
                .map(|x| {
                    (0..height).fold(0u64, |num, y| {
                        let ch = grid[y][x];
                        if ch.is_ascii_digit() {
                            num * 10 + (ch - b'0') as u64
                        } else {
                            num
                        }
                    })
                })
                .sum()
        } else {
            (left..right)
                .map(|x| {
                    (0..height).fold(0u64, |num, y| {
                        let ch = grid[y][x];
                        if ch.is_ascii_digit() {
                            num * 10 + (ch - b'0') as u64
                        } else {
                            num
                        }
                    })
                })
                .product()
        };

        part1 += row_result;
        part2 += col_result;

        right = left - 1;
    }

    println!("{}", part1);
    println!("{}", part2);
}
