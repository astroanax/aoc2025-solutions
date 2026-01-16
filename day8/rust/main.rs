// junction box circuits - connect closest pairs, find largest circuits
// part 1: multiply top 3 circuit sizes after 1000 connections
// part 2: connect until all in one circuit, multiply x coords of last edge

use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    
    let points: Vec<(i64, i64, i64)> = input.lines()
        .filter(|l| !l.is_empty())
        .map(|l| {
            let nums: Vec<i64> = l.split(',').map(|s| s.trim().parse().unwrap()).collect();
            (nums[0], nums[1], nums[2])
        })
        .collect();
    
    let n = points.len();
    
    // compute all pairwise squared distances with indices
    // we use squared distance to avoid sqrt, ordering is preserved
    let mut edges: Vec<(i64, usize, usize)> = Vec::new();
    for i in 0..n {
        for j in (i+1)..n {
            let dx = points[i].0 - points[j].0;
            let dy = points[i].1 - points[j].1;
            let dz = points[i].2 - points[j].2;
            let dist_sq = dx*dx + dy*dy + dz*dz;
            edges.push((dist_sq, i, j));
        }
    }
    
    // sort by distance
    edges.sort_by_key(|e| e.0);
    
    // union-find with path compression and union by rank
    let mut parent: Vec<usize> = (0..n).collect();
    let mut rank: Vec<usize> = vec![0; n];
    let mut size: Vec<usize> = vec![1; n];
    let mut num_components = n;
    
    fn find(parent: &mut [usize], x: usize) -> usize {
        if parent[x] != x {
            parent[x] = find(parent, parent[x]);
        }
        parent[x]
    }
    
    fn union(parent: &mut [usize], rank: &mut [usize], size: &mut [usize], x: usize, y: usize) -> bool {
        let px = find(parent, x);
        let py = find(parent, y);
        if px == py { return false; }
        if rank[px] < rank[py] {
            parent[px] = py;
            size[py] += size[px];
        } else if rank[px] > rank[py] {
            parent[py] = px;
            size[px] += size[py];
        } else {
            parent[py] = px;
            size[px] += size[py];
            rank[px] += 1;
        }
        true
    }
    
    let mut part1 = 0;
    let mut part2 = 0;
    let mut connections_made = 0;
    
    for &(_, i, j) in edges.iter() {
        if union(&mut parent, &mut rank, &mut size, i, j) {
            num_components -= 1;
        }
        
        connections_made += 1;
        
        // part 1: after 1000 connection attempts
        if connections_made == 1000 {
            let mut circuit_sizes: Vec<usize> = (0..n)
                .filter(|&k| find(&mut parent, k) == k)
                .map(|k| size[k])
                .collect();
            circuit_sizes.sort_by(|a, b| b.cmp(a));
            part1 = circuit_sizes.iter().take(3).product();
        }
        
        // part 2: last edge that makes all one component
        if num_components == 1 {
            part2 = points[i].0 * points[j].0;
            break;
        }
    }
    
    println!("{}", part1);
    println!("{}", part2);
}
