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
    edges.sort_by_key(|e| e.0);
    
    let mut parent: Vec<usize> = (0..n).collect();
    let mut rank: Vec<usize> = vec![0; n];
    let mut size: Vec<usize> = vec![1; n];
    
    fn find(parent: &mut [usize], x: usize) -> usize {
        if parent[x] != x { parent[x] = find(parent, parent[x]); }
        parent[x]
    }
    
    fn union(parent: &mut [usize], rank: &mut [usize], size: &mut [usize], x: usize, y: usize) {
        let px = find(parent, x);
        let py = find(parent, y);
        if px == py { return; }
        if rank[px] < rank[py] { parent[px] = py; size[py] += size[px]; }
        else if rank[px] > rank[py] { parent[py] = px; size[px] += size[py]; }
        else { parent[py] = px; size[px] += size[py]; rank[px] += 1; }
    }
    
    // only 10 for the example
    for &(_, i, j) in edges.iter().take(10) {
        union(&mut parent, &mut rank, &mut size, i, j);
    }
    
    let mut circuit_sizes: Vec<usize> = (0..n)
        .filter(|&i| find(&mut parent, i) == i)
        .map(|i| size[i])
        .collect();
    circuit_sizes.sort_by(|a, b| b.cmp(a));
    
    eprintln!("sizes: {:?}", circuit_sizes);
    let result: usize = circuit_sizes.iter().take(3).product();
    println!("{}", result);
}
