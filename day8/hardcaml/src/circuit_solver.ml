(* hardware union-find for junction box circuit problem. testbench sorts edges,
   hardware maintains parent/rank/size arrays and processes union operations.
   implements union by rank without path compression for simpler hardware. *)

open! Core
open! Hardcaml
open! Signal

let max_nodes = 2048  (* 2^11, enough for 1001 points *)
let node_bits = 11

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; num_nodes : 'a [@bits node_bits]
    ; set_num_nodes : 'a
    ; edge_a : 'a [@bits node_bits]
    ; edge_b : 'a [@bits node_bits]
    ; edge_x1 : 'a [@bits 32]
    ; edge_x2 : 'a [@bits 32]
    ; edge_valid : 'a
    ; finish : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { part1 : 'a [@bits 64]
    ; part2 : 'a [@bits 64]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Init_arrays
    | Ready
    | Find_a
    | Find_a_chase
    | Find_b
    | Find_b_chase
    | Do_union
    | Write_union
    | Check_done
    | Scan_roots
    | Sort_sizes
    | Multiply_top3
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  (* parent ram *)
  let parent_we = Variable.wire ~default:gnd () in
  let parent_waddr = Variable.wire ~default:(zero node_bits) () in
  let parent_wdata = Variable.wire ~default:(zero node_bits) () in
  let parent_raddr = Variable.wire ~default:(zero node_bits) () in
  
  let parent_ram = Ram.create ~name:"parent" ~collision_mode:Read_before_write
    ~size:max_nodes
    ~write_ports:[|{ write_clock = i.clock; write_address = parent_waddr.value
                   ; write_enable = parent_we.value; write_data = parent_wdata.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = parent_raddr.value
                  ; read_enable = vdd }|] ()
  in
  let parent_data = parent_ram.(0) in
  
  (* rank ram *)
  let rank_we = Variable.wire ~default:gnd () in
  let rank_waddr = Variable.wire ~default:(zero node_bits) () in
  let rank_wdata = Variable.wire ~default:(zero 8) () in
  let rank_raddr = Variable.wire ~default:(zero node_bits) () in
  
  let rank_ram = Ram.create ~name:"rank" ~collision_mode:Read_before_write
    ~size:max_nodes
    ~write_ports:[|{ write_clock = i.clock; write_address = rank_waddr.value
                   ; write_enable = rank_we.value; write_data = rank_wdata.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = rank_raddr.value
                  ; read_enable = vdd }|] ()
  in
  let rank_data = rank_ram.(0) in
  
  (* size ram *)
  let size_we = Variable.wire ~default:gnd () in
  let size_waddr = Variable.wire ~default:(zero node_bits) () in
  let size_wdata = Variable.wire ~default:(zero 16) () in
  let size_raddr = Variable.wire ~default:(zero node_bits) () in
  
  let size_ram = Ram.create ~name:"size" ~collision_mode:Read_before_write
    ~size:max_nodes
    ~write_ports:[|{ write_clock = i.clock; write_address = size_waddr.value
                   ; write_enable = size_we.value; write_data = size_wdata.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = size_raddr.value
                  ; read_enable = vdd }|] ()
  in
  let size_data = size_ram.(0) in
  
  (* registers *)
  let%hw_var n = Variable.reg spec ~width:node_bits in
  let%hw_var num_components = Variable.reg spec ~width:node_bits in
  let%hw_var connections = Variable.reg spec ~width:16 in
  let%hw_var init_idx = Variable.reg spec ~width:node_bits in
  
  let%hw_var edge_node_a = Variable.reg spec ~width:node_bits in
  let%hw_var edge_node_b = Variable.reg spec ~width:node_bits in
  let%hw_var edge_x1_reg = Variable.reg spec ~width:32 in
  let%hw_var edge_x2_reg = Variable.reg spec ~width:32 in
  
  let%hw_var root_a = Variable.reg spec ~width:node_bits in
  let%hw_var root_b = Variable.reg spec ~width:node_bits in
  let%hw_var rank_a = Variable.reg spec ~width:8 in
  let%hw_var rank_b = Variable.reg spec ~width:8 in
  let%hw_var size_a = Variable.reg spec ~width:16 in
  let%hw_var size_b = Variable.reg spec ~width:16 in
  let%hw_var cur_node = Variable.reg spec ~width:node_bits in
  
  let%hw_var part1 = Variable.reg spec ~width:64 in
  let%hw_var part2 = Variable.reg spec ~width:64 in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  let%hw_var part1_computed = Variable.reg spec ~width:1 in
  
  (* for finding top 3 sizes *)
  let%hw_var scan_idx = Variable.reg spec ~width:node_bits in
  let%hw_var top1 = Variable.reg spec ~width:16 in
  let%hw_var top2 = Variable.reg spec ~width:16 in
  let%hw_var top3 = Variable.reg spec ~width:16 in
  
  compile [
    sm.switch [
      (Idle, [
        when_ i.start [
          part1 <--. 0; part2 <--. 0; done_flag <--. 0;
          connections <--. 0; part1_computed <--. 0;
          sm.set_next Init_arrays;
        ];
        when_ i.set_num_nodes [
          n <-- i.num_nodes;
          num_components <-- i.num_nodes;
          init_idx <--. 0;
        ];
      ]);
      
      (* initialize parent[i]=i, rank[i]=0, size[i]=1 *)
      (Init_arrays, [
        parent_we <-- vdd;
        parent_waddr <-- init_idx.value;
        parent_wdata <-- init_idx.value;
        rank_we <-- vdd;
        rank_waddr <-- init_idx.value;
        rank_wdata <--. 0;
        size_we <-- vdd;
        size_waddr <-- init_idx.value;
        size_wdata <--. 1;
        if_ (init_idx.value ==: n.value -:. 1) [
          sm.set_next Ready;
        ] [
          init_idx <-- init_idx.value +:. 1;
        ];
      ]);
      
      (* wait for edge or finish *)
      (Ready, [
        when_ i.edge_valid [
          edge_node_a <-- i.edge_a;
          edge_node_b <-- i.edge_b;
          edge_x1_reg <-- i.edge_x1;
          edge_x2_reg <-- i.edge_x2;
          connections <-- connections.value +:. 1;
          cur_node <-- i.edge_a;
          parent_raddr <-- i.edge_a;
          sm.set_next Find_a;
        ];
        when_ i.finish [
          done_flag <--. 1;
          sm.set_next Done;
        ];
      ]);
      
      (* find root of node a - chase parent pointers *)
      (Find_a, [
        parent_raddr <-- cur_node.value;
        sm.set_next Find_a_chase;
      ]);
      
      (Find_a_chase, [
        if_ (parent_data ==: cur_node.value) [
          (* found root *)
          root_a <-- cur_node.value;
          rank_raddr <-- cur_node.value;
          size_raddr <-- cur_node.value;
          cur_node <-- edge_node_b.value;
          parent_raddr <-- edge_node_b.value;
          sm.set_next Find_b;
        ] [
          cur_node <-- parent_data;
          parent_raddr <-- parent_data;
        ];
      ]);
      
      (* find root of node b *)
      (Find_b, [
        rank_a <-- rank_data;
        size_a <-- size_data;
        parent_raddr <-- cur_node.value;
        sm.set_next Find_b_chase;
      ]);
      
      (Find_b_chase, [
        if_ (parent_data ==: cur_node.value) [
          root_b <-- cur_node.value;
          rank_raddr <-- cur_node.value;
          size_raddr <-- cur_node.value;
          sm.set_next Do_union;
        ] [
          cur_node <-- parent_data;
          parent_raddr <-- parent_data;
        ];
      ]);
      
      (* union the two sets *)
      (Do_union, [
        rank_b <-- rank_data;
        size_b <-- size_data;
        if_ (root_a.value ==: root_b.value) [
          (* already same component *)
          sm.set_next Check_done;
        ] [
          num_components <-- num_components.value -:. 1;
          sm.set_next Write_union;
        ];
      ]);
      
      (* write updated parent/rank/size *)
      (Write_union, [
        if_ (rank_a.value <: rank_b.value) [
          (* attach a under b *)
          parent_we <-- vdd;
          parent_waddr <-- root_a.value;
          parent_wdata <-- root_b.value;
          size_we <-- vdd;
          size_waddr <-- root_b.value;
          size_wdata <-- size_a.value +: size_b.value;
        ] @@ elif (rank_a.value >: rank_b.value) [
          (* attach b under a *)
          parent_we <-- vdd;
          parent_waddr <-- root_b.value;
          parent_wdata <-- root_a.value;
          size_we <-- vdd;
          size_waddr <-- root_a.value;
          size_wdata <-- size_a.value +: size_b.value;
        ] [
          (* equal rank - attach b under a, increase a's rank *)
          parent_we <-- vdd;
          parent_waddr <-- root_b.value;
          parent_wdata <-- root_a.value;
          size_we <-- vdd;
          size_waddr <-- root_a.value;
          size_wdata <-- size_a.value +: size_b.value;
          rank_we <-- vdd;
          rank_waddr <-- root_a.value;
          rank_wdata <-- rank_a.value +:. 1;
        ];
        sm.set_next Check_done;
      ]);
      
      (* check if part1 or part2 conditions met *)
      (Check_done, [
        (* part1: after 1000 connections *)
        when_ ((connections.value ==:. 1000) &: (~:(part1_computed.value))) [
          part1_computed <--. 1;
          scan_idx <--. 0;
          top1 <--. 0; top2 <--. 0; top3 <--. 0;
          sm.set_next Scan_roots;
        ];
        (* part2: when all one component *)
        when_ ((num_components.value ==:. 1) &: (part2.value ==:. 0)) [
          part2 <-- uresize (edge_x1_reg.value *: edge_x2_reg.value) ~width:64;
        ];
        when_ (~:((connections.value ==:. 1000) &: (~:(part1_computed.value)))) [
          sm.set_next Ready;
        ];
      ]);
      
      (* part1: scan to find top 3 component sizes *)
      (Scan_roots, [
        parent_raddr <-- scan_idx.value;
        size_raddr <-- scan_idx.value;
        sm.set_next Sort_sizes;
      ]);
      
      (Sort_sizes, [
        (* check if this is a root (parent[i] == i) *)
        when_ (parent_data ==: scan_idx.value) [
          (* insert size into top 3 *)
          if_ (size_data >: top1.value) [
            top3 <-- top2.value;
            top2 <-- top1.value;
            top1 <-- size_data;
          ] @@ elif (size_data >: top2.value) [
            top3 <-- top2.value;
            top2 <-- size_data;
          ] @@ elif (size_data >: top3.value) [
            top3 <-- size_data;
          ] [];
        ];
        if_ (scan_idx.value ==: n.value -:. 1) [
          sm.set_next Multiply_top3;
        ] [
          scan_idx <-- scan_idx.value +:. 1;
          parent_raddr <-- scan_idx.value +:. 1;
          size_raddr <-- scan_idx.value +:. 1;
        ];
      ]);
      
      (Multiply_top3, [
        (* top1 * top2 * top3, keeping within 64 bits *)
        part1 <-- sel_bottom 
          (uresize (uresize top1.value ~width:32 *: uresize top2.value ~width:32) ~width:64 
           *: uresize top3.value ~width:64) ~width:64;
        sm.set_next Ready;
      ]);
      
      (Done, [ when_ i.start [ sm.set_next Idle ]; ]);
    ];
  ];
  
  { O.part1 = part1.value; part2 = part2.value; done_ = done_flag.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"circuit_solver" create
