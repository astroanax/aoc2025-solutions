(* Paper roll accessibility counter using RAM-based grid and worklist. *)

open! Core
open! Hardcaml
open! Signal

let max_rows = 150
let max_cols = 150
let max_cells = max_rows * max_cols
let worklist_capacity = 16384

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; rows : 'a [@bits 8]
    ; cols : 'a [@bits 8]
    ; set_dims : 'a
    ; cell_row : 'a [@bits 8]
    ; cell_col : 'a [@bits 8]
    ; cell_is_roll : 'a [@bits 1]
    ; cell_neighbor_count : 'a [@bits 4]
    ; cell_valid : 'a
    ; finish_load : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { part1 : 'a [@bits 32]
    ; part2 : 'a [@bits 32]
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle | Loading | Process_start | Process_pop_wait | Process_read_current
    | Process_write_current | Process_neighbor_read | Process_neighbor_wait
    | Process_neighbor_write | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let dirs = [(-1,-1); (-1,0); (-1,1); (0,-1); (0,1); (1,-1); (1,0); (1,1)]

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  let idx_bits = Int.ceil_log2 (max_cells + 1) in
  let wl_ptr_bits = Int.ceil_log2 worklist_capacity in
  
  let grid_write_enable = Variable.wire ~default:gnd () in
  let grid_write_addr = Variable.wire ~default:(zero idx_bits) () in
  let grid_write_data = Variable.wire ~default:(zero 5) () in
  let grid_read_addr = Variable.wire ~default:(zero idx_bits) () in
  
  let grid_ram = Ram.create ~name:"grid_ram" ~collision_mode:Read_before_write
    ~size:max_cells
    ~write_ports:[|{ write_clock = i.clock; write_address = grid_write_addr.value
                   ; write_enable = grid_write_enable.value; write_data = grid_write_data.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = grid_read_addr.value; read_enable = vdd }|]
    ()
  in
  let grid_read_data = grid_ram.(0) in
  let grid_is_roll = msb grid_read_data in
  let grid_neighbor_count = sel_bottom grid_read_data ~width:4 in
  
  let wl_write_enable = Variable.wire ~default:gnd () in
  let wl_write_addr = Variable.wire ~default:(zero wl_ptr_bits) () in
  let wl_write_data = Variable.wire ~default:(zero 16) () in
  let wl_read_addr = Variable.wire ~default:(zero wl_ptr_bits) () in
  
  let worklist_ram = Ram.create ~name:"worklist_ram" ~collision_mode:Read_before_write
    ~size:worklist_capacity
    ~write_ports:[|{ write_clock = i.clock; write_address = wl_write_addr.value
                   ; write_enable = wl_write_enable.value; write_data = wl_write_data.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = wl_read_addr.value; read_enable = vdd }|]
    ()
  in
  let wl_read_data = worklist_ram.(0) in
  let wl_read_row = sel_top wl_read_data ~width:8 in
  let wl_read_col = sel_bottom wl_read_data ~width:8 in
  
  let%hw_var num_rows = Variable.reg spec ~width:8 in
  let%hw_var num_cols = Variable.reg spec ~width:8 in
  let%hw_var part1_count = Variable.reg spec ~width:32 in
  let%hw_var part2_count = Variable.reg spec ~width:32 in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  let%hw_var wl_head = Variable.reg spec ~width:wl_ptr_bits in
  let%hw_var wl_tail = Variable.reg spec ~width:wl_ptr_bits in
  let worklist_empty = wl_head.value ==: wl_tail.value in
  let%hw_var saved_wl_addr = Variable.reg spec ~width:wl_ptr_bits in
  let%hw_var current_row = Variable.reg spec ~width:8 in
  let%hw_var current_col = Variable.reg spec ~width:8 in
  let%hw_var neighbor_idx = Variable.reg spec ~width:4 in
  let%hw_var neighbor_linear_idx = Variable.reg spec ~width:idx_bits in
  let%hw_var neighbor_valid = Variable.reg spec ~width:1 in
  let%hw_var neighbor_row_reg = Variable.reg spec ~width:8 in
  let%hw_var neighbor_col_reg = Variable.reg spec ~width:8 in
  
  let to_idx row col = 
    let row_ext = uresize row ~width:idx_bits in
    let cols_ext = uresize num_cols.value ~width:idx_bits in
    let col_ext = uresize col ~width:idx_bits in
    sel_bottom (row_ext *: cols_ext) ~width:idx_bits +: col_ext
  in
  
  let compute_neighbor_coords dir_idx row col =
    let dr_dc_list = List.map dirs ~f:(fun (dr, dc) ->
      (of_int_trunc ~width:9 dr, of_int_trunc ~width:9 dc)) in
    let dr = mux dir_idx (List.map dr_dc_list ~f:fst) in
    let dc = mux dir_idx (List.map dr_dc_list ~f:snd) in
    (uresize row ~width:9 +: dr, uresize col ~width:9 +: dc)
  in
  
  let check_neighbor_valid nr nc =
    let row_valid = (~:(msb nr)) &: (sel_bottom nr ~width:8 <: num_rows.value) in
    let col_valid = (~:(msb nc)) &: (sel_bottom nc ~width:8 <: num_cols.value) in
    row_valid &: col_valid
  in
  
  let load_idx = to_idx i.cell_row i.cell_col in
  let nr, nc = compute_neighbor_coords neighbor_idx.value current_row.value current_col.value in
  let n_valid = check_neighbor_valid nr nc in
  let n_row = sel_bottom nr ~width:8 in
  let n_col = sel_bottom nc ~width:8 in
  let n_idx = to_idx n_row n_col in
  
  compile [
    sm.switch [
      (Idle, [ when_ i.start [
        part1_count <--. 0; part2_count <--. 0; done_flag <--. 0;
        wl_head <--. 0; wl_tail <--. 0; sm.set_next Loading; ]; ]);
      
      (Loading, [
        when_ i.set_dims [ num_rows <-- uresize i.rows ~width:8; num_cols <-- uresize i.cols ~width:8; ];
        when_ i.cell_valid [
          grid_write_enable <-- vdd; grid_write_addr <-- load_idx;
          grid_write_data <-- (i.cell_is_roll @: i.cell_neighbor_count);
          when_ (i.cell_is_roll &: (i.cell_neighbor_count <:. 4)) [
            wl_write_enable <-- vdd; wl_write_addr <-- wl_tail.value;
            wl_write_data <-- (i.cell_row @: i.cell_col);
            wl_tail <-- wl_tail.value +:. 1; part1_count <-- part1_count.value +:. 1; ]; ];
        when_ i.finish_load [ sm.set_next Process_start; ]; ]);
      
      (Process_start, [
        if_ worklist_empty [ done_flag <--. 1; sm.set_next Done; ] [
          saved_wl_addr <-- wl_head.value; wl_read_addr <-- wl_head.value;
          wl_head <-- wl_head.value +:. 1; sm.set_next Process_pop_wait; ]; ]);
      
      (Process_pop_wait, [ wl_read_addr <-- saved_wl_addr.value; sm.set_next Process_read_current; ]);
      
      (Process_read_current, [
        current_row <-- wl_read_row; current_col <-- wl_read_col;
        grid_read_addr <-- to_idx wl_read_row wl_read_col; sm.set_next Process_write_current; ]);
      
      (Process_write_current, [
        grid_read_addr <-- to_idx current_row.value current_col.value;
        if_ grid_is_roll [
          grid_write_enable <-- vdd; grid_write_addr <-- to_idx current_row.value current_col.value;
          grid_write_data <-- (gnd @: grid_neighbor_count);
          part2_count <-- part2_count.value +:. 1; neighbor_idx <--. 0;
          sm.set_next Process_neighbor_read;
        ] [ sm.set_next Process_start; ]; ]);
      
      (Process_neighbor_read, [
        neighbor_valid <-- n_valid; neighbor_row_reg <-- n_row; neighbor_col_reg <-- n_col;
        neighbor_linear_idx <-- n_idx; grid_read_addr <-- n_idx; sm.set_next Process_neighbor_wait; ]);
      
      (Process_neighbor_wait, [ grid_read_addr <-- neighbor_linear_idx.value; sm.set_next Process_neighbor_write; ]);
      
      (Process_neighbor_write, [
        grid_read_addr <-- neighbor_linear_idx.value;
        when_ (neighbor_valid.value &: grid_is_roll) [
          grid_write_enable <-- vdd; grid_write_addr <-- neighbor_linear_idx.value;
          grid_write_data <-- (vdd @: (grid_neighbor_count -:. 1));
          when_ (grid_neighbor_count ==:. 4) [
            wl_write_enable <-- vdd; wl_write_addr <-- wl_tail.value;
            wl_write_data <-- (neighbor_row_reg.value @: neighbor_col_reg.value);
            wl_tail <-- wl_tail.value +:. 1; ]; ];
        if_ (neighbor_idx.value ==:. 7) [ sm.set_next Process_start; ]
        [ neighbor_idx <-- neighbor_idx.value +:. 1; sm.set_next Process_neighbor_read; ]; ]);
      
      (Done, [ when_ i.start [ sm.set_next Idle ]; ]); ]; ];
  
  { O.part1 = part1_count.value; part2 = part2_count.value; result_valid = done_flag.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"roll_counter" create
