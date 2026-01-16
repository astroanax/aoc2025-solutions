(* beam splitter timeline counter using bottom-up dp. processes splitters from
   bottom row to top, computing timeline count as sum of left and right branches.
   part1 (reachable count) computed by testbench, hardware does the dp. *)

open! Core
open! Hardcaml
open! Signal

let max_splitters = 8192
let splitter_id_bits = 13

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; num_cols : 'a [@bits 8]
    ; start_col : 'a [@bits 8]
    ; set_dims : 'a
    ; splitter_row : 'a [@bits 8]
    ; splitter_col : 'a [@bits 8]
    ; splitter_valid : 'a
    ; finish_load : 'a
    ; reachable_count : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { part1 : 'a [@bits 32]
    ; part2 : 'a [@bits 64]
    ; result_valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Loading
    | Dp_start
    | Dp_read_cur
    | Dp_start_left
    | Dp_search_left
    | Dp_read_left
    | Dp_got_left
    | Dp_start_right
    | Dp_search_right
    | Dp_read_right
    | Dp_got_right
    | Dp_write
    | Find_entry
    | Find_entry_wait
    | Read_result
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  (* splitter ram: (row:8, col:8), stored in row-ascending order *)
  let spl_we = Variable.wire ~default:gnd () in
  let spl_waddr = Variable.wire ~default:(zero splitter_id_bits) () in
  let spl_wdata = Variable.wire ~default:(zero 16) () in
  let spl_raddr = Variable.wire ~default:(zero splitter_id_bits) () in
  
  let spl_ram = Ram.create ~name:"splitters" ~collision_mode:Read_before_write
    ~size:max_splitters
    ~write_ports:[|{ write_clock = i.clock; write_address = spl_waddr.value
                   ; write_enable = spl_we.value; write_data = spl_wdata.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = spl_raddr.value
                  ; read_enable = vdd }|] ()
  in
  let spl_data = spl_ram.(0) in
  let spl_row = sel_top spl_data ~width:8 in
  let spl_col = sel_bottom spl_data ~width:8 in
  
  (* memo ram: splitter index -> timeline count *)
  let memo_we = Variable.wire ~default:gnd () in
  let memo_waddr = Variable.wire ~default:(zero splitter_id_bits) () in
  let memo_wdata = Variable.wire ~default:(zero 64) () in
  let memo_raddr = Variable.wire ~default:(zero splitter_id_bits) () in
  
  let memo_ram = Ram.create ~name:"memo" ~collision_mode:Read_before_write
    ~size:max_splitters
    ~write_ports:[|{ write_clock = i.clock; write_address = memo_waddr.value
                   ; write_enable = memo_we.value; write_data = memo_wdata.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = memo_raddr.value
                  ; read_enable = vdd }|] ()
  in
  let memo_data = memo_ram.(0) in
  
  let%hw_var cols = Variable.reg spec ~width:8 in
  let%hw_var start_c = Variable.reg spec ~width:8 in
  let%hw_var num_spl = Variable.reg spec ~width:splitter_id_bits in
  let%hw_var part1 = Variable.reg spec ~width:32 in
  let%hw_var part2 = Variable.reg spec ~width:64 in
  let%hw_var done_reg = Variable.reg spec ~width:1 in
  
  let%hw_var dp_idx = Variable.reg spec ~width:splitter_id_bits in
  let%hw_var cur_row = Variable.reg spec ~width:8 in
  let%hw_var cur_col = Variable.reg spec ~width:8 in
  let%hw_var search_idx = Variable.reg spec ~width:splitter_id_bits in
  let%hw_var target_col = Variable.reg spec ~width:8 in
  let%hw_var left_val = Variable.reg spec ~width:64 in
  let%hw_var right_val = Variable.reg spec ~width:64 in
  
  compile [
    sm.switch [
      (Idle, [
        when_ i.start [
          part1 <--. 0; part2 <--. 0; done_reg <--. 0;
          num_spl <--. 0;
          sm.set_next Loading;
        ];
      ]);
      
      (Loading, [
        when_ i.set_dims [
          cols <-- i.num_cols; start_c <-- i.start_col;
          part1 <-- i.reachable_count;
        ];
        when_ i.splitter_valid [
          spl_we <-- vdd;
          spl_waddr <-- num_spl.value;
          spl_wdata <-- i.splitter_row @: i.splitter_col;
          num_spl <-- num_spl.value +:. 1;
        ];
        when_ i.finish_load [
          if_ (num_spl.value ==:. 0) [
            part2 <--. 1; done_reg <--. 1;
            sm.set_next Done;
          ] [
            dp_idx <-- num_spl.value -:. 1;
            sm.set_next Dp_start;
          ];
        ];
      ]);
      
      (* start processing splitter at dp_idx *)
      (Dp_start, [
        spl_raddr <-- dp_idx.value;
        sm.set_next Dp_read_cur;
      ]);
      
      (Dp_read_cur, [
        cur_row <-- spl_row;
        cur_col <-- spl_col;
        sm.set_next Dp_start_left;
      ]);
      
      (* search for first splitter below in left column *)
      (Dp_start_left, [
        if_ (cur_col.value ==:. 0) [
          left_val <--. 1;
          sm.set_next Dp_start_right;
        ] [
          target_col <-- cur_col.value -:. 1;
          search_idx <-- dp_idx.value +:. 1;
          left_val <--. 1;
          sm.set_next Dp_search_left;
        ];
      ]);
      
      (Dp_search_left, [
        if_ (search_idx.value >=: num_spl.value) [
          sm.set_next Dp_start_right;
        ] [
          spl_raddr <-- search_idx.value;
          sm.set_next Dp_read_left;
        ];
      ]);
      
      (Dp_read_left, [
        (* wait for spl_ram read *)
        if_ (spl_col ==: target_col.value) [
          memo_raddr <-- search_idx.value;
          sm.set_next Dp_got_left;
        ] [
          search_idx <-- search_idx.value +:. 1;
          sm.set_next Dp_search_left;
        ];
      ]);
      
      (Dp_got_left, [
        (* wait for memo_ram read *)
        left_val <-- memo_data;
        sm.set_next Dp_start_right;
      ]);
      
      (* search for first splitter below in right column *)
      (Dp_start_right, [
        if_ (cur_col.value >=: cols.value -:. 1) [
          right_val <--. 1;
          sm.set_next Dp_write;
        ] [
          target_col <-- cur_col.value +:. 1;
          search_idx <-- dp_idx.value +:. 1;
          right_val <--. 1;
          sm.set_next Dp_search_right;
        ];
      ]);
      
      (Dp_search_right, [
        if_ (search_idx.value >=: num_spl.value) [
          sm.set_next Dp_write;
        ] [
          spl_raddr <-- search_idx.value;
          sm.set_next Dp_read_right;
        ];
      ]);
      
      (Dp_read_right, [
        if_ (spl_col ==: target_col.value) [
          memo_raddr <-- search_idx.value;
          sm.set_next Dp_got_right;
        ] [
          search_idx <-- search_idx.value +:. 1;
          sm.set_next Dp_search_right;
        ];
      ]);
      
      (Dp_got_right, [
        right_val <-- memo_data;
        sm.set_next Dp_write;
      ]);
      
      (* write memo for current splitter *)
      (Dp_write, [
        memo_we <-- vdd;
        memo_waddr <-- dp_idx.value;
        memo_wdata <-- left_val.value +: right_val.value;
        if_ (dp_idx.value ==:. 0) [
          search_idx <--. 0;
          sm.set_next Find_entry;
        ] [
          dp_idx <-- dp_idx.value -:. 1;
          sm.set_next Dp_start;
        ];
      ]);
      
      (* find entry point splitter *)
      (Find_entry, [
        spl_raddr <-- search_idx.value;
        sm.set_next Find_entry_wait;
      ]);
      
      (Find_entry_wait, [
        if_ (search_idx.value >=: num_spl.value) [
          part2 <--. 1; done_reg <--. 1;
          sm.set_next Done;
        ] @@ elif (spl_col ==: start_c.value) [
          memo_raddr <-- search_idx.value;
          sm.set_next Read_result;
        ] [
          search_idx <-- search_idx.value +:. 1;
          spl_raddr <-- search_idx.value +:. 1;
          sm.set_next Find_entry;
        ];
      ]);
      
      (Read_result, [
        part2 <-- memo_data;
        done_reg <--. 1;
        sm.set_next Done;
      ]);
      
      (Done, [ when_ i.start [ sm.set_next Idle ]; ]);
    ];
  ];
  
  { O.part1 = part1.value; part2 = part2.value; result_valid = done_reg.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"beam_splitter" create
