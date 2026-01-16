(* trash compactor - hardware parses grid, extracts numbers, applies operators *)

open! Core
open! Hardcaml
open! Signal

let result_bits = 64
let max_grid_size = 32768  (* max bytes in grid - 5 rows * 4096 cols *)
let grid_addr_bits = 15
let max_width = 4096 [@@warning "-32"]
let max_height = 32 [@@warning "-32"]
let coord_bits = 12

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; grid_byte : 'a [@bits 8]
    ; grid_valid : 'a
    ; grid_done : 'a
    ; width : 'a [@bits coord_bits]
    ; height : 'a [@bits coord_bits]  (* not counting operator row *)
    ; is_part2 : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { result : 'a [@bits result_bits]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Load_grid
    | Find_block
    | Read_op
    | Init_scan
    | Parse_number
    | Apply_op
    | Next_item
    | Block_done
    | Finish
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope (i : _ I.t) : _ O.t =
  let _ = scope in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  
  (* grid ram *)
  let grid_we = Variable.wire ~default:gnd () in
  let grid_waddr = Variable.wire ~default:(zero grid_addr_bits) () in
  let grid_wdata = Variable.wire ~default:(zero 8) () in
  let grid_raddr = Variable.wire ~default:(zero grid_addr_bits) () in
  let grid_ram = Ram.create ~name:"grid" ~collision_mode:Read_before_write ~size:max_grid_size
    ~write_ports:[|{ write_clock = i.clock; write_address = grid_waddr.value
                   ; write_enable = grid_we.value; write_data = grid_wdata.value }|]
    ~read_ports:[|{ read_clock = i.clock; read_address = grid_raddr.value; read_enable = vdd }|] ()
  in
  let grid_byte = grid_ram.(0) in
  
  (* state *)
  let%hw_var grid_size = Variable.reg spec ~width:grid_addr_bits in
  let%hw_var width_reg = Variable.reg spec ~width:coord_bits in
  let%hw_var height_reg = Variable.reg spec ~width:coord_bits in
  let%hw_var is_part2_reg = Variable.reg spec ~width:1 in
  
  let%hw_var block_left = Variable.reg spec ~width:coord_bits in
  let%hw_var block_right = Variable.reg spec ~width:coord_bits in
  let%hw_var scan_col = Variable.reg spec ~width:coord_bits in  (* used differently for part1 vs part2 *)
  let%hw_var scan_row = Variable.reg spec ~width:coord_bits in
  let%hw_var current_num = Variable.reg spec ~width:result_bits in
  let%hw_var is_multiply = Variable.reg spec ~width:1 in
  let%hw_var first_in_block = Variable.reg spec ~width:1 in
  
  let%hw_var block_acc = Variable.reg spec ~width:result_bits in
  let%hw_var grand_total = Variable.reg spec ~width:result_bits in
  let%hw_var done_flag = Variable.reg spec ~width:1 in
  
  (* address computation: grid[y][x] at address y * width + x. operator row is at y = height *)
  let addr_of ~x ~y = 
    let prod = uresize y ~width:grid_addr_bits *: uresize width_reg.value ~width:grid_addr_bits in
    sel_bottom (prod +: uresize x ~width:(width prod)) ~width:grid_addr_bits
  in
  
  let is_digit b = (b >=:. 0x30) &: (b <=:. 0x39) in
  let digit_val b = uresize (b -:. 0x30) ~width:result_bits in
  
  compile [
    sm.switch [
      (Idle, [
        when_ i.start [
          grid_size <--. 0;
          grand_total <--. 0;
          done_flag <--. 0;
          sm.set_next Load_grid;
        ];
      ]);
      
      (Load_grid, [
        when_ i.grid_valid [
          grid_we <-- vdd;
          grid_waddr <-- grid_size.value;
          grid_wdata <-- i.grid_byte;
          grid_size <-- grid_size.value +:. 1;
        ];
        when_ i.grid_done [
          width_reg <-- i.width;
          height_reg <-- i.height;
          is_part2_reg <-- i.is_part2;
          block_right <-- i.width;
          sm.set_next Find_block;
        ];
      ]);
      
      (* scan right-to-left on operator row to find block boundaries *)
      (Find_block, [
        if_ (block_right.value ==:. 0) [
          done_flag <--. 1;
          sm.set_next Finish;
        ] [
          (* read operator at (block_right-1, height) - scan backwards *)
          block_left <-- block_right.value -:. 1;
          grid_raddr <-- addr_of ~x:(block_right.value -:. 1) ~y:height_reg.value;
          sm.set_next Read_op;
        ];
      ]);
      
      (Read_op, [
        (* wait for ram, then check if this is an operator *)
        let is_plus = grid_byte ==:. 0x2B in  (* '+' *)
        let is_mult = grid_byte ==:. 0x2A in  (* '*' *)
        if_ (is_plus |: is_mult) [
          is_multiply <-- is_mult;
          block_acc <--. 0;
          first_in_block <--. 1;
          sm.set_next Init_scan;
        ] [
          (* not an operator, keep scanning left *)
          if_ (block_left.value ==:. 0) [
            done_flag <--. 1;
            sm.set_next Finish;
          ] [
            block_left <-- block_left.value -:. 1;
            grid_raddr <-- addr_of ~x:(block_left.value -:. 1) ~y:height_reg.value;
          ];
        ];
      ]);
      
      (* initialize scan based on part1/part2 *)
      (Init_scan, [
        current_num <--. 0;
        if_ is_part2_reg.value [
          (* part2: scan columns from left to right, rows top to bottom within column *)
          scan_col <-- block_left.value;
          scan_row <--. 0;
        ] [
          (* part1: scan rows from top to bottom, columns left to right within row *)
          scan_row <--. 0;
          scan_col <-- block_left.value;
        ];
        grid_raddr <-- addr_of ~x:block_left.value ~y:(zero coord_bits);
        sm.set_next Parse_number;
      ]);
      
      (Parse_number, [
        (* check if we're done with current item (row for part1, column for part2) *)
        let done_with_item = mux2 is_part2_reg.value
          (scan_row.value >=: height_reg.value)
          (scan_col.value >=: block_right.value) in
        let next_num = sel_bottom (current_num.value *: of_int_trunc ~width:result_bits 10) ~width:result_bits +: digit_val grid_byte in
        if_ done_with_item [
          sm.set_next Apply_op;
        ] @@ elif (is_digit grid_byte) [
          current_num <-- next_num;
          (* advance to next position *)
          if_ is_part2_reg.value [
            scan_row <-- scan_row.value +:. 1;
            grid_raddr <-- addr_of ~x:scan_col.value ~y:(scan_row.value +:. 1);
          ] [
            scan_col <-- scan_col.value +:. 1;
            grid_raddr <-- addr_of ~x:(scan_col.value +:. 1) ~y:scan_row.value;
          ];
        ] [
          (* space - skip *)
          if_ is_part2_reg.value [
            scan_row <-- scan_row.value +:. 1;
            grid_raddr <-- addr_of ~x:scan_col.value ~y:(scan_row.value +:. 1);
          ] [
            scan_col <-- scan_col.value +:. 1;
            grid_raddr <-- addr_of ~x:(scan_col.value +:. 1) ~y:scan_row.value;
          ];
        ];
      ]);
      
      (Apply_op, [
        if_ first_in_block.value [
          block_acc <-- current_num.value;
          first_in_block <--. 0;
        ] @@ elif is_multiply.value [
          block_acc <-- sel_bottom (block_acc.value *: current_num.value) ~width:result_bits;
        ] [
          block_acc <-- block_acc.value +: current_num.value;
        ];
        current_num <--. 0;
        sm.set_next Next_item;
      ]);
      
      (Next_item, [
        (* move to next row (part1) or column (part2) *)
        if_ is_part2_reg.value [
          scan_col <-- scan_col.value +:. 1;
          if_ (scan_col.value +:. 1 >=: block_right.value) [
            sm.set_next Block_done;
          ] [
            scan_row <--. 0;
            grid_raddr <-- addr_of ~x:(scan_col.value +:. 1) ~y:(zero coord_bits);
            sm.set_next Parse_number;
          ];
        ] [
          scan_row <-- scan_row.value +:. 1;
          if_ (scan_row.value +:. 1 >=: height_reg.value) [
            sm.set_next Block_done;
          ] [
            scan_col <-- block_left.value;
            grid_raddr <-- addr_of ~x:block_left.value ~y:(scan_row.value +:. 1);
            sm.set_next Parse_number;
          ];
        ];
      ]);
      
      (Block_done, [
        grand_total <-- grand_total.value +: block_acc.value;
        if_ (block_left.value ==:. 0) [
          (* last block processed, we're done *)
          done_flag <--. 1;
          sm.set_next Finish;
        ] [
          block_right <-- block_left.value -:. 1;
          sm.set_next Find_block;
        ];
      ]);
      
      (Finish, [
        when_ i.start [ sm.set_next Idle ];
      ]);
    ];
  ];
  
  { O.result = grand_total.value; done_ = done_flag.value }

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"math_accumulator" create
