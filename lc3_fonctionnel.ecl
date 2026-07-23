type flag           = int<1>;;
type three_b        = int<4>;;
type imm            = int<5>;;
type pc_offset_6    = int<6>;;
type pc_offset_8    = int<8>;;
type pc_offset_9    = int<9>;;
type pc_offset_11   = int<11>;;
type short          = int<16>;;

type instruction = NOT of three_b * three_b 
    | ADD of three_b * three_b * three_b
    | AND of three_b * three_b * three_b

    | ADD_IMM of three_b * three_b * short
    | AND_IMM of three_b * three_b * short
    
    | LEA of three_b * short
    
    | LD of three_b * short
    | ST of three_b * short
    
    | LDR of three_b * three_b * short
    | STR of three_b * three_b * short
    
    | LDI of three_b * short
    | STI of three_b * short
    
    | BR of flag * flag * flag * short

    | NOP of unit

    | JMP of three_b

    | RET of unit 

    | JSR of short

    | PRINT_REG of three_b

    | PRINT_STR of bytes<1> 

    | PRINT_LINE of unit

    (*
    | JSRR of three_b

    | RTI of unit 

    | TRAP of pc_offset_8
    *)

;;

type value = Bool of bool 
    | Addr of short 
    | Char of char 
    | Const of short
    | Instr of instruction
;;

type etat_f = {
    pc      : short vect<1>;
    nzp     : flag vect<3>;
    regs    : value vect<8>;
    memory  : value vect<200>;
    leds    : bool vect<10>
};;


(*---------------ACCESSEURS----------------*)
let get_f (n, i) = vect_nth(n, int_resize<<32>>(i));;

let set_f (n, i, elem) = vect_update(n, int_resize<<32>>(i), elem);;

let get_const inst : short = 
    match inst with 
    | Const c -> c 
    | Addr a -> a
    | _ -> 0
;;

let get_const_p (n, i) =
    let x = get_f(n, i) in
    get_const(x)
;;

(*---------------ENVIRONNEMENT----------------*)
    
let initial_memory : value vect<200> =
    let empty : value vect<200> = vect_make<<200>>(Instr(NOP())) in
    let m : value vect<200> = vect_update(empty,  0, Instr(AND_IMM(0, 0, 0))) in
    let m : value vect<200> = vect_update(m,      1, Instr(AND_IMM(1, 1, 0))) in
    let m : value vect<200> = vect_update(m,      2, Instr(AND_IMM(2, 2, 0))) in
    let m : value vect<200> = vect_update(m,      3, Instr(AND_IMM(3, 3, 0))) in
    let m : value vect<200> = vect_update(m,      4, Instr(AND_IMM(4, 4, 0))) in
    let m : value vect<200> = vect_update(m,      5, Instr(AND_IMM(5, 5, 0))) in
    let m : value vect<200> = vect_update(m,      6, Instr(AND_IMM(6, 6, 0))) in
    let m : value vect<200> = vect_update(m,      7, Instr(AND_IMM(7, 7, 0))) in

    (*---------------HANOI-------------*)

    let m : value vect<200> = vect_update(m,     10, Instr(LEA(6, 190))) in

    let m : value vect<200> = vect_update(m,     11, Instr(AND_IMM(0, 0, 0))) in
    let m : value vect<200> = vect_update(m,     12, Instr(ADD_IMM(0, 0, 3))) in
    let m : value vect<200> = vect_update(m,     13, Instr(ADD_IMM(6, 6, -3))) in
    let m : value vect<200> = vect_update(m,     14, Instr(STR(0, 6, 0))) in        (*arg0 <- 3*)
    let m : value vect<200> = vect_update(m,     15, Instr(STR(0, 6, 2))) in        (*arg2 <- 3*)
    let m : value vect<200> = vect_update(m,     16, Instr(ADD_IMM(0, 0, -2))) in
    let m : value vect<200> = vect_update(m,     17, Instr(STR(0, 6, 1))) in        (*arg1 <- 1*)
    let m : value vect<200> = vect_update(m,     18, Instr(JSR(19))) in             (*hanoi(3, 1, 3)*)
    let m : value vect<200> = vect_update(m,     19, Instr(ADD_IMM(6, 6, 3))) in    (*pop arg0, arg1, arg2*)

    (* adresse morte : fin du programme *)
    let m : value vect<200> = vect_update(m,     21, Instr(BR(1, 1, 1, 200))) in

    (*Fonction mvt(src, dst)*)

    let m : value vect<200> = vect_update(m,     22, Instr(ADD_IMM(6, 6, -3))) in
    let m : value vect<200> = vect_update(m,     23, Instr(STR(0, 6, 0))) in
    let m : value vect<200> = vect_update(m,     24, Instr(STR(1, 6, 1))) in
    let m : value vect<200> = vect_update(m,     25, Instr(STR(7, 6, 2))) in        (*prologue*)

    let m : value vect<200> = vect_update(m,     26, Instr(LDR(0, 6, 3))) in
    let m : value vect<200> = vect_update(m,     27, Instr(PRINT_REG(0))) in        (*print R0*)
    let m : value vect<200> = vect_update(m,     28, Instr(PRINT_STR(bytes_make(':')))) in  (*print_string ":"*)
    let m : value vect<200> = vect_update(m,     29, Instr(LDR(0, 6, 4))) in
    let m : value vect<200> = vect_update(m,     30, Instr(PRINT_REG(0))) in        (*print R0*)
    let m : value vect<200> = vect_update(m,     31, Instr(PRINT_LINE())) in

    let m : value vect<200> = vect_update(m,     32, Instr(LDR(7, 6, 2))) in
    let m : value vect<200> = vect_update(m,     33, Instr(LDR(1, 6, 1))) in
    let m : value vect<200> = vect_update(m,     34, Instr(LDR(0, 6, 0))) in
    let m : value vect<200> = vect_update(m,     35, Instr(ADD_IMM(6, 6, 3))) in
    let m : value vect<200> = vect_update(m,     36, Instr(RET())) in

    (*Fonction hanoi(nb_disques, src, dst)*)

    let m : value vect<200> = vect_update(m,     37, Instr(ADD_IMM(6, 6, -5))) in
    let m : value vect<200> = vect_update(m,     38, Instr(STR(7, 6, 0))) in
    let m : value vect<200> = vect_update(m,     39, Instr(STR(0, 6, 1))) in
    let m : value vect<200> = vect_update(m,     40, Instr(STR(1, 6, 2))) in
    let m : value vect<200> = vect_update(m,     41, Instr(STR(2, 6, 3))) in
    let m : value vect<200> = vect_update(m,     42, Instr(STR(3, 6, 4))) in

    let m : value vect<200> = vect_update(m,     43, Instr(LDR(0, 6, 5))) in
    let m : value vect<200> = vect_update(m,     44, Instr(AND(0, 0, 0))) in
    let m : value vect<200> = vect_update(m,     45, Instr(BR(1, 1, 0, 20))) in     (*On s'arrête : nb_disques <= 0*)

    let m : value vect<200> = vect_update(m,     46, Instr(ADD_IMM(0, 0, -1))) in   (*R0 <- nb_disques - 1*)

    let m : value vect<200> = vect_update(m,     47, Instr(LDR(1, 6, 6))) in        (*R1 <- src*)
    let m : value vect<200> = vect_update(m,     48, Instr(LDR(2, 6, 7))) in        (*R2 <- dst*)
    let m : value vect<200> = vect_update(m,     49, Instr(ADD(3, 1, 2))) in
    let m : value vect<200> = vect_update(m,     50, Instr(NOT(3, 3))) in
    let m : value vect<200> = vect_update(m,     51, Instr(ADD_IMM(3, 3, 7))) in    (*itm = R3 <- 6 - src - dst*)

    let m : value vect<200> = vect_update(m,     52, Instr(ADD_IMM(6, 6, -3))) in
    let m : value vect<200> = vect_update(m,     53, Instr(STR(0, 6, 0))) in
    let m : value vect<200> = vect_update(m,     54, Instr(STR(1, 6, 1))) in
    let m : value vect<200> = vect_update(m,     55, Instr(STR(3, 6, 2))) in
    let m : value vect<200> = vect_update(m,     56, Instr(JSR(-19))) in             (*hanoi(nb_disques - 1, src, itm)*)

    let m : value vect<200> = vect_update(m,     57, Instr(STR(1, 6, 0))) in
    let m : value vect<200> = vect_update(m,     58, Instr(STR(2, 6, 1))) in
    let m : value vect<200> = vect_update(m,     59, Instr(JSR(-37))) in            (*mvt(src, dst)*)

    let m : value vect<200> = vect_update(m,     60, Instr(STR(0, 6, 0))) in
    let m : value vect<200> = vect_update(m,     61, Instr(STR(3, 6, 1))) in
    let m : value vect<200> = vect_update(m,     62, Instr(STR(2, 6, 2))) in
    let m : value vect<200> = vect_update(m,     63, Instr(JSR(-26))) in            (*hanoi(nb_disques - 1, itm, tgt)*)
    let m : value vect<200> = vect_update(m,     64, Instr(ADD_IMM(6, 6, 3))) in

    let m : value vect<200> = vect_update(m,     65, Instr(LDR(3, 6, 4))) in
    let m : value vect<200> = vect_update(m,     66, Instr(LDR(2, 6, 3))) in        (*prologue - hanoi0*)
    let m : value vect<200> = vect_update(m,     67, Instr(LDR(1, 6, 2))) in
    let m : value vect<200> = vect_update(m,     68, Instr(LDR(0, 6, 1))) in
    let m : value vect<200> = vect_update(m,     69, Instr(LDR(7, 6, 0))) in
    let m : value vect<200> = vect_update(m,     70, Instr(ADD_IMM(6, 6, 5))) in
    let m : value vect<200> = vect_update(m,     71, Instr(RET())) in
    m
;;


let initial_state_f : etat_f = {
    pc = vect_make<<1>>(0);
    nzp = vect_make<<3>>(0);
    regs = vect_make<<8>>(Const(0));
    memory = initial_memory;    
    leds = vect_make<<10>>(false)
};;

let maj_nzp (res, env) : etat_f = 
    {
        env with nzp =
        vect_update(
        vect_update(
        vect_update(env.nzp,
            0, if res < 0 then 1 else 0),
            1, if res = 0 then 1 else 0),
            2, if res > 0 then 1 else 0)
    }
;;

(*---------------DECODAGE----------------*)
let decode_minimalist (env : etat_f) : etat_f = 
    let pc_tmp : short = get_f(env.pc, 0) in

    let size = int_resize<<16>>(vect_size(env.memory)) in 
    if(size <= pc_tmp) then (
        print_string("programme terminé"); 
        print_newline(); 
        env

    ) else (   
        let ir : value = get_f(env.memory, pc_tmp) in   
        let env = {env with pc = set_f(env.pc, 0, pc_tmp + 1)} in

        let env1 = 
        (   
            match ir with 
            | Instr instr -> 
            (
                match instr with 
                | NOT(dst_reg, src_reg) -> 
                    let res = lnot(get_const_p(env.regs, src_reg)) in
                    let env_up = {env with regs = set_f(env.regs, dst_reg, Const(res))} in 
                    maj_nzp(res, env_up)

                | ADD(dst_reg, sr1, sr2) -> 
                    let res = get_const_p(env.regs, sr1) + get_const_p(env.regs, sr2) in   
                    let env_up = {env with regs = set_f(env.regs, dst_reg, Const(res))} in
                    maj_nzp(res, env_up)

                | ADD_IMM(dst_reg, sr1, imm) ->
                    let res = (get_const_p(env.regs, sr1) + imm) in
                    let env_up = {env with regs = set_f(env.regs, dst_reg, Const(res))} in
                    maj_nzp(res, env_up)

                | AND(dst_reg, sr1, sr2) -> 
                    let res = Int.land(get_const_p(env.regs, sr1), get_const_p(env.regs, sr2)) in
                    let env_up = {env with regs = set_f(env.regs, dst_reg, Const(res))} in
                    maj_nzp(res, env_up)

                | AND_IMM(dst_reg, sr1, imm) -> 
                    let res = Int.land(get_const_p(env.regs, sr1), imm) in
                    let env_up = {env with regs = set_f(env.regs, dst_reg, Const(res))} in
                    maj_nzp(res, env_up)

                | LEA(dst_reg, offset) -> 
                    {env with regs = set_f(env.regs, dst_reg, Addr(pc_tmp + offset))}

                | LD(dst_reg, offset) -> 
                    print_string "INSTR: LD"; print_newline();
                    {env with regs = set_f(env.regs, dst_reg, get_f(env.memory, pc_tmp + offset))}

                | ST(src_reg, offset) ->    
                    {env with memory = set_f(env.memory, pc_tmp + offset, get_f(env.regs, src_reg))}

                | LDR(dst_reg, sr1, offset) -> 
                    {env with regs = set_f(env.regs, dst_reg, get_f(env.memory, get_const_p(env.regs, sr1) + offset))}

                | STR(src_reg, sr1, offset) -> 
                    {env with memory = set_f(env.memory, get_const_p(env.regs, sr1) + offset, get_f(env.regs, src_reg))}

                | LDI(dst_reg, offset) -> 
                    let val_1 = get_f(env.memory, pc_tmp + offset) in
                    let const_mem = get_const(val_1) in 
                    let val_2 = get_f(env.memory, const_mem) in
                    {env with regs = set_f(env.regs, dst_reg, val_2)}

                | STI(src_reg, offset) ->   
                    let val_instruction = get_f(env.memory, pc_tmp + offset) in
                    let val = get_const(val_instruction) in
                    {env with memory = set_f(env.memory, val, get_f(env.regs, src_reg))}

                | NOP() -> 
                    env

                | BR(n, z, p, offset) -> 
                    if(get_bit(get_f(env.nzp, 0), 0) && get_bit(n, 0)) 
                        then {env with pc = set_f(env.pc, 0, pc_tmp + offset)}
                    else if(get_bit(get_f(env.nzp, 1), 0) && get_bit(z, 0)) 
                        then {env with pc = set_f(env.pc, 0, pc_tmp + offset)}
                    else if(get_bit(get_f(env.nzp, 2), 0) && get_bit(p, 0)) 
                        then {env with pc = set_f(env.pc, 0, pc_tmp + offset)}
                    else env

                | JMP src_reg -> 
                    {env with pc = set_f(env.pc, 0, get_const_p(env.regs, src_reg))}

                | RET() -> 
                    {env with pc = set_f(env.pc, 0, get_const_p(env.regs, 7))}

                | JSR offset -> 
                    let cur_pc : short = get_f(env.pc, 0) in 
                    let env_up = {env with regs = set_f(env.regs, 7, Const(cur_pc))} in
                    let value : short = int_resize<<16>>(pc_tmp) + int_resize<<16>>(offset) in 
                    {env_up with pc = set_f(env_up.pc, 0, value)}

                | PRINT_REG src_reg -> 
                    print_int (get_const_p(env.regs, src_reg)); 
                    env

                | PRINT_STR str -> 
                    print_bytes(str); 
                    env

                | PRINT_LINE() -> 
                    print_newline(); 
                    env
            ) 

            | Char c -> 
                print_string    ("| [mem "); 
                print_int       (pc_tmp); 
                print_string    ("] : "); 
                print_char      (c);
                print_string    ("                                                 |");
                print_newline   ();
                env

            | Const c -> 
                print_string    ("| [mem "); 
                print_int       (pc_tmp); 
                print_string    ("] : ");
                print_int       (c);
                print_string    ("                                                 |");
                print_newline   ();
                env
            
            | Addr a -> 
                print_string    ("| [mem ");
                print_int       (pc_tmp); 
                print_string    ("] : "); 
                print_int       (a);
                print_string    ("                                                 |");
                print_newline   ();
                env

            | Bool b -> 
                print_string    ("| [mem "); 
                print_int       (pc_tmp); 
                print_string    ("] : "); 
                Bool.print      (b);
                print_string    ("                                                 |");
                print_newline   ();
                env
        
            | _ -> print_string("incohérence rencontré"); print_newline(); env
        ) 
        in env1
    ) 
;;


let chrono() = 
    reg (fun c -> c + 1) init 0
;;


let main() = 
    let compteur = chrono() in
    let state = reg decode_minimalist init initial_state_f in
    if get_f(state.pc, 0) >= int_resize<<16>>(vect_size(state.memory)) 
    then (
            print_string "+-------------+";
            print_newline();
            print_string "| compteur=";
            print_int compteur;
            print_string " |";
            print_newline();
            print_string "+-------------+";
            print_newline(); 
            state.leds
        )
;;