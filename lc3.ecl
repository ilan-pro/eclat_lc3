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
    | PRINT_STR of string
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
    leds    : bool vect<10>
};;

type state = DECOD of unit | EXE of unit;;

let memory : value array<1000> = create<1000>();;

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
let initial_state_f : etat_f = {
    pc = vect_make<<1>>(0);
    nzp = vect_make<<3>>(0);
    regs = vect_make<<8>>(Const(0)); 
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

    let (_, _, env') = reg (fun (state, pc_arg, env) -> 
        match state with 
        | DECOD() -> get_start(memory, pc_arg) ; (EXE(), pc_arg, env) 
        | EXE() -> 
            let ir : value = get_end(memory) in
            let env' = 
            (   
                let env = {env with pc = set_f(env.pc, 0, pc_arg + 1)} in
                
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
                        let res = get_const_p(env.regs, sr1) + imm in
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
                        {env with regs = set_f(env.regs, dst_reg, Addr(pc_arg + offset))}

                    | LD(dst_reg, offset) ->
                        {env with regs = set_f(env.regs, dst_reg, get(memory, pc_arg + offset))}

                    | ST(src_reg, offset) ->
                        immediate set(memory, pc_arg + offset, get_f(env.regs, src_reg));
                        env

                    | LDR(dst_reg, sr1, offset) ->
                        {env with regs = set_f(env.regs, dst_reg, get(memory, get_const_p(env.regs, sr1) + offset))}

                    | STR(src_reg, sr1, offset) ->
                        immediate set(memory, get_const_p(env.regs, sr1) + offset, get_f(env.regs, src_reg));
                        env

                    | LDI(dst_reg, offset) ->
                        let val_1 = get(memory, pc_arg + offset) in
                        let const_mem = get_const(val_1) in
                        let val_2 = get(memory, const_mem) in
                        {env with regs = set_f(env.regs, dst_reg, val_2)}

                    | STI(src_reg, offset) ->
                        let val_instruction = get(memory, pc_arg + offset) in
                        let val = get_const(val_instruction) in
                        immediate set(memory, val, get_f(env.regs, src_reg));
                        env

                    | NOP() ->
                        env

                    | BR(n, z, p, offset) ->
                        
                        if (get_bit(get_f(env.nzp, 0), 0) && get_bit(n, 0))
                            then {env with pc = set_f(env.pc, 0, pc_arg + offset)}
                        else if (get_bit(get_f(env.nzp, 1), 0) && get_bit(z, 0))
                            then {env with pc = set_f(env.pc, 0, pc_arg + offset)}
                        else if (get_bit(get_f(env.nzp, 2), 0) && get_bit(p, 0))
                            then {env with pc = set_f(env.pc, 0, pc_arg + offset)}
                        else env

                    | JMP src_reg ->
                        {env with pc = set_f(env.pc, 0, get_const_p(env.regs, src_reg))}

                    | RET() ->
                        {env with pc = set_f(env.pc, 0, get_const_p(env.regs, 7))}

                    | JSR offset ->
                        let cur_pc : short = get_f(env.pc, 0) in
                        let env_up = {env with regs = set_f(env.regs, 7, Const(cur_pc))} in
                        let value : short = int_resize<<16>>(pc_arg) + int_resize<<16>>(offset) in
                        {env_up with pc = set_f(env_up.pc, 0, value)}

                    | PRINT_REG src_reg ->
                        print_int (get_const_p(env.regs, src_reg));
                        env

                    | PRINT_STR str ->
                        print_string str;
                        env

                    | PRINT_LINE() ->
                        print_newline();
                        env
                ) 
                | Char c -> 
                    print_string    ("| [mem "); 
                    print_int       (pc_arg); 
                    print_string    ("] : "); 
                    print_char      (c);
                    print_string    ("                                                 |");
                    print_newline   ();
                    env
                | Const c -> 
                    print_string    ("| [mem "); 
                    print_int       (pc_arg); 
                    print_string    ("] : ");
                    print_int       (c);
                    print_string    ("                                                 |");
                    print_newline   ();
                    env
                | Addr a -> 
                    print_string    ("| [mem ");
                    print_int       (pc_arg); 
                    print_string    ("] : "); 
                    print_int       (a);
                    print_string    ("                                                 |");
                    print_newline   ();
                    env
                | Bool b -> 
                    print_string    ("| [mem "); 
                    print_int       (pc_arg); 
                    print_string    ("] : "); 
                    Bool.print      (b);
                    print_string    ("                                                 |");
                    print_newline   ();
                    env
                | _ -> print_string("incohérence rencontré"); print_newline(); env
            ) in (DECOD(), get_f(env'.pc, 0), env')    
    ) init (DECOD(), pc_tmp, env) 
    in env'
;;

let chrono() = 
    reg (fun c -> c + 1) init 0
;;

let main() = 
    let loading_done : bool = 
        reg (fun last_rdy -> 
            if (last_rdy) 
                then true 
            else 
                let (_, rdy) = exec 
                                set(memory,  0, Instr(AND_IMM(0, 0, 0)));
                                set(memory,  1, Instr(AND_IMM(1, 1, 0)));
                                set(memory,  2, Instr(AND_IMM(2, 2, 0)));
                                set(memory,  3, Instr(AND_IMM(3, 3, 0)));
                                set(memory,  4, Instr(AND_IMM(4, 4, 0)));
                                set(memory,  5, Instr(AND_IMM(5, 5, 0)));
                                set(memory,  6, Instr(AND_IMM(6, 6, 0)));
                                set(memory,  7, Instr(AND_IMM(7, 7, 0)));
                                (*---------------HANOI-------------*)
                                set(memory, 10, Instr(LEA(6, 390)));
                                set(memory, 11, Instr(AND_IMM(0, 0, 0)));
                                set(memory, 12, Instr(ADD_IMM(0, 0, 3)));
                                set(memory, 13, Instr(ADD_IMM(6, 6, -3)));
                                set(memory, 14, Instr(STR(0, 6, 0)));         (*arg0 <- 3*)
                                set(memory, 15, Instr(STR(0, 6, 2)));         (*arg2 <- 3 : on le fait direct car R0 contient déjà 3*)
                                set(memory, 16, Instr(ADD_IMM(0, 0, -2)));
                                set(memory, 17, Instr(STR(0, 6, 1)));         (*arg1 <- 1*)
                                set(memory, 18, Instr(JSR(19)));              (*hanoi(3, 1, 3)*)
                                set(memory, 19, Instr(ADD_IMM(6, 6, 3)));     (*pop arg0, arg1, arg2*)
                                set(memory, 21, Instr(BR(1, 1, 1, 1000)));      
                                (*Fonction mvt(src, dst)*)
                                set(memory, 22, Instr(ADD_IMM(6, 6, -3)));
                                set(memory, 23, Instr(STR(0, 6, 0)));
                                set(memory, 24, Instr(STR(1, 6, 1)));
                                set(memory, 25, Instr(STR(7, 6, 2)));           (*prologue*)
                                set(memory, 26, Instr(LDR(0, 6, 3)));
                                set(memory, 27, Instr(PRINT_REG(0)));           (*print R0*)
                                (*set(memory, 28, Instr(PRINT_STR(" --> ")));     (*print_string "-->"*)*)
                                set(memory, 29, Instr(LDR(0, 6, 4)));         
                                set(memory, 30, Instr(PRINT_REG(0)));           (*print R0*)
                                set(memory, 31, Instr(PRINT_LINE()));          
                                set(memory, 32, Instr(LDR(7, 6, 2)));
                                set(memory, 33, Instr(LDR(1, 6, 1)));
                                set(memory, 34, Instr(LDR(0, 6, 0)));
                                set(memory, 35, Instr(ADD_IMM(6, 6, 3)));
                                set(memory, 36, Instr(RET()));
                                (*Fonction hanoi(nb_disques, src, dst)*)
                                set(memory, 37, Instr(ADD_IMM(6, 6, -5)));
                                set(memory, 38, Instr(STR(7, 6, 0)));
                                set(memory, 39, Instr(STR(0, 6, 1)));
                                set(memory, 40, Instr(STR(1, 6, 2)));
                                set(memory, 41, Instr(STR(2, 6, 3)));
                                set(memory, 42, Instr(STR(3, 6, 4)));
                                set(memory, 43, Instr(LDR(0, 6, 5)));
                                set(memory, 44, Instr(AND(0, 0, 0)));
                                set(memory, 45, Instr(BR(1, 1, 0, 20)));        (*On s'arrête là, le nombre de disque est <= 0*)
                                set(memory, 46, Instr(ADD_IMM(0, 0, -1)));      (*R0 <- nb_disques - 1*)
                                set(memory, 47, Instr(LDR(1, 6, 6)));           (*R1 <- src*)
                                set(memory, 48, Instr(LDR(2, 6, 7)));           (*R2 <- dst*)
                                set(memory, 49, Instr(ADD(3, 1, 2)));
                                set(memory, 50, Instr(NOT(3, 3)));
                                set(memory, 51, Instr(ADD_IMM(3, 3, 7)));       (*itm = R3 <- 6 - src - dst*)
                                set(memory, 52, Instr(ADD_IMM(6, 6, -3)));
                                set(memory, 53, Instr(STR(0, 6, 0)));
                                set(memory, 54, Instr(STR(1, 6, 1)));
                                set(memory, 55, Instr(STR(3, 6, 2)));
                                set(memory, 56, Instr(JSR(-19)));               (*hanoi(nb_disques - 1, src, itm)*)
                                set(memory, 57, Instr(STR(1, 6, 0)));
                                set(memory, 58, Instr(STR(2, 6, 1)));
                                set(memory, 59, Instr(JSR(-37)));               (*mvt(src, dst)*)
                                set(memory, 60, Instr(STR(0, 6, 0)));
                                set(memory, 61, Instr(STR(3, 6, 1)));
                                set(memory, 62, Instr(STR(2, 6, 2)));
                                set(memory, 63, Instr(JSR(-26)));               (*hanoi(nb_disques - 1, itm, tgt)*)
                                set(memory, 64, Instr(ADD_IMM(6, 6, 3)));
                                set(memory, 65, Instr(LDR(3, 6, 4)));
                                set(memory, 66, Instr(LDR(2, 6, 3)));           (*prologue - hanoi0*)
                                set(memory, 67, Instr(LDR(1, 6, 2)));
                                set(memory, 68, Instr(LDR(0, 6, 1)));
                                set(memory, 69, Instr(LDR(7, 6, 0)));
                                set(memory, 70, Instr(ADD_IMM(6, 6, 5)));
                                set(memory, 71, Instr(RET()))
                                default () in rdy) init false
    in 
    if (loading_done) then 
        let compteur = chrono() in 
        let state = reg decode_minimalist init initial_state_f in ()
        (*
        if get_f(state.pc, 0) >= int_resize<<16>>(length(memory))
            then vect_make<<10>>(false) 
        else 
            vect_make<<10>>(false)
        *)
;;