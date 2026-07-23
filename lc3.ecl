(*
    `value`  : Type pour représenter le contenu d'une case de la mémoire 
    `three_b`: Type pour représenter plus simplement l'adresse d'un registre (il est sur 4 bits car en signé nous avons `+2^(n-1)-1` )
    `short`  : Type pour représenter un short en Eclat
    `imm`    : Type pour représenter une variable immédiate plutôt qu'un numéro de registres
*)
type flag           = int<1>;;
type three_b        = int<4>;;
type imm            = int<5>;;
type pc_offset_6    = int<6>;;
type pc_offset_8    = int<8>;;
type pc_offset_9    = int<9>;;
type pc_offset_11   = int<11>;;
type short          = int<16>;;


(*
    Toutes les instructions du programme (c.f page 73 du polycopié)
*)
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

(*
    `value` : Type pour représenter le contenu de la mémoire (cela peut être des adresses ou des constantes)
*)
type value = Bool of bool 
    | Addr of short 
    | Char of char 
    | Const of short
    | Instr of instruction
;;

let nzp : flag array<3> = create<3>();;

let pc : short ref = ref 0;;

let ir : value array<1> = create<1>();;
(*
    Les registres sont des value car elles peuvent à la fois contenir des char, des addresses et d'autre chose défini par le type `value` 
*)
let registers : value array<8> = create<8>();;

let memory : value array<502> = create<502>();;


(*---------------ACCESSEURS----------------*)
let get_const inst = 
    match inst with 
    | Const c -> c 
    | Addr a -> a
    (* 
        Pour ce qui est du assert, nous pouvons avoir une approche différente comme juste renvoyer 0 
        Nous pouvons aussi transformer certaines constantes comme les char ou booléans afin de pouvoir utiliser leur 
        représentation entière 
    *)
    (*(assert false ; 0)*)
    | _ -> 0
;;

let get_reg cur_reg = 
    let regis = get(registers, cur_reg) in
    get_const(regis)
;;


let get_r r = 
    get(r, 0)
;;

let set_r (r, v) = 
    set(r, 0, v)
;;

let get_n () = 
    get(nzp, 0)
;;

let get_z () = 
    get(nzp, 1)
;;

let get_p () = 
    get(nzp, 2)
;;

let set_n val = 
    set(nzp, 0, val)
;;

let set_z val = 
    set(nzp, 1, val)
;;

let set_p val = 
    set(nzp, 2, val)
;;

(* VERSION IMPERATIVE ET FONCTIONNEL !!*)
let set_i (arr, s, i, x) = 
    set(arr, i, x); 
    s
;;

let get_i (arr, i) = 
    get(arr, i)
;;

let set_f (s, i, x) = 
    vect_update(s, i, x)
;;

let get_f (s, i) = 
    vect_nth(s, i)
;;


(*---------------ENVIRONNEMENT----------------*)
let init_env() = 
    set_n(0);
    set_z(0);
    set_p(0);
    set_r(ir, Instr(NOP()));

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

    (*
        adresse morte pour dire que c'est la fin du programme, donner la dernière adresse de la mémoire après le programme 
        et la pile
    *)
    set(memory, 21, Instr(BR(1, 1, 1, 502)));      

    (*Fonction mvt(src, dst)*)

    set(memory, 22, Instr(ADD_IMM(6, 6, -3)));
    set(memory, 23, Instr(STR(0, 6, 0)));
    set(memory, 24, Instr(STR(1, 6, 1)));
    set(memory, 25, Instr(STR(7, 6, 2)));           (*prologue*)

    set(memory, 26, Instr(LDR(0, 6, 3)));
    set(memory, 27, Instr(PRINT_REG(0)));           (*print R0*)
    set(memory, 28, Instr(PRINT_STR(" --> ")));     (*print_string "-->"*)
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

;;
 

(*---------------DECODAGE----------------*)
let rec decode_minimalist (set_q, get_q) : unit = 
    let pc_tmp = !pc in ();

    let size = length(memory) in 
    if(size <= pc_tmp) then (
        print_string("programme terminé"); 
        print_newline(); 
        ()

    ) else (   
        set_r(ir, get(memory, pc));
        let curr_inst : value = get_r(ir) in    
        pc := !pc + 1;

        (match curr_inst with 
        | Instr instr -> 
            
            (match instr with 
            | NOT(dst_reg, src_reg) -> 
                (*lnot pour logical not venant de `stdlib`*)
                let res = lnot(get_reg(src_reg)) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in ()

            | ADD(dst_reg, sr1, sr2) -> 
                let res = get_reg(sr1) + get_reg(sr2) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in ()

            | ADD_IMM(dst_reg, sr1, imm) ->
                (*
                    Attention, sr1 est un registre contenant un entier 16 bits mais
                    imm est un eniter 5 bits
                    Pour cela, nous pouvons utiliser int_resize de la bibliothèque 
                    standard d'Eclat
                *)
                let res = (get_reg(sr1) + imm) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in ()

            | AND(dst_reg, sr1, sr2) -> 
                let res = Int.land(get_reg(sr1), get_reg(sr2)) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in ()

            | AND_IMM(dst_reg, sr1, imm) -> 
                let res = Int.land(get_reg(sr1), imm) in
                set(registers, dst_reg, Const(res));
                
                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in ()

            | LEA(dst_reg, offset) -> 
                (*
                    Contrairement au LC-3 classique ou l'asm nous permettrait de mettre des noms de labels et il y aurait un travail de fond
                    qui remplacerait tous les noms par le bon incrément afin de stocker/sauter à cette adresse directement. Ici, nous 
                    n'allons pas faire cela car cela serait `overkill` pour un simple bench.
                    
                    PC est toujours incrementé avant le fetch dans le processeur LC-3 et c'est ce que nous avons voulu reproduire ici.
                    Donc au moment de l'incrément, pc n'est pas l'adresse de l'instruction courante (ici LEA) mais bien l'instruction suivante.
                    Ainsi, si nous voulons que le registre pointe bien l'instruction contenu à pc + pc_off nous devons décrémenter 
                    pc_off de 1 (il vaut mieux faire ça car nous allons souvent oublier qu'il faut prendre cela en compte au moment d'écrire 
                    l'instruction brut dans la fonction `init_env`). 
                    ATTENTION : je prends une copie de PC avant INCREMENT, donc pas besoin de décrementer. C'est un 
                    raccourcis (prendre le vrai tmp puis décrementer de 1 pour avoir parfaitement la même logique)
                    c.f notion rapport-2
                *)
                set(registers, dst_reg, Addr(pc_tmp + offset))

            | LD(dst_reg, offset) -> 
                set(registers, dst_reg, get(memory, pc_tmp + offset))

            | ST(src_reg, offset) ->    
                set(memory, pc_tmp + offset, get(registers, src_reg))

            | LDR(dst_reg, sr1, offset) -> 
                set(registers, dst_reg, get(memory, get_reg(sr1) + offset))

            | STR(src_reg, sr1, offset) -> 
                set(memory, get_reg(sr1) + offset, get(registers, src_reg))

            (*TODO : PAS TESTÉ*)
            | LDI(dst_reg, offset) -> 
                let val_1 = get(memory, pc_tmp + offset) in
                let const_mem = get_const(val_1) in 
                let val_2 = get(memory, const_mem) in
                set(memory, dst_reg, val_2)

            (*TODO : PAS TESTÉ*)
            | STI(src_reg, offset) ->   
                let val_instruction = get(memory, pc_tmp + offset) in
                let val = get_const(val_instruction) in
                set(memory, val, get(registers, src_reg))

            | NOP() -> ()

            | BR(n, z, p, offset) -> 
                (*
                    !ATTENTION! 
                    Ici j'utilise le getter de la bibliothèque standard d'Eclat sauf qu'il comprend 
                    les entiers en format Big_endian. Mais comme nous sommes sur 1 bits, 
                    il n'y a aucun problème.
                    À partir du moment ou il y a un flag vrai, nous sautons sans regarder le reste.
                *)
                let _ = 
                (
                    if(get_bit(get_n(), 0) && get_bit(n, 0)) 
                        then set_r(pc, pc_tmp + offset)
                    else if(get_bit(get_z(), 0) && get_bit(z, 0)) 
                        then set_r(pc, pc_tmp + offset)
                    else if(get_bit(get_p(), 0) && get_bit(p, 0)) 
                        then set_r(pc, pc_tmp + offset)
                ) in ()
            
            | JMP src_reg -> 
                set_r(pc, get_reg(src_reg))

            | RET() -> 
                set_r(pc, get_reg(7))

            | JSR offset -> 
                let cur_pc = get_r(pc) in 
                set(registers, 7, Const(cur_pc));
                set_r(pc, pc_tmp + offset)

            | PRINT_REG src_reg -> 
                print_int (get_reg(src_reg))

            | PRINT_STR str -> 
                print_string str

            | PRINT_LINE -> 
                print_newline()
                
            ) 

        | Char c -> 
            print_string    ("| [mem "); 
            print_int       (pc_tmp); 
            print_string    ("] : "); 
            print_char      (c);
            print_string    ("                                                 |");
            print_newline   ()

        | Const c -> 
            print_string    ("| [mem "); 
            print_int       (pc_tmp); 
            print_string    ("] : ");
            print_int       (c);
            print_string    ("                                                 |");
            print_newline   ()
        
        | Addr a -> 
            print_string    ("| [mem ");
            print_int       (pc_tmp); 
            print_string    ("] : "); 
            print_int       (a);
            print_string    ("                                                 |");
            print_newline   ()

        | Bool b -> 
            print_string    ("| [mem "); 
            print_int       (pc_tmp); 
            print_string    ("] : "); 
            Bool.print      (b);
            print_string    ("                                                 |");
            print_newline   ()

        | _ -> ()
        );

        decode_minimalist(set_q, get_q)
    )
;;
(*
(* version fonctionnelle *)
let decode_f (state) = decode(set_f, get_f, state, state.pc);;

(* version impérative *)
let decode_i (state) = decode(set_i, get_i, state, get_r(pc));;
*)

let chrono() = 
    reg (fun c -> c + 1) init 0
;;

let main() = 
    let compteur = chrono() in

    let (x, rdy) = 
        exec 
            (init_env() ; decode_minimalist(set_i, get_i))
        default() 
    in
    if(rdy)
        then 
            (   
                print_string    ("+-------------+");
                print_newline   () ; 
                print_string    ("| compteur="); 
                print_int       (compteur);
                print_string    (" |");
                print_newline   ();  
                print_string    ("+-------------+");
                print_newline   (); 
                print_newline   ()
            )
;;