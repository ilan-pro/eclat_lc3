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

let pc : short array<1> = create<1>();;

let ir : value array<1> = create<1>();;
(*
    Les registres sont des value car elles peuvent à la fois contenir des char, des addresses et d'autre chose défini par le type `value` 
*)
let registers : value array<8> = create<8>();;

let memory : value array<3000> = create<3000>();;


(*---------------ACCESSEURS----------------*)
let get_const inst = 
    match inst with 
    | Const c -> c 
    (* 
        Pour ce qui est du assert, nous pouvons avoir une approche différente comme juste renvoyer 0 
        Nous pouvons aussi transformer certaines constantes comme les char ou booléans afin de pouvoir utiliser leur 
        représentation entière 
    *)
    (*(assert false ; 0)*)
    | _ -> 0
;;

let get_reg_const cur_reg = 
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


(*---------------AFFICHAGE/DEBUG----------------*)
let print_instruction (instr : instruction) = 
    match instr with 
    | NOT       _ -> print_string("NOT")
    | ADD       _ -> print_string("ADD")
    | AND       _ -> print_string("AND")

    | ADD_IMM   _ -> print_string("ADD_IMM")
    | AND_IMM   _ -> print_string("AND_IMM")
    
    | LEA       _ -> print_string("LEA")
    
    | LD        _ -> print_string("LD")
    | ST        _ -> print_string("ST")
    
    | LDR       _ -> print_string("LDR")
    | STR       _ -> print_string("STR")
    
    | LDI       _ -> print_string("LDI")
    | STI       _ -> print_string("STI")

    | BR        _ -> print_string("BR")

    | NOP       _ -> print_string("NOP")

    | JMP       _ -> print_string("JMP")

    | RET       _ -> print_string("RET")

    | JSR       _ -> print_string("JSR")
;;

let print_reg cur_reg = 
    print_string    ("| [R");
    print_int       (cur_reg);
    print_string    ("] ");
    let res_reg = get(registers, cur_reg) in 
    (match res_reg with 
    | Bool b    -> print_string         ("voici le booléan : ") ; Bool.print(b)
    | Addr adr  -> print_string         ("voici l'adresse : ") ; print_int(adr)
    | Char c    -> print_string         ("voici le caractère : ") ; print_char(c)
    | Const s   -> print_string         ("voici le short : ") ; print_int(s)
    | Instr x   -> print_instruction    (x)
    | _         -> print_string         ("case du tableau vide ?")
    );
    print_string    ("                                    |")
;;

let print_mem cur_posi = 
    print_string    ("[mem ");
    print_int       (cur_posi);
    print_string    ("] ");
    let regis = get(memory, cur_posi) in
    (match regis with 
    | Bool b    -> print_string         ("voici le booléan : ") ; Bool.print(b)
    | Addr adr  -> print_string         ("voici l'adresse : ") ; print_int(adr)
    | Char c    -> print_string         ("voici le caractère : ") ; print_char(c)
    | Const s   -> print_string         ("voici le short : ") ; print_int(s)
    | Instr x   -> print_instruction    (x)
    | _         -> print_string         ("case du tableau vide ?")
    );
    print_string    ("                                    |")

;;

let debug_reg cur_reg = 
    print_string    ("| DEBUG_REG---------------------------------------------------- |");
    print_newline   ();
    print_reg       (cur_reg);
    print_newline   ()
;;

let debug_pc() = 
    print_string    ("| DEBUG_PC----------------------------------------------------- |");
    print_newline   ();
    print_string    ("| ");
    print_int       (get_r(pc));
    print_string    ("                                                            |");
    print_newline   ()
;;

let start_pretty_print() = 
    print_string    ("+------------------------------[");
    print_int       (get_r(pc));
    print_string    ("]------------------------------+");
    print_newline()
;;

let end_pretty_print() = 
    print_string    ("+---------------------------------------------------------------+");
    print_newline   ();
    print_newline   ();
    print_newline   ()
;;


let debug_nzp() =  
    print_string    ("| DEBUG_NZP---------------------------------------------------- |");
    print_newline   ();
    print_string    ("| [");
    print_int       (int_resize<<32>>(get_n()));
    print_string    ("; ");
    print_int       (int_resize<<32>>(get_z()));
    print_string    ("; ");
    print_int       (int_resize<<32>>(get_p()));
    print_string    ("]                                                     |");
    print_newline   ()
;;


(*---------------ENVIRONNEMENT----------------*)
let init_env() = 
    set_n(0);
    set_z(0);
    set_p(0);
    set_r(pc, 0);
    set_r(ir, Instr(NOP()));

    set(memory,  0, Instr(AND_IMM(0, 0, 0)));
    set(memory,  1, Instr(AND_IMM(1, 1, 0)));
    set(memory,  2, Instr(AND_IMM(2, 2, 0)));
    set(memory,  3, Instr(AND_IMM(3, 3, 0)));
    set(memory,  4, Instr(AND_IMM(4, 4, 0)));
    set(memory,  5, Instr(AND_IMM(5, 5, 0)));
    set(memory,  6, Instr(AND_IMM(6, 6, 0)));
    set(memory,  7, Instr(AND_IMM(7, 7, 0)));

    set(memory, 10, Instr(LD(0, 20)));
    set(memory, 11, Instr(LD(1, 20)));
    set(memory, 12, Instr(JSR(0)));

    set(memory, 13, Instr(AND(2, 2, 0)));
    set(memory, 14, Instr(AND(1, 1, 1)));
    set(memory, 15, Instr(BR(0, 1, 0, 4)));

    set(memory, 16, Instr(ADD(2, 2, 0)));
    set(memory, 17, Instr(ADD_IMM(1, 1, -1)));
    set(memory, 18, Instr(BR(0, 0, 1, -2)));
    set(memory, 19, Instr(NOP()));


    set(memory, 30, Const(2));
    set(memory, 31, Const(3))
;;
 

(*---------------DECODAGE----------------*)
(*
    Fonction qui va décoder toutes les instructions du programme et les exécuter.
    Nous allons beaucoup utiliser des fonctions de la bibliothèqes standard d'Eclat pour nous faciliter.
    Pour savoir pourquoi certaines instructions modifient les flags `nzp`, il faut se réferer au polycopié.
*)
let rec decode () : unit = 
    let pc_tmp = get_r(pc) in 
    start_pretty_print();

    let size = length(memory) in 
    if(size <= pc_tmp) 
        then (print_string "programme terminé" ; print_newline())
    else    
        set_r(ir, get(memory, get_r(pc)));
        let curr_inst : value = get_r(ir) in    
        set_r(pc, pc_tmp + 1);

        (match curr_inst with 
        | Instr instr -> 
            (match instr with 
            | NOT(dst_reg, src_reg) -> 
                (*lnot pour logical not venant de `stdlib`*)
                let res = lnot(get_reg_const(src_reg)) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in

                debug_reg(dst_reg);
                debug_nzp()

            | ADD(dst_reg, sr1, sr2) -> 
                let res = get_reg_const(sr1) + get_reg_const(sr2) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in

                debug_reg(dst_reg);
                debug_nzp()

            | ADD_IMM(dst_reg, sr1, imm) ->
                (*
                    Attention, sr1 est un registre contenant un entier 16 bits mais
                    imm est un eniter 5 bits
                    Pour cela, nous pouvons utiliser int_resize de la bibliothèque 
                    standard d'Eclat
                *)
                let res = (get_reg_const(sr1) + imm) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in

                debug_reg(dst_reg);
                debug_nzp()

            | AND(dst_reg, sr1, sr2) -> 
                let res = Int.land(get_reg_const(sr1), get_reg_const(sr2)) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in

                debug_reg(dst_reg);
                debug_nzp()

            | AND_IMM(dst_reg, sr1, imm) -> 
                let res = Int.land(get_reg_const(sr1), imm) in
                set(registers, dst_reg, Const(res));
                
                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in
                
                debug_reg(dst_reg);
                debug_nzp()

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
                set(registers, dst_reg, Addr(pc_tmp + offset));

                debug_reg(dst_reg)

            | LD(dst_reg, offset) -> 
                set(registers, dst_reg, get(memory, pc_tmp + offset));

                debug_reg(dst_reg)

            | ST(src_reg, offset) ->    
                set(memory, pc_tmp + offset, get(registers, src_reg));

                (*DEBUG*)
                print_mem       (pc_tmp + offset);
                print_reg       (src_reg);
                print_newline   ()

            | LDR(dst_reg, sr1, offset) -> 
                set(registers, dst_reg, get(memory, get_reg_const(sr1) + offset));

                debug_reg(dst_reg)

            | STR(src_reg, sr1, offset) -> 
                set(memory, get_reg_const(sr1) + offset, get(registers, src_reg))

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
                set_r(pc, get_reg_const(src_reg))

            | RET() -> 
                set_r(pc, get_reg_const(7))

            | JSR offset -> 
                let cur_pc = get_r(pc) in 
                set(registers, 7, Const(cur_pc));
                set_r(pc, cur_pc + offset);

                debug_reg(7);
                debug_pc()
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

        | _ -> 
            print_string    ("| vide                                                           |"); 
            print_newline   ()
        );

        end_pretty_print();
        decode()
;;


let chrono() = 
    reg (fun c -> c + 1) init 0
;;


let main() = 
    let compteur = chrono() in
    
    let (x, rdy) = 
        exec 
            (init_env() ; decode())
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