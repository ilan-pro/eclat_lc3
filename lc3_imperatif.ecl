(*
    `value`  : Type pour représenter le contenu d'une case de la mémoire 
    `three_b`: Type pour représenter plus simplement l'adresse d'un registre (il est sur 4 bits car en signé nous avons `+2^(n-1)-1` )
    `short`  : Type pour représenter un short en Eclat
    `imm`    : Type pour représenter une variable immédiate plutôt qu'un numéro de registres
*)
type flag = int<1>;;
type three_b = int<4>;;
type imm = int<5>;;
type pc_offset_6 = int<6>;;
type pc_offset_9 = int<9>;;
type pc_offset_11 = int<11>;;
type short = int<16>;;

(*
    Toutes les instructions du programme (c.f page 73 du polycopié)
*)
type instruction = NOT of three_b * three_b 
    | ADD of three_b * three_b * three_b
    | AND of three_b * three_b * three_b

    | ADD_IMM of three_b * three_b * imm
    | AND_IMM of three_b * three_b * imm
    
    | LEA of three_b * pc_offset_9
    
    | LD of three_b * pc_offset_9
    | ST of three_b * pc_offset_9
    
    (*
    | LDR of three_b * three_b * pc_offset_6
    | STR of three_b * three_b * pc_offset_6
    
    | LDI of three_b * pc_offset_9
    | STI of three_b * pc_offset_9
    *)
    | BR of flag * flag * flag * pc_offset_9

    | NOP of unit
    (*
    | JMP of three_b

    | RET of unit 
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
let get_reg_const cur_reg = 
    let regis = get(registers, cur_reg) in
    match regis with 
    | Const(c) -> c 
    (* Pour ce qui est du assert, nous pouvons avoir une approche différente comme juste renvoyer 0 *)
    | _ -> (assert false ; 0)
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
let print_reg cur_reg = 
    print_string("[reg ");
    print_int(cur_reg);
    print_string "] ";
    let res_reg = get(registers, cur_reg) in 
    match res_reg with 
    | Bool b -> print_string("voici le booléan : ") ; Bool.print(b)
    | Addr adr -> print_string("voici l'adresse : ") ; print_int(adr)
    | Char c -> print_string("voici le caractère : ") ; print_char(c)
    | Const s -> print_string("voici le short : ") ; print_int(s)
    | Instr x -> print_string("c'est une instruction...")
    | _ -> print_string("case du tableau vide ?")
;;

let print_mem cur_posi = 
    print_string("[reg ");
    print_int(cur_posi);
    print_string "] ";
    let regis = get(memory, cur_posi) in
    match regis with 
    | Bool b -> print_string("voici le booléan : ") ; Bool.print(b)
    | Addr adr -> print_string("voici l'adresse : ") ; print_int(adr)
    | Char c -> print_string("voici le caractère : ") ; print_char(c)
    | Const s -> print_string("voici le short : ") ; print_int(s)
    | _ -> print_string("case du tableau vide ?")
;;

let debug cur_reg = 
    print_string("DEBUG-----------------------------------------------------------------");
    print_newline();
    print_reg(cur_reg);
    print_newline();
    print_string("----------------------------------------------------------------------");
    print_newline();
    print_newline()
;;

let debug_nzp () =  
    print_string("NZP-------------------------------------------------------------------");
    print_newline();
    print_string("[");
    print_int(int_resize<<32>>(get_n()));
    print_string("; ");
    print_int(int_resize<<32>>(get_z()));
    print_string("; ");
    print_int(int_resize<<32>>(get_p()));
    print_string("]");
    print_newline();
    print_string("----------------------------------------------------------------------");
    print_newline();
    print_newline()
;;


(*---------------ENVIRONNEMENT----------------*)
let init_env () = 
    set_n(0);
    set_z(0);
    set_p(0);
    set_r(pc, 0);
    set_r(ir, Instr(NOP()));

    set(memory,  0, Instr(AND_IMM(0, 0, 0)));
    set(memory,  1, Instr(AND_IMM(2, 2, 0)));
    set(memory,  2, Instr(ADD_IMM(0, 0, 24)));
    set(memory,  3, Instr(NOT(1,0)));
    set(memory,  4, Instr(AND_IMM(3, 2, 0)));
    set(memory,  5, Instr(NOP()));
    set(memory,  6, Instr(LEA(5, 17)));
    (*Attention au position, au moment de la 7ème instruction, `pc` vaut 8 !*)
    set(memory,  7, Instr(LD(5, 23)));
    set(memory,  8, Instr(LD(5, 23)));
    set(memory,  9, Instr(LD(5, 23)));
    set(memory, 10, Instr(LD(5, 23)));
    set(memory, 11, Instr(LD(5, 23)));
    set(memory, 12, Instr(LD(5, 23)));
    set(memory, 13, Instr(ST(1, 10)));
    set(memory, 14, Instr(BR(0, 1, 0, 1)));

    (*SEGMENT DONNEE*)
    set(memory, 30, Char('H'));
    set(memory, 31, Char('e'));
    set(memory, 32, Char('l'));
    set(memory, 33, Char('l'));
    set(memory, 34, Char('o'));
    set(memory, 35, Char('!'))
;;
 

(*---------------DECODAGE----------------*)
(*
    Fonction qui va décoder toutes les instructions du programme et les exécuter.
    Nous allons beaucoup utiliser des fonctions de la bibliothèqes standard d'Eclat pour nous faciliter.
    Pour savoir pourquoi certaines instructions modifient les flags `nzp`, il faut se réferer au polycopié.
*)
let rec decode () : unit = 
    let pc_tmp = get_r(pc) in 
    print_string "pc=";
    print_int pc_tmp;
    print_newline();


    let size = length(memory) in 
    if(size <= pc_tmp) then (print_string "programme terminé" ; print_newline())
    else (
        
        set_r(ir, get(memory, get_r(pc)));
        let curr_inst : value = get_r(ir) in    
        set_r(pc, pc_tmp + 1);

        match curr_inst with 
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

                debug(dst_reg);
                debug_nzp();
            
                decode()

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

                debug(dst_reg);
                debug_nzp();
            
                decode()

            | ADD_IMM(dst_reg, sr1, imm) ->
                (*
                    Attention, sr1 est un registre contenant un entier 16 bits mais
                    imm est un eniter 5 bits
                    Pour cela, nous pouvons utiliser int_resize de la bibliothèque 
                    standard d'Eclat
                *)
                let res = get_reg_const(sr1) + int_resize<<16>>(imm) in
                set(registers, dst_reg, Const(res));

                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in

                debug(dst_reg);
                debug_nzp();

                decode()

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

                debug(dst_reg);
                debug_nzp();
    
                decode()

            | AND_IMM(dst_reg, sr1, imm) -> 
                let res = Int.land(get_reg_const(sr1), int_resize<<16>>(imm)) in
                set(registers, dst_reg, Const(res));
                
                let _ = 
                    if(res < 0) then(set_n(1))
                    else (set_n(0));

                    if(res == 0) then(set_z(1))
                    else (set_z(0));

                    if(res > 0) then(set_p(1))
                    else (set_p(0))
                in
                
                debug(dst_reg);
                debug_nzp();

                decode()

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
                    ATTENTION : je prends une copie de PC avant INCREMENT, donc pas besoin de décrementer
                    c.f notion rapport-2
                *)
                set(registers, dst_reg, Addr(pc_tmp + int_resize<<16>>(offset)));

                debug(dst_reg);
                decode()

            | LD(dst_reg, offset) -> 
                set(registers, dst_reg, get(memory, pc_tmp + int_resize<<16>>(offset)));

                debug(dst_reg);
                decode()

            | ST(src_reg, offset) ->    
                set(memory, pc_tmp + int_resize<<16>>(offset), get(registers, src_reg));

                (*DEBUG*)
                print_mem(offset);
                print_newline();
                print_reg(src_reg);
                print_newline();

                decode()

            | NOP() -> 
                decode()

            | BR(n, z, p, offset) -> 
                (*
                    !ATTENTION! 
                    Ici j'utilise le getter de la bibliothèque standard d'Eclat sauf qu'il comprend 
                    les entiers en format Big_endian. Mais comme nous sommes sur 1 bits, 
                    il n'y a aucun problème.
                *)
                if(get_bit(get_n(), 0) && get_bit(n, 0)) 
                    then print_string("youhou1");
                if(get_bit(get_z(), 0) && get_bit(z, 0)) 
                    then print_string("youhou2");
                if(get_bit(get_p(), 0) && get_bit(p, 0)) 
                    then print_string("youhou3");
                

                decode()
            )

        | Char c -> 
            print_string("[") ; print_int(pc_tmp) ; print_string("] : "); print_char(c);
            print_newline();

            decode()

        | Const c -> 
            print_string("[") ; print_int(pc_tmp) ; print_string("] : "); print_int(c);
            print_newline();

            decode()
        
        | Addr a -> 
            print_string("[") ; print_int(pc_tmp) ; print_string("] : "); print_int(a);
            print_newline();

            decode()

        | Bool b -> 
            print_string("[") ; print_int(pc_tmp) ; print_string("] : "); Bool.print(b);
            print_newline();

            decode()

        | _ -> print_string("NOP") ; print_newline() ; decode()
    )
;;


let chrono () = 
    reg (fun c -> c + 1) init 0
;;


let main () = 
    let compteur = chrono () in
    
    let (x, rdy) = 
        exec 
            (init_env() ; decode ())
        default () 
    in
    if(rdy)
        then 
            (   
                print_string "+-------------+";
                print_newline() ; 
                print_string "| compteur=" ; 
                print_int compteur ;
                print_string " |" ;
                print_newline() ;  
                print_string "+-------------+";
                print_newline() ; 
                print_newline()
            )
;;