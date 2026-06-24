(*
    `value`  : Type pour représenter le contenu d'une case de la mémoire 
    `three_b`: Type pour représenter plus simplement l'adresse d'un registre (il est sur 4 bits car en signé nous avons `+2^(n-1)-1` )
    `short`  : Type pour représenter un short en Eclat
    `imm`    : Type pour représenter une variable immédiate plutôt qu'un numéro de registres
*)
type three_b = int<4>;;
type short = int<16>;;
type imm = int<5>;;
type pc_offset_9 = int<9>;;

(*
    Toutes les instructions du programme 
*)
type instruction = NOT of three_b * three_b 
    | ADD of three_b * three_b * three_b
    | ADD_IMM of three_b * three_b * imm
    | AND of three_b * three_b * three_b
    | AND_IMM of three_b * three_b * imm
    | LEA of three_b * pc_offset_9
    | LD of three_b * pc_offset_9

    | NOP of unit
;;

(*
    `value` : Type pour représenter le contenu de la mémoire (cela peut être des adresses ou des constantes)
*)
type value = Bool of bool 
    | Addr of short 
    | Char of char 
    | Const of short
;;

let pc : short array<1> = create<1>();;
let ir : instruction array<1> = create<1>();;
(*
    Les registres sont des value car elles peuvent à la fois contenir des char, des addresses et d'autre chose défini par le type `value` 
*)
let registers : value array<8> = create<8>();;

let prog : instruction array<8> = create<8>();;
let memory : value array<3000> = create<3000>();;


(*---------------AFFICHAGE/DEBUG----------------*)
let print_reg cur_reg = 
    let res_reg = get(registers, cur_reg) in 
    match res_reg with 
    | Bool(b) -> print_string "voici le booléan : " ; Bool.print b
    | Addr(adr) -> print_string "voici l'adresse : " ; print_int adr
    | Char(c) -> print_string "voici le caractère : " ; print_char c
    | Const(s) -> print_string "voici le short : " ; print_int s
    | _ -> print_string "case du tableau vide ?"
;;

let debug cur_reg = 
    print_string "DEBUG------------------------------------------------------";
    print_newline();
    print_string "voici ce qu'il y a dans le registre ";
    print_int cur_reg;
    print_string " : ";
    print_reg(cur_reg);
    print_newline();
    print_string "-----------------------------------------------------------";
    print_newline();
    print_newline()
;;


(*---------------ACCESSEURS----------------*)
let get_const cur_reg = 
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


(*---------------ENVIRONNEMENT----------------*)
let init_env () = 
    set_r(pc, 0);
    set_r(ir, NOP());

    set(prog, 0, AND_IMM(0,0,0));
    set(prog, 1, AND_IMM(2,2,0));
    set(prog, 2, ADD_IMM(0,0,24));
    set(prog, 3, NOT(1,0));
    set(prog, 4, AND_IMM(3,2,0));
    set(prog, 5, NOP());
    set(prog, 6, LEA(5, 20));
    set(prog, 7, LD(5, 0));

    set(memory, 0, Char('H'));
    set(memory, 1, Char('e'));
    set(memory, 2, Char('l'));
    set(memory, 3, Char('l'));
    set(memory, 4, Char('o'));
    set(memory, 5, Char(' '))
;;
 

(*---------------DECODAGE----------------*)
(*
    Fonction qui va décoder toutes les instructions du programme et les exécuter.
    Nous allons beaucoup utiliser des fonctions de la bibliothèqes standard d'Eclat pour nous faciliter 
*)
let rec decode () : unit = 
    let pc_tmp = get_r(pc) in 
    print_string "pc=";
    print_int pc_tmp;
    print_newline();


    let size = length(prog) in 
    if size <= pc_tmp then (print_string "programme terminé" ; print_newline())
    else (
        
        set_r(ir, get(prog, get_r(pc)));
        let curr_inst : instruction = get_r(ir) in    
        set_r(pc, pc_tmp + 1);

        match curr_inst with 
        | NOT(dst_reg, src_reg) -> 
            (*lnot pour logical not venant de `stdlib`*)
            set(registers, dst_reg, Const(lnot(get_const(src_reg))));
            
            debug(dst_reg);
            decode()

        | ADD(dst_reg, sr1, sr2) -> 
            set(registers, dst_reg, Const(get_const(sr1) + get_const(sr2)));

            debug(dst_reg);
            decode()

        | ADD_IMM(dst_reg, sr1, imm) ->
            (*
                Attention, sr1 est un registre contenant un entier 16 bits mais
                imm est un eniter 5 bits
                Pour cela, nous pouvons utiliser int_resize de la bibliothèque 
                standard d'Eclat
            *)
            set(registers, dst_reg, Const(get_const(sr1) + int_resize<<16>>(imm)));

            debug(dst_reg);
            decode()

        | AND(dst_reg, sr1, sr2) -> 
            set(registers, dst_reg, Const(Int.land(get_const(sr1), get_const(sr2))));

            debug(dst_reg);
            decode()

        | AND_IMM(dst_reg, sr1, imm) -> 
            set(registers, dst_reg, Const(Int.land(get_const(sr1), int_resize<<16>>(imm))));
            
            
            debug(dst_reg);
            decode()

        | LEA(dst_reg, pc_off) -> 

            (*
                Contrairement au LC-3 classique ou l'asm nous permettrait de mettre des noms de labels et il y aurait un travail de fond
                qui remplacerait tous les noms par le bon incrément afin de stocker/sauter à cette adresse directement. Ici, nous 
                n'allons pas faire cela car cela serait `overkill` pour un simple bench.
                De plus, je n'ajoute pas le compteur pc car il est le compteur du programme `prog` qui est différent 
                de memory, si je faisais une addition, cela serait incohérent (Cela vaut aussi pour tout ce qui se sert de pc comme 
                de base pour accèder à une mémoire)
            *)
            set(registers, dst_reg, Addr(int_resize<<16>>(pc_off)));

            debug(dst_reg);
            decode()

        | LD(dst_reg, pc_off) -> 
            set(registers, dst_reg, get(memory, int_resize<<16>>(pc_off)));

            debug(dst_reg);
            decode()

        | NOP() -> 
            decode()

        | _ -> print_string "Incohérence dans le programme (case du tableau prog vide)"
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
    if rdy 
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