(*
    Bibliothèque de fonctions propre au processeur
*)



(*
    `tree_b` : Type pour représenter plus simplement l'adresse d'un registre
    `short`  : Type pour représenter un short en Eclat
    `imm`    : Type pour représenter une variable immédiate plutôt qu'un numéro de registres

*)
type tree_b = int<3>
type short = int<16>
type imm = int<5>

(*
    Toutes les instructions du programme 
*)
type instruction = NOT of tree_b * tree_b 
    | ADD of tree_b * tree_b * tree_b
    | ADD_IMM of tree_b * tree_b * imm
    | AND of tree_b * tree_b * tree_b
    | AND_IMM of tree_b * tree_b * imm
    (*
    | LEA of tree_b * int<9> 
    faire le reste....
    *)
;;

(*
    Type qui contiendra tous les registres nécessaires à l'environnement 
    `registers` : un vecteur d'instructions (style fonctionnel car immuable)
*)
let pc = create<1>();;
let ir = create<1>();;
let registers : short array<8> = create<8>();;

(*
    Instruction représentant le programme en cours 
    Est-ce que [] est du sucre syntaxique pour un vecteur de taille statique
    inféré à la compilation ?
*)
let prog : instruction array<5> = create<5>();;

let init_env () = 
    set(pc, 0, 0);
    set(ir, 0, 0);
    set(prog, 0, AND_IMM(0,0,0));
    set(prog, 1, AND_IMM(2,2,0));
    set(prog, 2, ADD_IMM(0,0,24));
    set(prog, 3, NOT(1,0));
    set(prog, 4, AND_IMM(3,2,0))
;;

let debug cur_reg = 
    print_string "DEBUG------------------------------------------------------";
    print_newline();
    print_string "voici ce qu'il y a dans le registre ";
    print_int cur_reg;
    print_string " : ";
    print_int(get(registers, cur_reg));
    print_newline();
    print_string "-----------------------------------------------------------";
    print_newline();
    print_newline()
;;
 
(*
    Fonction qui va décoder toutes les instructions du programme et les exécuter.
    Nous allons beaucoup utiliser des fonctions de la bibliothèqes standard d'Eclat pour nous faciliter 
*)
let rec decode () : unit = 
    let pc_tmp = get(pc, 0) in 
    print_string "pc=";
    print_int pc_tmp;
    print_newline();
    let size = length(prog) in 
    if size <= pc_tmp then (print_string "programme terminé" ; print_newline())
    else (
        let curr_inst : instruction = get(prog, (get(pc,0))) in 
        
        match curr_inst with 
        | NOT(dst_reg, src_reg) -> 
            set(pc, 0, pc_tmp + 1);
            (*lnot pour logical not venant de `stdlib`*)
            
            set(registers, dst_reg, lnot(get(registers, src_reg)));

            debug(dst_reg);
            decode()
        | ADD(dst_reg, sr1, sr2) -> 
            set(pc, 0, pc_tmp + 1);

            set(registers, dst_reg, get(registers, sr1) + get(registers, sr2));

            debug(dst_reg);
            decode()
        | ADD_IMM(dst_reg, sr1, imm) -> 
            set(pc, 0, get(pc, 0) + 1);
            
            (*
                Attention, sr1 est un registre contenant un entier 16 bits mais
                imm est un eniter 5 bits
                Pour cela, nous pouvons utiliser int_resize de la bibliothèque 
                standard d'Eclat
            *)
            set(registers, dst_reg, get(registers, sr1) + int_resize<<16>>(imm));
            
            debug(dst_reg);
            decode()
        |AND(dst_reg, sr1, sr2) -> 
            set(pc, 0, pc_tmp + 1);
            
            set(registers, dst_reg, Int.land(get(registers, sr1), get(registers, sr2)));

            debug(dst_reg);
            decode()
        |AND_IMM(dst_reg, sr1, imm) -> 
            set(pc, 0, pc_tmp + 1);
            
            set(registers, dst_reg, Int.land(get(registers, sr1), int_resize<<16>>(imm)));
            
            debug(dst_reg);
            decode()
        | _ -> print_string "Incohérence dans le programme"
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