(*
    Bibliothèque de fonctions propre au processeur
    - Le not bit à bit d'un entier de 16 bits (un short)
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
    | ADD_IMM of tree_b * tree_b * imm
    (*
    | ADD of tree_b * tree_b * tree_b
    | AND of tree_b * tree_b * tree_b
    | AND_IMM of tree_b * tree_b * imm
    | LEA of tree_b * int<9> *)
    (* faire le reste....*)
;;

(*
    Type qui contiendra tous les registres nécessaires à l'environnement 
    `registers` : un vecteur d'instructions (style fonctionnel car immuable)
*)
let pc = ref 0;;
let ir = ref 0;;
let registers : short array<8> = create<8>();;

(*
    Instruction représentant le programme en cours 
    Est-ce que [] est du sucre syntaxique pour un vecteur de taille statique
    inféré à la compilation ?
*)
let prog = [ADD_IMM(0,0,0),ADD_IMM(0,0,20),NOT(1,0)];;
 
(*
    Fonction qui va décoder toutes les instructions du programme et les exécuter 
*)
let rec decode () : unit = 
    print_string "pc=";
    print_int !pc;
    print_newline ();
    let curr_inst : instruction = vect_nth(prog, !pc) in 
    
    match curr_inst with 
    | NOT(dst_reg, src_reg) -> 
        pc := !pc+1;
        (*lnot pour logical not venant de `stdlib`*)
        set(registers, dst_reg, lnot(get(registers, src_reg)));
        (*
        print_string "src_reg=";
        print_int (get(registers, src_reg));
        print_newline();
        print_newline()
        *)
        decode()
    | ADD_IMM(dst_reg, sr1, imm) -> 
        pc := !pc+1;
        (*
            Attention, sr1 est un registre contenant un entier 16 bits mais
            imm est un eniter 5 bits
            Pour cela, nous pouvons utiliser int_resize de la bibliothèque 
            standard d'Eclat
        *)
        set(registers, dst_reg, get(registers, sr1) + int_resize<<16>>(imm));
        decode()
    | _ -> print_string "je ne rentre jamais"
;;

let chrono () = 
    reg (fun c -> c + 1) init 0
;;

let main () = 
    let compteur = chrono () in
    (*
    let (x, rdy) = 
        exec 
            decode () 
        default () 
    in
    if rdy then (print_string "compteur=" ; print_int compteur ; print_newline ())
    *)
    decode()
    (*
    print_string "voici les valeurs des registres (r0,r1) : (";
    let r0 = get(registers, 0) in
    print_int r0;
    print_string ",";
    print_int (get(registers, 1));
    print_string ")";
    print_newline ()
    *)
;;