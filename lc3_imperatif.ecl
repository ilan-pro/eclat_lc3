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
    
    | LDR of three_b * three_b * pc_offset_6
    | STR of three_b * three_b * pc_offset_6
    
    | LDI of three_b * pc_offset_9
    | STI of three_b * pc_offset_9

    | BR of flag * flag * flag * pc_offset_9

    | NOP of unit

    | JMP of three_b

    | RET of unit 
;;

(*
    `value` : Type pour représenter le contenu de la mémoire (cela peut être des adresses ou des constantes)
*)
type value = Bool of bool 
    | Addr of short 
    | Char of char 
    | Const of short
;;

let nzp : flag array<3> = create<3>();;

let pc : short array<1> = create<1>();;
let ir : instruction array<1> = create<1>();;
(*
    Les registres sont des value car elles peuvent à la fois contenir des char, des addresses et d'autre chose défini par le type `value` 
*)
let registers : value array<8> = create<8>();;

let prog : instruction array<15> = create<15>();;
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
    int_resize<<32>>(get(nzp, 0))
;;

let get_z () = 
    int_resize<<32>>(get(nzp, 1))
;;

let get_p () = 
    int_resize<<32>>(get(nzp, 2))
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
    print_string("Valeur contenu dans le registre [");
    print_int(cur_reg);
    print_string "] : ";
    let res_reg = get(registers, cur_reg) in 
    match res_reg with 
    | Bool(b) -> print_string("voici le booléan : ") ; Bool.print(b)
    | Addr(adr) -> print_string("voici l'adresse : ") ; print_int(adr)
    | Char(c) -> print_string("voici le caractère : ") ; print_char(c)
    | Const(s) -> print_string("voici le short : ") ; print_int(s)
    | _ -> print_string("case du tableau vide ?")
;;

let print_mem cur_posi = 
    print_string("Valeur contenu dans la mémoire [");
    print_int(cur_posi);
    print_string "] : ";
    let regis = get(memory, cur_posi) in
    match regis with 
    | Bool(b) -> print_string("voici le booléan : ") ; Bool.print(b)
    | Addr(adr) -> print_string("voici l'adresse : ") ; print_int(adr)
    | Char(c) -> print_string("voici le caractère : ") ; print_char(c)
    | Const(s) -> print_string("voici le short : ") ; print_int(s)
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
    print_int(get_n());
    print_string("; ");
    print_int(get_z());
    print_string("; ");
    print_int(get_p());
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
    set_r(ir, NOP());

    set(prog, 0, AND_IMM(0,0,0));
    set(prog, 1, AND_IMM(2,2,0));
    set(prog, 2, ADD_IMM(0,0,24));
    set(prog, 3, NOT(1,0));
    set(prog, 4, AND_IMM(3,2,0));
    set(prog, 5, NOP());
    set(prog, 6, LEA(5, 20));
    set(prog, 7, LD(5, 0));
    set(prog, 8, LD(5, 1));
    set(prog, 9, LD(5, 2));
    set(prog, 10, LD(5, 3));
    set(prog, 11, LD(5, 4));
    set(prog, 12, LD(5, 5));
    set(prog, 13, ST(1, 6));
    set(prog, 14, BR(1, 0, 0, 1));

    set(memory, 0, Char('H'));
    set(memory, 1, Char('e'));
    set(memory, 2, Char('l'));
    set(memory, 3, Char('l'));
    set(memory, 4, Char('o'));
    set(memory, 5, Char('!'))
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


    let size = length(prog) in 
    if(size <= pc_tmp) then (print_string "programme terminé" ; print_newline())
    else (
        
        set_r(ir, get(prog, get_r(pc)));
        let curr_inst : instruction = get_r(ir) in    
        set_r(pc, pc_tmp + 1);

        match curr_inst with 
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
                De plus, je n'ajoute pas le compteur pc car il est le compteur du programme `prog` qui est différent 
                de memory, si je faisais une addition, cela serait incohérent (Cela vaut aussi pour tout ce qui se sert de pc comme 
                de base pour accèder à une mémoire)
            *)
            set(registers, dst_reg, Addr(int_resize<<16>>(offset)));

            debug(dst_reg);
            decode()

        | LD(dst_reg, offset) -> 
            set(registers, dst_reg, get(memory, int_resize<<16>>(offset)));

            debug(dst_reg);
            decode()

        | ST(src_reg, offset) ->    
            set(memory, int_resize<<16>>(offset), get(registers, src_reg));

            (*DEBUG*)
            print_mem(offset);
            print_newline();
            print_reg(src_reg);
            print_newline();

            decode()

        | NOP() -> 
            decode()

        | BR(n, z, p, offset) -> 
            if(get_bit(get_n(), 0) && get_bit(n, 0)) then print_string("youhou");

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