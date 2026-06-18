(*
    Type qui représente une instruction :
        - 4 premiers sont pour l'op-cod 
        - 3 suivants pour le resistre destination 
*)
type instruction = ((bool * bool * bool * bool) * (bool * bool * bool)
                    * (bool * bool * bool * bool * bool * bool * bool * bool * bool))

(*
    Faire un fichier pour l'ALU
*)