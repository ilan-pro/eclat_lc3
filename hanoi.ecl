let print_move (src, dst) =
    print_int src;
    print_string " --> ";
    print_int dst;
    print_newline()
;;

let rec hanoi (n, src, aux, dst) =
    if n > 0 then (
        hanoi(n - 1, src, dst, aux);
        print_move(src, dst);
        hanoi(n - 1, aux, src, dst)
    )
;;

let main () =
  let (x, rdy) = 
        exec 
          hanoi(3, 1, 2, 3)
        default ()
  in ()  
;;
