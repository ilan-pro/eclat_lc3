let rec hanoi n src aux dst =
  if n > 0 then begin
    hanoi (n - 1) src dst aux;

    Printf.printf "Déplacer un disque de %c vers %c\n" src dst;

    hanoi (n - 1) aux src dst
  end

let () =
  hanoi 3 'A' 'B' 'C'