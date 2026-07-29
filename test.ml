let rec factorielle n = 
  if n <= 1 then 1
  else n * factorielle(n-1)

let () = 
  print_endline("La factorielle de 5 est : " ^ string_of_int (factorielle 5))