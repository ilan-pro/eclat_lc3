; Programme principal appel à sous-routine somme
         .ORIG x0000
main:    ; Initialisation pile
         LEA R6, stackend
         AND R0, R0, 0
         ADD R0, R0, 5
         LEA R7, fin
         BR  somme     ; JSR somme
; Reservation espace mémoire pour la pile
         .BLKW x50
stackend:

; Sous-routine somme de R0 à 1
; @param R0 le nombre
; @return R1 la somme
; Préserve les autres registres
; Adresse de retour en R7
somme:   ADD R6, R6, -2
         STR R0, R6, 0 
         STR R7, R6, 1
         AND R0, R0, R0
         BRnz zero
         ADD R0, R0, -1
         JSR somme
         LDR R0, R6, 0
         ADD R1, R1, R0
         BR finsomme
zero:    AND R1, R1, 0
finsomme: LDR R7, R6, 1
          ADD R6, R6, 2
          RET
fin:     NOP
	 .END
