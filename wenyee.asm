.model small
.stack 100
.data
    n_line        db 10,13,"$"
    depositmsg    db "Enter the deposit amount : $"
    disdeposit    db  "Your current bank balance : $"
    againdeposit  db  "Are you continue to deposit? (Y/N) : $"
    isdeposit     db  ?

    depositAmount  db    ?

  ; ENTER DEPOSIT AMOUNT ----> CALCULATE  ----->  DISPLAY  -------> ASK REPEAT -------> LOOP 
  ; ENTER WITHDRAW AMOUNT ----> CALCULATE  ----->  DISPLAY  -------> ASK REPEAT -------> LOOP
  ; ENTER BANK ACC ----->  ENTER TRANSFER AMOUNT ----->  CALCULATE ------->  DISPLAY ------->  ASK REPEAT   ------> LOOP

.code
 main proc
  mov ax,@data
  mov ds,ax

  mov ah,4ch
  int 21h

main endp
  end main
