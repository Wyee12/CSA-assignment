.model small
.stack 100
.data

	;logo 
	;-----------------------------------------------------------------------------------------------------
	LOGO	DB	13,10,"			   *****   ******    ******"
			DB	13,10,"		   	   *   *   **    *   **"
			DB	13,10,"			   ****    ******    ****"
			DB	13,10,"			   **      **    *   **"
			DB	13,10,"			   **      ******    ******$"
			DB 13,10, "                                                                                    $"
			DB 13,10, "                                                                                    $"

	;LOGMENU
	;-------------------------------------------------------------------------------------------------------------------------------------------
	LOGMENU 	DB 13,10,"                         WELCOME TO PUBLIC BANK        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |               LOG MENU               |"
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |1.Log In ACCOUNT                      |"
			DB 13,10,"                   |0.EXIT                                |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                            Enter selection: $"

	;MAINMENU
	;-------------------------------------------------------------------------------------------------------------------------------------------
	MAINMENU DB 13,10,"                           PLEASE SELECT A FUNCTION        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |              MAIN MENU               |"
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |1.Withdraw                            |"
			DB 13,10,"                   |2.Tranfer                             |"
			DB 13,10,"                   |3.Deposit                             |"
			DB 13,10,"                   |4.Summary                             |"
			DB 13,10,"                   |0.Exit                                |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                            Enter selection: $"   

	isAction	DB	?
			
	;SUMMARY
	;-------------------------------------------------------------------------------------------------------------------------------------------		
     SMRY 	DB 13,10,"                         ~ WELCOME TO SUMARY REPORT ~        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |               SUMMARRY               |"
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |1.Withdraw                            |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                        Enter 0 to Exit: $"
	
	;Invalid msg
	;-------------------------------------------------------------------------------------------------------------------------------------------
	INCORRECT DB 13,10, "                     ![INCORRECT PLS ENTER AGAIN]! $"
			  DB 13,10,"$"
	invalid   DB "                  ![INVALID PLS ENTER THE NUMBER GIVEN]! $"
	CLOSE     DB "                                 THANKS YOU! $"
	dginvalidmsg	  db "Invalid input amount.Please enter digit only.$"
	inTransMsg DB	13,10,"The account number does not exist.Please enter a valid account number.",13,10,"$"
	

	;success msg
	;-------------------------------------------------------------------------------------------------------------------------------------------
	success DB "Successfully enterrrrrrr !!! $"
	n_line  DB 13,10,"$"

	;user information
	;-------------------------------------------------------------------------------------------------------------------------------------------
	ACC1 DB "111111$"	;ID
	ACC2 DB "222222$"
	ACC3 DB "333333$"
	
	ACCPASS1 DB "AA11$"	;PASSWORD
	ACCPASS2 DB "BB22$"
	ACCPASS3 DB "CC33$"
	
	ACCNO1 DB "1111100000$"	;ACC NUMBER
	ACCNO2 DB "2222200000$"
	ACCNO3 DB "3333300000$"
	
	ACCNAME1 DB "ANGEL$"		;USER NAME
	ACCNAME2 DB "BUNNY$"
	ACCNAME3 DB "CHOLE$"
	
	ACCBAL1 DW 13000
	ACCBAL2 DW 2000
	ACCBAL3 DW 300
	
	ACCCent1 db 50
	ACCCent2 db 00
	ACCCent3 db 80
	
	isUser  DB ?
	TEMPBAL DW ?
	DEPOSITSUM DB 4 DUP(?)

	
	
;DISPLAY	
;-------------------------------------------------------------------------------------------------------------------------------------------
	DACCNO   DB 13,10,"                          ACCOUNT NO: $"
	DACCNAME DB	"                            USERNAME: $"
		     DB 13,10,"$"
	CURRENTAMT DB 13,10,"                          CURRENT AMOUNT: $"
	DBAL DB "CURRENT BALANCE: $"
	IDN DB 13,10,"                            ENTER ID: $"
	PASSW DB "                      ENTER PASSWORD: $"
	disbalance    db  "Your current bank balance : $"


		;Local Arrays
	ID	LABEL BYTE
	MAXIMUM		DB 	  7
	ACTUAL		DB	  ?
	IDNUM 		DB	  7 DUP()
	
		;Local Arrays
	PASSWORD	LABEL BYTE
	MAXIMUMP	DB 	  7
	ACTUALP		DB	  ?
	PASSWORDNUM DB	  7 DUP()

	;DEPOSIT	
;-------------------------------------------------------------------------------------------------------------------------------------------
	depositmsg    db "Enter the deposit amount                : $"
	depositmsg2	  db "Enter the cent of deposit amount(00-99) : $"
    againdeposit  db  "Are you continue to deposit? (Y/N) : $"
	tempBalance   dw 0
	tempamount 	  dw 0
	tempAccCent   db 00
	tempAmtCent   db 00
	
	rate 		  dw 1 
	remainder      dw ?

	
	;WITHDRAW	
;-------------------------------------------------------------------------------------------------------------------------------------------
	withdrawmsg    db "Enter the withdraw amount                : $"
	withdrawmsg2	  db "Enter the cent of withdraw amount(00-99) : $"
    againwithdraw  db  "Are you continue to withdraw? (Y/N) : $"

	
	;TRANSFER	
;-------------------------------------------------------------------------------------------------------------------------------------------
	transaccmsg	DB	"Enter the transfer account :$"
	transfermsg    db "Enter the transfer amount                : $"
	transfermsg2	  db "Enter the cent of transfer amount(00-99) : $"
    againtransfer  db  "Are you continue to transfer? (Y/N) : $"
	noMoneyMsg DB "No enough money to cash out.Please enter a valid amount.$"
	
	istransUser DB	?
	
	TRANSIN	LABEL	BYTE
	MAX_TIN	DB		11
	ACT_TIN	DB		?
	DT_TIN	DB		11 DUP ('*')
	
	INPUTAMT	LABEL	BYTE
	MAX_IN	    DB	6
	ACT_IN	    DB	?
	DT_IN	    DB	6 DUP('0')
	
	DECIMAL	LABEL	BYTE
	MAX_DEC	DB	3
	ACT_DEC	DB	?
	DT_DEC	DB	3 DUP ('0')
	

	
;=====================================================================================================================
.code
main proc 
	mov ax,@data                    
	mov ds,ax
	

menu:
			mov ah,09h		
			lea dx,LOGO
			int 21h

			mov ah,09h		
			lea dx,LOGMENU
			int 21h
	
input:		mov ah,01h
			int 21h
			mov BL,0
			SUB AL,30H
	
compare:	
			mov BL,AL
			cmp BL,1
			JE LOGIN
			CMP BL,0
			JE EXIT

EXIT: 		
			mov ah,09h
			lea dx,n_line
			int 21h
			
			MOV AH,09H
			LEA DX,CLOSE
			INT 21H
			MOV AH,4CH
			INT 21H
			
invalidmsg: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h		;invalid msg
			lea dx,invalid
			int 21h
			jmp input

;----------------------------LOG IN-----------------------
Login:			
			mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h		
			lea dx,IDN
			int 21h

			mov cx,6
			mov si,0

;----------------------------ENTER ID-----------------------
			MOV AH, 0AH
			LEA DX, ID
			INT 21H

			CMP ACTUAL, 6  ;only enter 6 numbers
			JE LCOMPARE1

JMPER1N2: JMP MENU			
again:      mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h		;invalid msg
			lea dx,INCORRECT
			int 21h
			
			jmp Login

LCOMPARE1:
		MOV SI, 0
		MOV CX, 6
	;loop to scan 1 by 1
SCANID1:
		MOV AL, IDNUM[SI]  ;compare with array
		CMP AL, ACC1[SI]
		JNE LCOMPARE2
		INC SI
		LOOP SCANID1
		mov al,1
		mov isUser,al
		JMP PW
		
LCOMPARE2: 
        MOV SI,0
        MOV CX,6
SCANID2:
    
		MOV AL, IDNUM[SI]  ;compare with array
		CMP AL, ACC2[SI]
		JNE LCOMPARE3
		INC SI
		LOOP SCANID2
		mov al,2
		mov isUser,al
		JMP PW
		
	
LCOMPARE3:
        MOV SI,0
        MOV CX,6 
        
SCANID3:
		MOV AL, IDNUM[SI]  ;compare with array
		CMP AL, ACC3[SI]
		JNE AGAIN
		INC SI
		LOOP SCANID3
		mov al,3
		mov isUser,al
		JMP PW
	
;----------------------------ENTER PASSWORD-----------------------	
PW:
		mov ah,09h
		lea dx,n_line
		int 21h
			
		mov ah,09h		
		lea dx,PASSW
		int 21h

		mov cx,6
		mov si,0

		MOV AH,0
		MOV AH, 0AH
		LEA DX, PASSWORD
		INT 21H

		CMP ACTUALP,4  ;only enter 4 numbers
		JMP COMPAREPS
		
PAGAIN:     mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h		;invalid msg
			lea dx,INCORRECT
			int 21h
			jmp PW
JMPER:	
		JMP PAGAIN
		
JMPER1N1: JMP JMPER1N2	
COMPAREPS:	      
        MOV SI, 0
		MOV CX, 4
		MOV BL,0
		
		MOV bl,isUser
		cmp bl,1
		je PASCOMPARE1
		CMP bL,2
		Je PASCOMPARE2
		CMP bL,3
		Je PASCOMPARE3 
		
PASCOMPARE1:

	    MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS1[SI]
		JNE JMPER
		INC SI
		LOOP PASCOMPARE1
		JMP DISUSER1
		
PASCOMPARE2:

		MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS2[SI]
		JNE JMPER
		INC SI
		LOOP PASCOMPARE2
		JMP DISUSER2
		
PASCOMPARE3:
		
		MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS3[SI]
		JNE JMPER
		INC SI
		LOOP PASCOMPARE3
		JMP DISUSER3

		
DISUSER1:
		mov ah,09h
		lea dx,n_line
		int 21h
		
		lea dx,DACCNO
		int 21h 
		lea dx,ACCNO1
		int 21h 
		
		lea dx,n_line
		int 21h
		
		lea dx,DACCNAME
		int 21h 
		lea dx,ACCNAME1
		int 21h
		
		mov ax,ACCBAL1
		mov tempBalance,ax
		
		mov BL,ACCCent1
		mov tempAccCent,BL
		
		JMP mmenu
DISUSER2:
		mov ah,09h
		lea dx,n_line
		int 21h
		
		lea dx,DACCNO
		int 21h 
		lea dx,ACCNO2
		int 21h 
		
		lea dx,n_line
		int 21h
		
		lea dx,DACCNAME
		int 21h 
		lea dx,ACCNAME2
		int 21h
		
		mov ax,ACCBAL2
		mov tempBalance,ax
		
		mov BL,ACCCent2
		mov tempAccCent,BL
		
		jmp mmenu

DISUSER3:
		mov ah,09h
		lea dx,n_line
		int 21h
		
		lea dx,DACCNO
		int 21h 
		lea dx,ACCNO3
		int 21h 
		
		lea dx,n_line
		int 21h
		
		lea dx,DACCNAME
		int 21h 
		lea dx,ACCNAME3
		int 21h
		
		mov ax,ACCBAL2
		mov tempBalance,ax
		
		mov BL,ACCCent2
		mov tempAccCent,BL
		
		jmp mmenu    
		
JMPER1: JMP JMPER1N1

mmenu:
	;display account balance
	mov ah,09h
	lea dx,CURRENTAMT
	int 21h
	
	call disBal
	
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,n_line
	int 21h
		
		mov ah,09h
		lea dx,MAINMENU
		int 21h
		
		MOV AH,01H
		INT 21H
		MOV BL,0
		SUB AL,30H
		MOV BL,AL
		MOV isAction,BL
		
		CMP BL,1
		JE WITHDRAW
		CMP BL,2
		JE TRANFER
		CMP BL,3
		JE DEPOSIT
		CMP BL,4
		JE SUMMARY
		CMP BL,0
		JE JMPER1
		
WITHDRAW: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			call with
			
			;MOV AH,09H
			;LEA DX,SUCCESS
			;INT 21H
			;MOV AH,4CH
			;INT 21H
			
TRANFER: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			call trans
			
DEPOSIT: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
            call dep
			
		
SUMMARY: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			MOV AH,09H
			LEA DX,SMRY
			INT 21H
			
	
			MOV SI,0
			MOV CX,4
			
DISPLAYDEPO:

			
			MOV AH,09H
			LEA DX,ACCBAL1[SI]
			INT 21H
			INC si
			
			JMP mmenu
			
			

		
		

;----------------------------END PROC-----------------------------------------
	mov ah,4ch
	int 21h
main endp

Dep proc
        			STARTDEP:
	MOV AH,09H
	lea dx,n_line
	int 21h
	
	lea dx,depositmsg
	int 21h
	
	mov ah,0AH
	lea dx,INPUTAMT
	int 21h
	
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,depositmsg2
	int 21h
	
	mov ah,0AH
	lea dx,DECIMAL
	int 21h

call validDigit

	
DEPCALCULATE:
		mov si,0
        mov tempamount,0	
		mov rate,1
		mov al,ACT_IN
		mov ah,00H
		mov cl,al
		mov ch,00H
		lea si,ACT_IN
		add si,ax
		jmp DEPINPUT
		
		
DEPINPUT:	

	mov bx,rate
	mov ax,bx
	mov bx,[si]
	mov bh,00H
	sub bx,0030h
	mul bx
	add tempamount,ax
	mov bx,rate
	mov ax,0000H
	mov al,10D
	mov dx,0000H
	mul bx
	mov rate,ax
	dec si
	loop DEPINPUT
	
	mov si,0
	mov ax,0
	mov tempAmtCent,0
	mov bl,ACT_DEC
	mov bh,00h
	lea si,DT_DEC
	
	cmp bl,2
	JNE DEPdigitCent1
	mov bl,[si]
	mov bh,00H
	sub bl,30h
	mov al,10D
	mul bl
	mov tempAmtCent,al
	inc si
	
	DEPdigitCent1:
	mov al,[si]
	sub al,30h
	add tempAmtCent,al
	
	mov al,tempAmtCent
	add tempAccCent,al
	mov al,tempAccCent
	cmp al,100
	JB addition
	sub al,100D
	mov tempAccCent,al
	inc tempamount
	jmp addition
	
depJumper1: jmp STARTDEP	
Addition:
	mov ax,0
	;mov ax,tempamount
	mov ax,tempBalance
	add ax,tempamount
	mov tempBalance,ax
	
	;display balance
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,disbalance
	int 21h
	call disBal
	
askdeposit:
	mov dx,0000h
	;ask to repeat deposit
	mov ah,09h
	lea dx,n_line
	int 21h
	
	mov ah,09h
	lea dx,n_line
	int 21h

	mov ah,09h
	lea dx,againdeposit ;display continue or not?
	int 21h
	
	mov ah,01h
	int 21h
	
	cmp al,'Y'
	JE depjumper1
	cmp al,'y'
	JE depjumper1
	jmp mmenu
Dep endp

;---------------------------------------------------------------------------------------------------------------------------
With proc
STARTWITH:
	MOV AH,09H
	lea dx,n_line
	int 21h
	
	lea dx,withdrawmsg
	int 21h
	
	mov ah,0AH
	lea dx,INPUTAMT
	int 21h
	
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,withdrawmsg2
	int 21h
	
	mov ah,0AH
	lea dx,DECIMAL
	int 21h

	call validDigit
	
	call subs

	;display balance
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,disbalance
	int 21h
	call disBal

askwithdraw:
	;ask to repeat deposit
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,n_line
	int 21h

	lea dx,againwithdraw ;display 
	int 21h
	
	mov ah,01h
	int 21h
	
	cmp al,'Y'
	JE STARTWITH
	cmp al,'y'
	JE STARTWITH
	jmp mmenu               
	                
	With endp
	
disBal proc
;display BALANCE
	
	mov ax,0
	mov ax,tempBalance
	
	cmp ax,10000
	JAE WITHdis5
	cmp ax,1000
	JAE WITHdis4
	cmp ax,100
	JAE WITHdis3
	cmp ax,10
	JAE WITHdis2
	jmp WITHdis1
	
WITHdis5: 
	mov dx,0 
	mov bx,10000D
	div bx
	mov remainder,dx
	mov ah,02h
	mov dl,al
	add dl,30h
	int 21h
	mov ax,remainder
	
WITHdis4:
	mov dx,0
	mov bx,1000D
	div bx  
	mov remainder,dx
	mov ah,02h
	mov dl,al
	add dl,30h
	int 21h
	mov ax,remainder
	
WITHdis3:
	mov dx,0
	mov bx,100D
	div bx 
	mov remainder,dx
	mov ah,02h
	mov dl,al 
	add dl,30h
	int 21h
	mov ax,remainder
	
WITHdis2:
	mov dx,0
	mov bx,10D 
	div bx     
	mov remainder,dx
	mov ah,02h
	mov dl,al
	add dl,30h
	int 21h	
	mov ax,remainder
	jmp WITHdis1
	
WITHdis1:	
	mov ah,02h   
	mov dl,al  
	add dl,30h
	int 21h
	
	mov ah,02h  ;display .
	mov dl,2EH
	int 21h
	
	;display demical
	mov ax,0000H
	mov al,tempAccCent
	mov bl,10D
	div bl
	mov bl,ah
	mov ah,02h
	mov dl,al
	add dl,30h
	int 21h
	
	mov dl,bl
	add dl,30h
	int 21h
	
ret
 disBal endp
 
 subs proc
 
 WITHCALCULATE:
		mov si,0
        mov tempamount,0	
		mov rate,1
		mov al,ACT_IN
		mov ah,00H
		mov cl,al
		mov ch,00H
		lea si,ACT_IN
		add si,ax
		jmp WITHINPUT
		
		
WITHINPUT:	

	mov bx,rate
	mov ax,bx
	mov bx,[si]
	mov bh,00H
	sub bx,0030h
	mul bx
	add tempamount,ax
	mov bx,rate
	mov ax,0000H
	mov al,10D
	mov dx,0000H
	mul bx
	mov rate,ax
	dec si
	loop WITHINPUT
	;check enough money
	mov ax,tempBalance
	mov bx,tempamount
	cmp ax,bx
	jae calcDecimal
	mov ah,09h
	lea dx,n_line
	int 21h

	lea dx,noMoneyMsg
	int 21h
	mov al,isAction
	cmp al,1
	je contJumper1
	jne contJumper2
	
contJumper1: jmp STARTWITH
contJumper2: jmp STARTTRANS2
	
	
	calcDecimal:
	mov si,0
	mov ax,0
	mov tempAmtCent,0
	mov bl,ACT_DEC
	mov bh,00h
	lea si,DT_DEC
	
	cmp bl,2
	JNE WITHdigitCent1 
	mov bl,[si]        ;calculation for two digit DECIMAL
	mov bh,00H
	sub bl,30h
	mov al,10D
	mul bl
	mov tempAmtCent,al
	inc si
	
	WITHdigitCent1:    ;calculation for one digit decimal
	mov al,[si]
	sub al,30h
	add tempAmtCent,al
	
	mov al,tempAmtCent
	cmp tempAccCent,al
	JAE decSub
	mov bl,100D
	add tempAccCent,BL
	dec tempBalance
	decSub:
	sub tempAccCent,al
	mov al,tempAccCent
	jmp SUBSTACTION
	
withjumper: jmp STARTWITH	

SUBSTACTION:
	mov ax,0
	mov ax,tempamount
	mov ax,tempBalance
	sub ax,tempamount
	mov tempBalance,ax
 ret
 subs endp
 
 validDigit proc
 
 ;validation to make sure user input all digit
	mov cx,0000h
	mov cl,ACT_IN
	mov si,0
Dgvalid:
    mov ax,0000h	
	mov al,DT_IN[si]
	cmp al,30H
	jb withinvalid
	cmp al,39H
	ja withinvalid
	inc si
	loop Dgvalid
	MOV BL,isAction ; VALIDATE ACTION
			
			cmp BL,1
			JNE JMPTRANS
		    ret
JMPTRANS:	CMP BL,2
			JNE JMPDEP
			ret
JMPDEP:		ret
	
	
	withinvalid:	
			mov ah,09h
			lea dx,n_line
			int 21h
	
			lea dx,dginvalidmsg
			int 21h
			MOV BL,isAction
			
			cmp BL,1
			JNE CMPTRANS
			jmp STARTWITH
CMPTRANS:	CMP BL,2
			JNE CMPDEP
			JMP STARTWITH
CMPDEP:		JMP STARTDEP
			
ret	
	validDigit endp
	
trans proc
STARTTRANS:
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,transaccmsg
	int 21h
	
	mov ah,0AH
	lea dx,TRANSIN
	int 21h
	
	;search ACCNO
	mov bl,isUser
	cmp bl,1
	je cmpuser2
	jne checkUser1
cmpuser2:
	cmp bl,2
	je cmpuser3
	jne checkUser2
cmpuser3:
	cmp bl,3
	je invalidtrans
	jne checkUser3
	
	checkUser1:
	mov cx,10
	mov si,0
check1:
	mov al,ACCNO1[si]
	cmp al,DT_TIN[si]
	jne cmpuser2
	inc si
	loop check1
	jmp STARTTRANS2
	
	checkUser2:
	mov cx,10
	mov si,0
check2:
	mov al,ACCNO2[si]
	cmp al,DT_TIN[si]
	jne cmpuser3
	inc si
	loop check2
	jmp STARTTRANS2
	
	checkUser3:
	mov cx,10
	mov si,0
check3:
	mov al,ACCNO3[si]
	cmp al,DT_TIN[si]
	jne invalidtrans
	inc si
	loop check3
	jmp STARTTRANS2

invalidtrans:

		mov ah,09h
		lea dx,inTransMsg
		int 21h
		JMP STARTTRANS
		
STARTTRANS2:
	
	MOV AH,09H
	lea dx,n_line
	int 21h
	
	lea dx,transfermsg
	int 21h
	
	mov ah,0AH
	lea dx,INPUTAMT
	int 21h
	
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,transfermsg2
	int 21h
	
	mov ah,0AH
	lea dx,DECIMAL
	int 21h
	
	call validDigit
	
	call subs
	
	
	
	;display balance
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,disbalance
	int 21h
	call disBal
	
asktransfer:
	;ask to repeat deposit
	mov ah,09h
	lea dx,n_line
	int 21h
	
	lea dx,n_line
	int 21h

	lea dx,againtransfer ;display 
	int 21h
	
	mov ah,01h
	int 21h
	
	cmp al,'Y'
	JE contStart1
	cmp al,'y'
	JE contStart1
	jmp mmenu
contStart1: jmp STARTTRANS
	
trans endp
	end main
