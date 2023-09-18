.model small
.stack 100
.data

	;logo 
	;-----------------------------------------------------------------------------------------------------
	LOGO	DB 13,10,"          .----------------.  .----------------.  .----------------. "
		DB 13,10,"         | .--------------. || .--------------. || .--------------. |"
		DB 13,10,"         | |   ______     | || |   ______     | || |  _________   | |"
		DB 13,10,"         | |  |_   __ \   | || |  |_   _ \    | || | |_   ___  |  | |"
		DB 13,10,"         | |    | |__) |  | || |    | |_) |   | || |   | |_  \_|  | |"
		DB 13,10,"         | |    |  ___/   | || |    |  __'.   | || |   |  _|  _   | |"
		DB 13,10,"         | |   _| |_      | || |   _| |__) |  | || |  _| |___/ |  | |"
		DB 13,10,"         | |  |_____|     | || |  |_______/   | || | |_________|  | |"
		DB 13,10,"         | |              | || |              | || |              | |"
		DB 13,10,"         | '--------------' || '--------------' || '--------------' |"
 		DB 13,10,"         '----------------'  '----------------'  '----------------' $"

	;LOGMENU
	;-------------------------------------------------------------------------------------------------------------------------------------------
	LOGMENU DB 13,10,"                           WELCOME TO PUBLIC BANK        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |               LOG MENU               |"
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |1.Log In ACCOUNT                      |"
			DB 13,10,"                   |0.EXIT                                |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                            Enter selection: $"


	MAINMENU DB 13,10,"                           PLEASE SELECT A FUNCTION        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |              MAIN MENU               |"
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |1.Withdraw                            |"
			DB 13,10,"                   |2.Tranfer                             |"
			DB 13,10,"                   |3.Deposit                             |"
			DB 13,10,"                   |4.Calc Interest                       |"
			DB 13,10,"                   |5.Summary                             |"
			DB 13,10,"                   |0.Exit(Log Menu)                      |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                            Enter selection: $"   
					
     SMRY 	DB 13,10,"                         ~ WELCOME TO SUMARY REPORT ~        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |               SUMMARRY               |"
			DB 13,10,"                   +======================================+$"
	SEXIT	DB 13,10,13,10,"                   Enter 0 to Main Menu: $"
	
	HDLG    DB 13,10,"                                WELCOME TO PBE              "		  
            DB 13,10,"                   ========================================>"
			DB 13,10,"                   |                                       >"
			DB 13,10,"                   |                LOGIN IN               >"
			DB 13,10,"                   |                                       >" 
			DB 13,10,"                   ========================================>$"	
	
	HDDEP   DB 13,10,"                                WELCOME TO PBE              "
            DB 13,10,"                   ========================================>"
			DB 13,10,"                   |                                       >"
			DB 13,10,"                   |                DEPOSIT                >"
			DB 13,10,"                   |                                       >" 
			DB 13,10,"                   ========================================>$"
	
	HDWITH  DB 13,10,"                                WELCOME TO PBE              "
            DB 13,10,"                   ========================================>"
			DB 13,10,"                   |                                       >"
			DB 13,10,"                   |                WITHDRAW               >"
			DB 13,10,"                   |                                       >" 
			DB 13,10,"                   ========================================>$"

	HDTRANS DB 13,10,"                                WELCOME TO PBE              "
            DB 13,10,"                   ========================================>"
			DB 13,10,"                   |                                       >"
			DB 13,10,"                   |                TRANSFER               >"
			DB 13,10,"                   |                                       >" 
			DB 13,10,"                   ========================================>$"	

	HDINT   DB 13,10,"                                WELCOME TO PBE              "
            DB 13,10,"                   ========================================>"
			DB 13,10,"                   |                                       >"
			DB 13,10,"                   |           CALCULATE INTEREST          >"
			DB 13,10,"                   |                                       >" 
			DB 13,10,"                   ========================================>$"
			
	QUITMSG DB       "                              |                   |"
			DB 13,10,"                              |  PRESS 0 TO QUIT  |"
			DB 13,10,"                              |___________________|$"	
	ANYKEY	DB 13,10,"                          PRESS ANY KEY TO CONTINUE$"		
	n_line  DB 13,10,"$"
	
	;Invalid msg
	;-------------------------------------------------------------------------------------------------------------------------------------------
	INCORRECT DB 13,10, "                        ![INCORRECT PLS ENTER AGAIN]! $"
			  DB 13,10,"$"
	invalid   DB "                  ![INVALID PLS ENTER THE NUMBER GIVEN]! $"
	CLOSE     DB "                                 THANKS YOU! $"
	dginvalidmsg	  DB "Invalid input amount.Please enter digit only.$"
	inTransMsg DB	13,10,"The account number does not exist.Please enter a valid account number.",13,10,"$"
	invldmsg  DB	13,10,"        The action does not exist.Please enter a valid action number.",13,10,"$"	
	
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
	
	isAction	DB ?
	isUser 		DB ?

;DISPLAY	
;-------------------------------------------------------------------------------------------------------------------------------------------
	DACCNO   	DB 13,10,"                          ACCOUNT NO: $"
	DACCNAME 	DB	"                            USERNAME: $"
	CURRENTAMT	DB 13,10,"                          CURRENT AMOUNT: $"
	DBAL		DB "CURRENT BALANCE: $"
	IDN 		DB 13,10,"                            ENTER ID: $"
	PASSW 		DB "                      ENTER PASSWORD: $"
	disbalance  DB  "Your current bank balance                : $" 
	DEPT		DB "                    TIMES OF DEPOSIT  IS $"
	WITHT		DB "                    TIMES OF WITHDRAW IS $"
	TRANT		DB "                    TIMES OF TRANFERS IS $"
	lastbalance DB  "                    Current bank balance : $"
	CASHIN		DB "                    CASH IN:$"
	CASHOUT		DB 13,10,"                    CASH OUT:$"
	SCCS		DB 13,10,"   <<========================[SUCCESSFULLY DONE]========================>>",13,10,13,10,"$"
	INTDIS		DB " YEARS OF 5% INTEREST IS $"
	TMDEPOSIT	DB 0
	TMWITH		DB 0
	TMTRANFER	DB 0
	TTLDEPOSIT  DW 0
	TTLTRANFER	DW 0
	TTLWITH		DW 0
	TTLDEPOCENT DB 0
	TTLWITHCENT DB 0

	ID	LABEL BYTE      ;INPUT ID
	MAXIMUM		DB 	  7
	ACTUAL		DB	  ?
	IDNUM 		DB	  7 DUP()

	PASSWORD	LABEL BYTE  ;INPUT PASSWORD
	MAXIMUMP	DB 	  7
	ACTUALP		DB	  ?
	PASSWORDNUM DB	  7 DUP()

	;DEPOSIT	
;-------------------------------------------------------------------------------------------------------------------------------------------
	depositmsg    db "Enter the deposit amount                 : $"
	depositmsg2	  db "Enter the cent of deposit amount(00-99)  : $"
    againdeposit  db  "Are you continue to deposit? (Y/N) : $"
	tempBalance   dw 0
	tempamount 	  dw 0
	tempAccCent   db 00
	tempAmtCent   db 00
	rate 		  dw 1 
	remainder     dw ?

	;WITHDRAW	
;-------------------------------------------------------------------------------------------------------------------------------------------
	withdrawmsg    db "Enter the withdraw amount                : $"
	withdrawmsg2   db "Enter the cent of withdraw amount(00-99) : $"
    againwithdraw  db  "Are you continue to withdraw? (Y/N) : $"

	;TRANSFER	
;-------------------------------------------------------------------------------------------------------------------------------------------
	transaccmsg		DB	"Enter the transfer account               : $"
	transfermsg    	DB "Enter the transfer amount                : $"
	transfermsg2	DB "Enter the cent of transfer amount(00-99) : $"
    againtransfer   DB  "Are you continue to transfer? (Y/N) : $"
	noMoneyMsg 		DB "No enough money to cash out.Please enter a valid amount.$"
	istransUser DB	?
	
	TRANSIN	LABEL	BYTE    ;INPUT TRANSFER ACCOUNT NO
	MAX_TIN	DB		11
	ACT_TIN	DB		?
	DT_TIN	DB		11 DUP ('*')
	
	INPUTAMT	LABEL	BYTE ;INPUT AMOUNT 
	MAX_IN	    DB	6
	ACT_IN	    DB	?
	DT_IN	    DB	6 DUP('0')
	
	DECIMAL	LABEL	BYTE  ;INPUT DECIMAL OF AMOUNT
	MAX_DEC	DB	3
	ACT_DEC	DB	?
	DT_DEC	DB	3 DUP ('0')
	
	;CALCULATE INTEREST	
;-------------------------------------------------------------------------------------------------------------------------------------------
	interestMsg  	db "Enter the amount of saving                :$"
	interestMsg2 	db "Enter the cent amount of saving(00-99)    :$"
	interestMsg3	db "Enter the total years of saving           :$"
	interestYear 	db	?
	resultInterest 	db "Your interest will be RM $"
	
	quo 	dw ?
	rem 	dw ?
	dec1 	db ?
	dec2 	db ?
	
;=====================================================================================================================
.code
main proc 
	mov ax,@data                    
	mov ds,ax
	
menu:		CALL CLR1
			mov ah,09h		
			lea dx,LOGO       ;DISPLAY LOGO
			int 21h
		
			lea dx,LOGMENU    ;DISPLAY LOGIN MENU
			int 21h
	
input:		mov ah,01h
			int 21h
			mov BL,0
			SUB AL,30H
	
compare:	mov BL,AL
			cmp BL,1
			JE LOGIN
			CMP BL,0
			JE EXIT

EXIT: 		CALL CLR1	      ;EXIT PROGRAM
			mov ah,09h
			lea dx,n_line
			int 21h
			
			LEA DX,CLOSE
			INT 21H
			MOV AH,4CH
			INT 21H
			
invalidmsg: mov ah,09h
			lea dx,n_line
			int 21h
			
			lea dx,invalid      ;invalid msg
			int 21h
			jmp input

Login:		CALL CLR2           ;LOGIN	
			mov ah,09h
			lea dx,n_line
			int 21h
			
			lea dx,HDLG
			int 21h
			
			lea dx,n_line
			int 21h
			
			lea dx,n_line
			int 21h
					
			lea dx,IDN
			int 21h

			mov cx,6
			mov si,0

			MOV AH, 0AH     ;ENTER ID
			LEA DX, ID
			INT 21H

			CMP ACTUAL, 6  ;only enter 6 numbers
			JE LCOMPARE1

JMPER1N2: 	JMP MENU			
again:      mov ah,09h
			lea dx,n_line
			int 21h
			
			lea dx,INCORRECT    ;invalid msg
			int 21h
			
			lea dx,ANYKEY
			int 21h
			
			mov ah,01h
			int 21h
			jmp Login

LCOMPARE1:	MOV SI, 0
			MOV CX, 6    	;loop to scan 1 by 1

SCANID1:	MOV AL, IDNUM[SI]  ;compare with array
			CMP AL, ACC1[SI]
			JNE LCOMPARE2
			INC SI
			LOOP SCANID1
			mov al,1
			mov isUser,al
			JMP PW
		
LCOMPARE2: 	MOV SI,0
			MOV CX,6
SCANID2:	MOV AL, IDNUM[SI]  ;compare with array
			CMP AL, ACC2[SI]
			JNE LCOMPARE3
			INC SI
			LOOP SCANID2
			mov al,2
			mov isUser,al
			JMP PW
		
LCOMPARE3:	MOV SI,0
			MOV CX,6 
        
SCANID3:	MOV AL, IDNUM[SI]  ;compare with array
			CMP AL, ACC3[SI]
			JNE AGAIN
			INC SI
			LOOP SCANID3
			mov al,3
			mov isUser,al
			JMP PW
	
PW:			mov ah,09h             ;ENTER PASSWORD
			lea dx,n_line
			int 21h
						
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
			
JMPER:		JMP PAGAIN
JMPER1N1: JMP JMPER1N2	

COMPAREPS:	CALL CLR3      
			MOV SI, 0
			MOV CX, 4
			MOV BL,0
			
			MOV bl,isUser       ;VEERIFY WHICH USER
			cmp bl,1
			je PASCOMPARE1
			CMP bL,2
			Je PASCOMPARE2
			CMP bL,3
			Je PASCOMPARE3 
		
PASCOMPARE1:	MOV AL, PASSWORDNUM[SI]  ;compare with array
				CMP AL, ACCPASS1[SI]
				JNE JMPER
				INC SI
				LOOP PASCOMPARE1
				JMP DISUSER1
		
PASCOMPARE2:	MOV AL, PASSWORDNUM[SI]  ;compare with array
				CMP AL, ACCPASS2[SI]
				JNE JMPER
				INC SI
				LOOP PASCOMPARE2
				JMP DISUSER2
		
PASCOMPARE3:	MOV AL, PASSWORDNUM[SI]  ;compare with array
				CMP AL, ACCPASS3[SI]
				JNE JMPER
				INC SI
				LOOP PASCOMPARE3
				JMP DISUSER3
		
DISUSER1:	mov ah,09h          ;MOVE USER1 DATA TO TEMP VARIABLE
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
			JMP mmmenu
			
DISUSER2:	mov ah,09h          ;MOVE USER2 DATA TO TEMP VARIABLE
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
			jmp mmmenu

DISUSER3:	mov ah,09h          ;MOVE USER3 DATA TO TEMP VARIABLE
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
			
			mov ax,ACCBAL3
			mov tempBalance,ax
			
			mov BL,ACCCent3
			mov tempAccCent,BL
			jmp mmmenu    
	
JMPER1:		mov bx,tempBalance      ;MOVE TEMP VARIABLE TO USER1
			mov dl,tempAmtCent
			mov ah,isUser
			cmp ah,1
			JNE cmpu2
			mov ACCBAL1,bx
			mov ACCCent1,dl
			jmp jmpMenu
		
cmpu2:		cmp ah,2                ;MOVE TEMP VARIABLE TO USER2
			JNE cmpu3
			mov ACCBAL2,bx
			mov ACCCent2,dl
			jmp jmpMenu
		
cmpu3:		mov ACCBAL3,bx         ;MOVE TEMP VARIABLE TO USER2
			mov ACCCent2,dl
		
jmpMenu:	JMP JMPER1N1
			
mmmenu:		mov TTLWITHCENT,0		;CLR 
			mov TTLWITH,0
			mov TTLDEPOSIT,0
			mov TTLDEPOCENT,0
			MOV TMDEPOSIT,0
			mov TMTRANFER,0
			MOV TMWITH,0
			JMP mmenu	
		
WHOISTHIS:	CALL CLR3	              ;DEFINE WHICH USER
			MOV bl,isUser	
			cmp bl,1	
			je D1	
			CMP bL,2	
			Je D2	
			CMP bL,3	
			Je D3 	
			
D1:		mov ah,09h			;DISPLAY USER1 DATA 
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
		JMP mmenu
		
D2:		mov ah,09h			;DISPLAY USER2 DATA 	
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
		jmp mmenu
		
D3:		mov ah,09h			;DISPLAY USER3 DATA 	
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
		
mmenu:	mov ah,09h
		lea dx,CURRENTAMT         ;display account balance
		int 21h
		
		mov ax,0
		mov ax,tempBalance
		call disBal
		mov al,tempAccCent
		CALL DDECIMAL
		
		mov ah,09h
		lea dx,n_line
		int 21h
		lea dx,n_line
		int 21h

		lea dx,MAINMENU
		int 21h
		
		MOV AH,01H				;ENTER & VERIFY ACTION
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
		JE contJumper3
		CMP BL,5
		JE contJumper5
		CMP BL,0
		JE contJumper4
		jmp invalidAct
		
invalidAct:		mov ah,09h
				lea dx,invldmsg
				int 21h
				
				lea dx,ANYKEY
				int 21h
				mov ah,01h
				int 21h
				jmp WHOISTHIS
				
contJumper5:	jmp SUMMARY				
contJumper4: 	jmp JMPER1
contJumper3: 	jmp INTERESTCALC		

WITHDRAW:   call clr2				;START OF WITHDRAW FEATURE
			CALL EXITMSG
			MOV AH,09H
			LEA DX,n_line
			INT 21H
			
			LEA DX,HDWITH
			INT 21H
			
			lea dx,n_line
			int 21h
			
			call with				;WITHDRAW PROGRAM
			
TRANFER:    call clr2				;START OF TRANSFER FEATURE
			CALL EXITMSG
			MOV AH,09H
			LEA DX,n_line
			INT 21H
			
			LEA DX,HDTRANS
			INT 21H
			
			mov ah,09h
			lea dx,n_line
			int 21h
			call trans				;TRANSFER PROGRAM
			
DEPOSIT:    call clr2				;START OF DEPOSIT FEATURE
			CALL EXITMSG
			MOV AH,09H
			LEA DX,n_line
			INT 21H
			
			LEA DX,HDDEP
			INT 21H
			
			mov ah,09h
			lea dx,n_line
			int 21h
            call dep				;DEPOSIT PROGRAM
			
SUMMARY:    call clr2 				;START OF SUMMARY 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			MOV AH,09H
			LEA DX,SMRY
			INT 21H 
				
			mov ah,09h
			lea dx,n_line
			int 21h  
			
			MOV AH,09H
			LEA DX,WITHT
			INT 21H 
			
			ADD TMWITH,30H	;ATTEMPT OF WITHDRAW
			MOV AH,02H
			MOV DL,TMWITH
			INT 21H
			SUB TMWITH,30H
	
			mov ah,09h
			lea dx,n_line
			int 21h

			MOV AH,09H
			LEA DX,TRANT
			INT 21H			
			
			ADD TMTRANFER,30H	;ATTEMPT OF TRANFER
			MOV AH,02H
			MOV DL,TMTRANFER
			INT 21H
			SUB TMTRANFER,30H

			mov ah,09h
			lea dx,n_line
			int 21h

			MOV AH,09H
			LEA DX,DEPT
			INT 21H
			
			ADD TMDEPOSIT,30H	;ATTEMPT OF DEPOSIT
			MOV AH,02H
			MOV DL,TMDEPOSIT
			INT 21H
			SUB TMDEPOSIT,30H
			
			mov ah,09h
			lea dx,n_line
			int 21h
			
			MOV AH,09H
			LEA DX,CASHIN		;DISPLAY TOTAL CASH IN
			INT 21H
			
			mov ax,0
			mov ax,TTLDEPOSIT
			call disBal
			MOV AL,0
			MOV AL,TTLDEPOCENT
			CALL DDECIMAL
			
			MOV AH,09H
			LEA DX,CASHOUT		;DISPLAY TOTAL CASH OUT
			INT 21H
			
			mov ax,0
			mov ax,TTLWITH
			call disBal
			MOV AL,0
			MOV AL,TTLWITHCENT
			CALL DDECIMAL

			mov ah,09h
			lea dx,n_line
			int 21h
			lea dx,n_line
			int 21h

			lea dx,lastbalance		;DISPLAY CURRENT BALANCE ACCOUNT
            int 21h
			MOV AX,0
			MOV AX,tempBalance
            call disBal
			MOV AL,tempAccCent
			CALL DDECIMAL
            			
			MOV AH,09H
			LEA DX,SEXIT
			INT 21H
			
			MOV AH,01H
			INT 21H
			
			CMP AL,'0'
			JMP WHOISTHIS					

INTERESTCALC:	call clr2				;START OF CALC INTEREST
				CALL EXITMSG
				MOV AH,09H
				LEA DX,n_line
				INT 21H
				
				LEA DX,HDINT
				INT 21H
				
				LEA DX,n_line
				INT 21H

				CALL CALCINT			;CALCULATION INTEREST PROGRAM
;----------------------------END PROC-----------------------------------------
	mov ah,4ch							;EXIT
	int 21h
main endp

Dep proc								;DEPOSIT PROGRAM
STARTDEP:	MOV AH,09H
			lea dx,n_line
			int 21h
			
			lea dx,depositmsg
			int 21h
			
			mov ah,0AH
			lea dx,INPUTAMT			;INPUT AMOUNT OF DEPOSIT
			int 21h
			
			mov ah,09h
			lea dx,n_line
			int 21h
			
			lea dx,depositmsg2
			int 21h
			
			mov ah,0AH
			lea dx,DECIMAL			;INPUT DECIMAL AMOUNT OF DEPOSIT
			int 21h

			call validDigit			;VALIDATION TO MAKE SURE USER ENTER 1-9
			MOV AL,0
			MOV AL,TMDEPOSIT
			ADD AL,1
			MOV TMDEPOSIT,AL
	
DEPCALCULATE:	mov si,0			
				mov tempamount,0	
				mov rate,1
				mov al,ACT_IN
				mov ah,00H
				mov cl,al
				mov ch,00H
				lea si,ACT_IN		;ADDRESS OF INPUT AMOUNT
				add si,ax
			
				call DigitINPUT 	;CHANGE STRING AMOUNT TO VALUE
		
				MOV BX,0			;STORE TOTAL AMT DEPOSIT
				MOV BX,tempamount
				MOV DX,TTLDEPOSIT
				ADD DX,BX
				MOV TTLDEPOSIT,DX	
				
				mov al,tempAmtCent	;CALCULATE DECIMAL
				add tempAccCent,al
				mov al,tempAccCent
				cmp al,100
				JB addition
				sub al,100D
				mov tempAccCent,al
				inc tempamount
				jmp addition
		
Addition:	MOV BL,0			;STORE TOTAL AMTCENT DEPOSIT
			MOV BL,tempAmtCent
			MOV DL,TTLDEPOCENT
			ADD DL,BL
			CMP DL,100
			JAE ADDAMT
			jmp POO
			
ADDAMT:		SUB DL,100D
			MOV TTLDEPOCENT,DL
			INC TTLDEPOSIT
			jmp POO

depJumper1: jmp STARTDEP

POO: 		MOV TTLDEPOCENT,DL
			mov ax,0
			mov ax,tempBalance		
			add ax,tempamount
			mov tempBalance,ax
			
			mov ah,09h
			lea dx,n_line
			int 21h
			
			LEA DX,SCCS
			INT 21H
			lea dx,disbalance		;display balance
			int 21h
			
			mov ax,0
			mov ax,tempBalance
			call disBal
			mov al,tempAccCent
			CALL DDECIMAL
	
askdeposit:	mov dx,0000h			;ask to repeat deposit
			mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h
			lea dx,n_line
			int 21h

			mov ah,09h
			lea dx,againdeposit	 	;display continue or not?
			int 21h
			
			mov ah,01h
			int 21h
			
			cmp al,'Y'
			JE depjumper1
			cmp al,'y'
			JE depjumper1
			jmp WHOISTHIS
Dep endp
;---------------------------------------------------------------------------------------------------------------------------
With proc						;WITHDRAW PROGRAM
STARTWITH:	MOV AH,09H
			lea dx,n_line
			int 21h
			
			lea dx,withdrawmsg
			int 21h
			
			mov ah,0AH
			lea dx,INPUTAMT				;INPUT AMOUNT OF WITHDRAW
			int 21h
			
			mov ah,09h
			lea dx,n_line
			int 21h
			
			lea dx,withdrawmsg2
			int 21h
			
			mov ah,0AH
			lea dx,DECIMAL				;INPUT DECIMAL AMOUNT OF WITHDRAW
			int 21h

			call validDigit				;VERIFY USER ENTER 0-9 ONLY
			call subs					;SUBSTRATION CASH PROGRAM
			MOV AL,0
			MOV AL,TMWITH
			ADD AL,1
			MOV TMWITH,AL
			
			mov ah,09h					
			lea dx,n_line
			int 21h
			
			LEA DX,SCCS
			INT 21H
			lea dx,disbalance			;display balance
			int 21h
			
			mov ax,0
			mov ax,tempBalance
			call disBal					;DISPLAY BALANCE PROGRAM
			mov al,tempAccCent
			CALL DDECIMAL				;DISPLAY DECIMAL BALANCE PROGRAM

askwithdraw:	mov ah,09h				;ask to repeat deposit
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
				jmp WHOISTHIS               				
	With endp
	
disBal proc							;display BALANCE			
	cmp ax,10000
	JAE WITHdis5
	cmp ax,1000
	JAE WITHdis4
	cmp ax,100
	JAE WITHdis3
	cmp ax,10
	JAE WITHdis2
	jmp WITHdis1
	
WITHdis5: 	mov dx,0 				;DISPLAY TEN THOUSAND DIGIT
			mov bx,10000D
			div bx
			mov remainder,dx
			mov ah,02h
			mov dl,al
			add dl,30h
			int 21h
			mov ax,remainder
	
WITHdis4: 	mov dx,0				;DISPLAY THOUSAND DIGIT
			mov bx,1000D
			div bx  
			mov remainder,dx
			mov ah,02h
			mov dl,al
			add dl,30h
			int 21h
			mov ax,remainder
	
WITHdis3: 	mov dx,0						;DISPLAY HUNDRED DIGIT
			mov bx,100D
			div bx 
			mov remainder,dx
			mov ah,02h
			mov dl,al 
			add dl,30h
			int 21h
			mov ax,remainder
	
WITHdis2: 	mov dx,0						;DISPLAY TEN DIGIT
			mov bx,10D 
			div bx     
			mov remainder,dx
			mov ah,02h
			mov dl,al
			add dl,30h
			int 21h	
			mov ax,remainder
			jmp WITHdis1
	
WITHdis1: 	mov ah,02h   					;DISPLAY SINGLE DIGIT	
			mov dl,al  
			add dl,30h
			int 21h
			
			mov ah,02h  					;display .
			mov dl,2EH
			int 21h
ret
	disBal endp
 
 DDECIMAL PROC						;display demical
			mov ah,00H
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
 DDECIMAL endp
 
 subs proc
 WITHCALCULATE:	mov si,0
				mov tempamount,0	
				mov rate,1
				mov al,ACT_IN
				mov ah,00H
				mov cl,al
				mov ch,00H
				lea si,ACT_IN
				add si,ax
				jmp WITHINPUT
		
WITHINPUT:		mov bx,rate					;ADD DIGITS TO TEMP AMOUNT
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
				jmp calcDecimal

	
contJumper1: jmp STARTWITH
contJumper2: jmp STARTTRANS2
	
	
calcDecimal:	mov si,0
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
				
WITHdigitCent1:	mov al,[si]    ;calculation for one digit decimal
				sub al,30h
				add tempAmtCent,al
				mov al,tempAmtCent
				
				mov al,tempAmtCent
				cmp tempAccCent,al
				JAE decSub
				mov bl,100D
				add tempAccCent,BL
				dec tempBalance
				decSub:
				sub tempAccCent,al
				mov al,tempAccCent
				
				CMP tempAccCent,al
				JB NOMONEY
				
				mov ax,tempBalance				;check enough money
				mov bx,tempamount
				cmp ax,bx
				jae storeTotalAmt
NOMONEY:		mov ah,09h
				lea dx,n_line
				int 21h

        		lea dx,noMoneyMsg
				int 21h
				mov al,isAction
				cmp al,1
				je contJumper1
				jne contJumper2
				
storeTotalAmt:			
				MOV BX,0			;STORE TOTAL AMT
				MOV BX,tempamount
				MOV DX,TTLWITH
				ADD DX,BX
				MOV TTLWITH,DX
				
				MOV CL,0			;STORE TOTAL AMTCENT
				MOV CL,tempAmtCent
				MOV DL,TTLWITHCENT
				ADD DL,CL
				CMP DL,100
				JAE ADDAMTT
				jmp PEE
				
ADDAMTT:		SUB DL,100D
				MOV TTLWITHCENT,DL
				INC TTLWITH

PEE:			MOV TTLWITHCENT,DL
				jmp SUBSTACTION	

	
withjumper: jmp STARTWITH	

SUBSTACTION:	mov ax,0
				mov ax,tempamount
				mov ax,tempBalance
				sub ax,tempamount
				mov tempBalance,ax
 ret
 subs endp
 
 validDigit proc				;VALIDATION FOR USER ENTER 1-9 ONLY
	mov si,0					;CHECK USER ENTER 0 AT THE FIRST DIGIT TO EXIT
	mov al,DT_IN[si]
	cmp al,30H
	je exitMenu
	jmp validDG
	
exitMenu:	mov ah,09h
			lea dx,n_line
			int 21h
			jmp WHOISTHIS
			
validDG:	mov cx,0000h		;validation to make sure user input all digit
			mov cl,ACT_IN
			mov si,0
			
Dgvalid:	mov ax,0000h	
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
	
withinvalid:	mov ah,09h
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
STARTTRANS: 	mov ah,09h					;TRANSFER PROGRAM
				lea dx,n_line
				int 21h
				
				lea dx,transaccmsg
				int 21h
				
				mov ah,0AH
				lea dx,TRANSIN
				int 21h
				
				mov si,0					;CHECK USER ENTER 0 AT FIRST DIGIT TO QUIT
				mov al,DT_TIN[si]
				cmp al,30H
				je exitMenu2
				jmp cmpuser1
	
exitMenu2:		mov ah,09h
				lea dx,n_line
				int 21h
				jmp WHOISTHIS

cmpuser1:		mov bl,isUser				;search ACCNO
				cmp bl,1
				je cmpuser2
				jne checkUser1
cmpuser2:		cmp bl,2
				je cmpuser3
				jne checkUser2
cmpuser3:		cmp bl,3
				je invalidtrans
				jne checkUser3
	
checkUser1:		mov cx,10				;check transfer account
				mov si,0
				
check1:			mov al,ACCNO1[si]
				cmp al,DT_TIN[si]
				jne cmpuser2
				inc si
				loop check1
				mov istransUser,1
				jmp STARTTRANS2
				
				checkUser2:
				mov cx,10
				mov si,0
				
check2:			mov al,ACCNO2[si]
				cmp al,DT_TIN[si]
				jne cmpuser3
				inc si
				loop check2
				mov istransUser,2
				jmp STARTTRANS2
				
				checkUser3:
				mov cx,10
				mov si,0
				
check3:			mov al,ACCNO3[si]
				cmp al,DT_TIN[si]
				jne invalidtrans
				inc si
				loop check3
				mov istransUser,3
				jmp STARTTRANS2

invalidtrans:	mov ah,09h
				lea dx,inTransMsg			;account number does find
				int 21h
				JMP STARTTRANS
		
STARTTRANS2:	MOV AH,09H
				lea dx,n_line
				int 21h
				
				lea dx,transfermsg
				int 21h
				
				mov ah,0AH
				lea dx,INPUTAMT				;INPUT AMOUNT OF TRANSFER
				int 21h
				
				mov ah,09h
				lea dx,n_line
				int 21h
				
				lea dx,transfermsg2
				int 21h
				
				mov ah,0AH
				lea dx,DECIMAL				;INPUT DECIMAL AMOUNT OF TRANSFER
				int 21h
				
				call validDigit
				call subs
				MOV AL,0				
				MOV AL,TMTRANFER
				ADD AL,1
				MOV TMTRANFER,AL
				
				mov ah,tempAmtCent
				mov al,istransUser
				cmp al,1
				JE transUser1
				cmp al,2
				JE transUser2
				JMP transUser3
				
transUser1:		add ACCCent1,ah					;TRANSFER AMOUNT TO USER1
				mov dl,ACCCent1
				call checkover100
				mov ACCCent1,DL
				add ACCBAL1,Ax
				mov bx,tempamount
				add ACCBAL1,bx
				jmp displayamt
	
transUser2:		add ACCCent2,ah					;TRANSFER AMOUNT TO USER2
				mov dl,ACCCent2
				call checkover100
				mov ACCCent2,DL
				add ACCBAL2,Ax
				mov bx,tempamount
				add ACCBAL2,bx
				jmp displayamt
	
transUser3:		add ACCCent3,ah					;TRANSFER AMOUNT TO USER3
				mov dl,ACCCent3
				call checkover100
				mov ACCCent3,DL
				add ACCBAL3,Ax
				mov bx,tempamount
				add ACCBAL3,bx	
	
displayamt:		mov ah,09h						;display balance
				lea dx,n_line
				int 21h
				
				LEA DX,SCCS
				INT 21H
				lea dx,disbalance
				int 21h
				
				mov ax,0
				mov ax,tempBalance
				call disBal
				mov al,tempAccCent
				CALL DDECIMAL
				
asktransfer:	mov ah,09h						;ask to repeat deposit
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
				jmp WHOISTHIS
				
contStart1: jmp STARTTRANS	
trans endp

checkover100 proc				;CHECK TRANSFER AMOUNT DECIMAL OVER 100 OR NOT
	mov ah,00h
	cmp dl,100D
	jb notOver
	mov al,1
	sub dl,100D
ret	
notOver: mov al,00h 
ret	
checkover100 endp

CALCINT proc
		mov ah,09h
		lea dx,n_line
		int 21h
		
		lea dx,interestMsg			
		int 21h
		
		mov ah,0AH
		lea dx,INPUTAMT					;ENTER AMOUNT OF SAVING AMOUNT
		int 21h
		
		call validDigit
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		lea dx,interestMsg2			
		int 21h
		
		mov ah,0AH
		lea dx,DECIMAL					;ENTER DECIMAL AMOUNT OF SAVING AMOUNT
		int 21h
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		lea dx,interestMsg3
		int 21h
		
		mov ah,01h						;ENTER YEAR OF SAVING
		int 21h   
		mov interestYear,al				
		
		mov si,0
        mov tempamount,0	
		mov rate,1
		mov al,ACT_IN
		mov ah,00H
		mov cl,al
		mov ch,00H
		lea si,ACT_IN
		add si,ax
		
		call DigitINPUT					;CHANGE STRING AMOUNT TO VALUE
		
		mov ax,tempamount
		mov bx,100D
		mov dx,0
		div bx
		mov rem,dx
		mov bl,05
		mul bl
		mov quo,ax 
		mov bl,interestYear
		sub bl,30h
		mul bx
		mov quo,ax
		mov ax,rem
		mov bl,interestYear
		sub bl,30h
		mul bl
		mov rem,ax
		mov bh,00
		mov bl,05
		mul bx
		mov rem,ax
		cmp ax,100D
		JL interestDec
		mov dx,0
		mov bx,100D
		div bx
		add quo,ax
		mov rem,dx
		
interestDec:	mov bl,05
				mov ah,00H
				mov al,tempAmtCent
				mul bl
				mov dx,0
				mov bx,0 
				mov bl,interestYear
				sub bl,30h
				mul bx
				cmp ax,100D
				JL disInterest
				mov dx,0000
				mov bx,100D
				div bx
				add rem,ax
				mov ax,rem
				cmp al,100D
				JL movDec
				sub al,100D
				mov bx,0001
				add quo,bx
		
movDec:			mov dec1,al
				mov dec2,dl
		
disInterest:    mov ah,09h
				lea dx,n_line
				int 21h
				
				MOV AH,02H
				MOV DL,interestYear
				INT 21H 
				MOV AH,09H
				LEA DX,INTDIS
				INT 21H
			
				mov ax,quo
				call disBal
				mov al,dec1
				call DDECIMAL
				mov ax,0
				mov al,dec2
				mov bl,10D
				div bl
				mov ah,02h
				mov dl,al
				add dl,30h
				int 21h
				
				mov ah,09h
				lea dx,n_line
				int 21h
				
				lea dx,ANYKEY
				int 21h
				
				mov ah,01h
				int 21h
				
				jmp WHOISTHIS
CALCINT endp
DigitINPUT proc
DEPINPUT:	mov bx,rate
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
ret
DigitINPUT endp

CLR1 PROC	
	MOV AH,00	
    MOV AL,02	
    INT 10H	
    MOV ax, 0600h	
    MOV BH, 1FH	
    MOV cx, 0000h	
    MOV dx, 184fh	
    INT 10H	
 RET	
CLR1 ENDP	
CLR2 PROC	
    MOV AH,00	
    MOV AL,02	
    INT 10H	
    MOV ax, 0600h	
    MOV BH, 2FH	
    MOV cx, 0000h	
    MOV dx, 184fh	
    INT 10H	
 RET	
CLR2 ENDP	
CLR3 PROC		
    MOV AH,00	
    MOV AL,02	
    INT 10H	
    MOV ax, 0600h	
    MOV BH, 6FH	
    MOV cx, 0000h	
    MOV dx, 184fh	
    INT 10H	
 RET	
CLR3 ENDP

EXITMSG proc
	mov ah,09h
	mov bl,04h
	mov cx,15
	int 10h

	MOV AH,09H
	LEA DX,QUITMSG
	INT 21H
ret
	EXITMSG endp
	end main
