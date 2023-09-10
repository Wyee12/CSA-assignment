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
	
	ACCBAL1 DW ?
	ACCBAL2 DW 2000
	ACCBAL3 DW 300
	
	isUser  DB ?
	TEMPBAL DW ?
	DEPOSITSUM DB 4 DUP(?)

	
	
;DISPLAY	
;-------------------------------------------------------------------------------------------------------------------------------------------
	DACCNO   DB 13,10,"                          ACCOUNT NO: $"
	DACCNAME DB	"                            USERNAME: $"
		     DB 13,10,"$"
	CURRENTAMT DB 13,10,"             CURRENT AMOUNT: $"
	DBAL DB "CURRENT BALANCE: $"
	IDN DB 13,10,"                            ENTER ID: $"
	PASSW DB "                      ENTER PASSWORD: $"


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
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
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
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
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

		mov ah,09h
		lea dx,n_line
		int 21h
		
		jmp mmenu    
		
JMPER1: JMP JMPER1N1
			
mmenu:
		mov ah,09h
		lea dx,MAINMENU
		int 21h
		
		MOV AH,01H
		INT 21H
		MOV BL,0
		SUB AL,30H
		MOV BL,AL
		
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
			
			MOV AH,09H
			LEA DX,SUCCESS
			INT 21H
			MOV AH,4CH
			INT 21H
			
TRANFER: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			MOV AH,09H
			LEA DX,SUCCESS
			INT 21H
			MOV AH,4CH
			INT 21H
			
DEPOSIT: 
			mov ah,09h
			lea dx,n_line
			int 21h
			
			MOV CX,4
			MOV SI,0
			
		
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
	end main
