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
			DB 13,10,"                   |3.EXIT                                |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                            Enter selection: $"

	;MAINMENU
	;-------------------------------------------------------------------------------------------------------------------------------------------
	MAINMENU 	DB 13,10,"                       PLEASE SELECT A FUNCTION        "
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |              MAIN MENU               |"
			DB 13,10,"                   +======================================+"
			DB 13,10,"                   |1.Withdraw                            |"
			DB 13,10,"                   |2.Tranfer                             |"
			DB 13,10,"                   |3.Deposit                             |"
			DB 13,10,"                   |4.Summary                             |"
			DB 13,10,"                   |5.Exit                                |"
			DB 13,10,"                   +======================================+"
			DB 13,10,13,10,"                            Enter selection: $"

	
	;Invalid msg
	;-------------------------------------------------------------------------------------------------------------------------------------------
	invalid DB "                  Invalid code pls Enter again ! $"

	;success msg
	;-------------------------------------------------------------------------------------------------------------------------------------------
	success DB "Successfully enterrrrrrr !!! $"

	;new line
	;-------------------------------------------------------------------------------------------------------------------------------------------
	n_line DB 13,10,"$"

	;user information
	;-------------------------------------------------------------------------------------------------------------------------------------------
	ACC1 DB "111111$"	;ID
	ACC2 DB "222222$"
	ACC3 DB "333333$"
	
	ACCPASS1 DB "AA11"	;PASSWORD
	ACCPASS2 DB "BB22"
	ACCPASS3 DB "CC33"
	
	ACCNO1 DB "1111100000"	;ACC NUMBER
	ACCNO2 DB "2222200000"
	ACCNO3 DB "3333300000"
	
	ACCNAME1 DB "ANGEL"		;USER NAME
	ACCNAME2 DB "BUNNY"
	ACCNAME3 DB "CHOLE"
	
	IDN DB "                            ENTER ID: $"
	PASSW DB "                         ENTER PASSWORD: $"
	ACCBAL1	 DW 40000
	ACCBAL DW ?
	ACCTYPE DB ?

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
			mov bl,0
	
compare:	
			mov bl,al

			cmp bl,'1'
			JE Login
			cmp bl,'2'
			JE invalidmsg
			cmp bl,'3'
			JE invalidmsg

			JE invalidmsg

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
			
again:      mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h		;invalid msg
			lea dx,invalid
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
		JMP PW
		
	
LCOMPARE3:
        MOV SI,0
        MOV CX,6 
        
SCANID3:
		MOV AL, IDNUM[SI]  ;compare with array
		CMP AL, ACC3[SI]
		JNE again
		INC SI
		LOOP SCANID3
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

		CMP ACTUALP,4  ;only enter 6 numbers
		JMP PASCOMPARE1
		
PAGAIN:     mov ah,09h
			lea dx,n_line
			int 21h
			
			mov ah,09h		;invalid msg
			lea dx,invalid
			int 21h
			jmp PW

PASCOMPARE1:
		MOV SI, 0
		MOV CX, 4

	;loop to scan 1 by 1
		
PW1:	MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS1[SI]
		JNE PASCOMPARE2
		INC SI
		LOOP PW1
		jmp mmenu
		
PASCOMPARE2:
		MOV SI, 0
		MOV CX, 4
		
PW2:
		MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS2[SI]
		JNE PASCOMPARE3
		INC SI
		LOOP PW2
		jmp mmenu
		
PASCOMPARE3:
		MOV SI, 0
		MOV CX, 4
	
PW3:
		MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS3[SI]
		JNE PAGAIN
		INC SI
		LOOP PW3
		jmp mmenu

mmenu:
		mov ah,09h
		lea dx,MAINMENU
		int 21h

	mov ah,4ch
	int 21h
main endp
	end main
