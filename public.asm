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
	
	ACCPASS1 DB "AA11$"	;PASSWORD
	ACCPASS2 DB "BB22$"
	ACCPASS3 DB "CC33$"
	
	ACCNO1 DB "1111100000$"	;ACC NUMBER
	ACCNO2 DB "2222200000$"
	ACCNO3 DB "3333300000$"
	
	ACCNAME1 DB "ANGEL$"		;USER NAME
	ACCNAME2 DB "BUNNY$"
	ACCNAME3 DB "CHOLE$"
	
	ACCBAL1 DW 10000
	ACCBAL2 DW 2000
	ACCBAL3 DW 300
	
	isUser  DB ?
	TEMPBAL DW ?	
	
	
;DISPLAY	
;-------------------------------------------------------------------------------------------------------------------------------------------
	DACCNAME DB "USERNAME: $"
	DACCNO DB "ACCOUNT NO: $"
	DBAL DB "CURRENT BALANCE: $"
	IDN DB "                            ENTER ID: $"
	PASSW DB "                         ENTER PASSWORD: $"


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
		JNE again
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
			lea dx,invalid
			int 21h
			jmp PW
JMPER:	
		JMP PAGAIN	
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
		
		JMP DINFO
		mov ah,09h
		lea dx,n_line
		int 21h
		
		mov ah,09h
		lea dx,DACCNO
		int 21h 
		mov ah,09h
		lea dx,ACCNO1
		int 21h 
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		mov ah,09h
		lea dx,DACCNAME
		int 21h 
		mov ah,09h
		lea dx,ACCNAME1
		int 21h
		
		;MOV BX,ACCBAL1
		;MOV TEMPBAL,BX
		;MOV AH,02H
		;LEA DX, 
		;INT 21H
		jmp mmenu
		
PASCOMPARE2:

		MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS2[SI]
		JNE JMPER
		INC SI
		LOOP PASCOMPARE2
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		mov ah,09h
		lea dx,DACCNO
		int 21h 
		mov ah,09h
		lea dx,ACCNO2
		int 21h 
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		mov ah,09h
		lea dx,DACCNAME
		int 21h 
		mov ah,09h
		lea dx,ACCNAME2
		int 21h
		jmp mmenu
		
PASCOMPARE3:
		
		MOV AL, PASSWORDNUM[SI]  ;compare with array
		CMP AL, ACCPASS3[SI]
		JNE JMPER
		INC SI
		LOOP PASCOMPARE3
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		mov ah,09h
		lea dx,DACCNO
		int 21h 
		mov ah,09h
		lea dx,ACCNO3
		int 21h 
		
		mov ah,09h
		lea dx,n_line
		int 21h
		
		mov ah,09h
		lea dx,DACCNAME
		int 21h 
		mov ah,09h
		lea dx,ACCNAME3
		int 21h
		jmp mmenu

mmenu:
		mov ah,09h
		lea dx,MAINMENU
		int 21h
		
		

;----------------------------END PROC-----------------------------------------
	mov ah,4ch
	int 21h
main endp
	end main
