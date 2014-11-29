print_color macro str,d,c,b
		MOV BP,OFFSET STR
		MOV DX,d		
                MOV CX,c
		MOV BL,b		;字符颜色属性（底色为红色，字符颜色为白色）
		MOV AX,1301H		;显示字符串（写方式：AL=0）
		INT 10H
endm

push_reg macro
	push dx
	push bx
	push ax
	push si
	push cx
endm

pop_reg macro
	pop cx
	pop si
	pop ax
	pop bx
	pop dx
endm
;打印数据段某字符串
extra_info macro mesg
	mov dx, offset mesg
	mov ah, 9h
	int 21h
endm
;光标定位
set_cursor macro disp
	mov dx, disp
        mov ah, 2h
	int 10h
endm
;字符串输入
input_str macro space
        mov dx,offset space
        mov ah,0ah
        int 21h
endm
;字符输入
input_char macro
	mov ah, 01h
	int 21h
endm

delayt macro t
        local delay3, loop3
        mov     bx, t
        delay3:
                push    bx
                mov     ah, 2ch
                int     21h
                mov     bx, dx
        loop3:
                mov     ah, 0bh
                int     21h
                cmp     al, 0ffh
                jz      end_sing
                mov     ah, 2ch
                int     21h
                cmp     dx, bx
                je      loop3
                pop     bx
                dec     bx
                jnz     delay3	
endm delayt


stack segment stack'stack'
        dw 100 dup(0)
stack ends

data segment
	 ;/////////////////////////////////welcome界面
        well2 db 0ah, 0dh,' O    .N    G        :@                                   $' 
        well3 db 0ah, 0dh,' @M   B@:  iB   ..   JB    ,,    .,.      .   .,      .   $' 
        well4 db 0ah, 0dh,'  @  F8.@  B7 rO::B2 r@  8BvY7 i@ur5@. Y@r7O@PrY@   @r:EB $' 
        well5 db 0ah, 0dh,'  M8 @  B. @  BFir0@ rB j@     @    v@ uB   B   BE @OiruB $' 
        well6 db 0ah, 0dh,'   B@8  :B@i  @J     L@ r@     BY   O@ 5@   @   @0 B8     $' 
        well7 db 0ah, 0dh,'   :B    PP    S7;L   k  ,SLLi  5JrL7  .2   M   Y:  uviL. $' 
        null db 0ah, 0dh,'                                                                     $'  ;空格打印两行
        
         well8 db 0ah, 0dh,'          u@B@@r vB@B@Y                                                $'
         well9 db 0ah, 0dh,'         B@B@@@B@B@@@B@@      ,     j              .. .:,.   1     ,:i $'
         wella db 0ah, 0dh,'        B@B@@@B@B@B@0@B@B   7B@O   B@   EJ    XBM  7@B@,5S@F iF1MB@Yj2;$'
         wellb db 0ah, 0dh,'        @B@G    8Bi  B@@@     M@B BB    U@    v@B  :G@Bj:F,@    i@7    $'
         wellc db 0ah, 0dh,'         @B@B@:    @@@B@       1@BM  E  MB :  :B@  .0B@B :EB    uPX    $'
         welld db 0ah, 0dh,'         B@@@B@   @B@@@B    O@  @@B  S  BZ @: M@B  .F@r 2uL     P7G    $'
         welle db 0ah, 0dh,'        B@B@B@  i@@@B@B@B      F@ @B:   @B    @@@   .Bi         @J@    $'
         wellf db 0ah, 0dh,'        @B@B@B@B@B@B@B@B@ .i .B@   5@k  r@L .MB@r   ,@:         B@B    $'
         wellg db 0ah, 0dh,'         @B@B@B@B@B@B@B@    vFi      B:   Y5qu:      B          rv2    $'
         wellh db 0ah, 0dh,'          7@@B@: :@B@B7                          	                   $'

	load_mesg db 'Enter any key to load...$'
	nice_mesg db 'Now, Please set Priority for 4 proc(1-9):$'
	;nice_proc db 'nice for proc$'
	start_mesg db 'So, Let us start(Enter any key)...$'
	;////////////////////////////////////nice 和 choose_proc
        set_pri  db 4, 3, 2, 1
        exec_que db 1, 2, 3, 4
        pri_hint db 0ah, 0dh, '       Set priority for 4 proc(1-9)$'
	
	;/////////////////////////////////face界面
        buf1 db '+---------------------------------------+----------------------------------+$'
        buf3 db '|                                       |                                  |$'
        tab_error db '!Press Tab for for task scheduling$'
	;tab_again db 'Any key to try again$'

        ;///////////////////////////////////proc1
	INF1 DB "Please input a number:$"
	IBUF DB 7,0,6 DUP(0)
        OBUF1 DB 0AH,0DH,"The result is:1+2+3+...+$"
        OBUF2 DB 6 DUP(0)
        OBUF3 DB "=$"
        OBUF4 DB 6 DUP(0)
	
	;////////////////////////////////////proc2
        tribuf db '*$'
        obuf db 'Input the length:$'
        len db 7,0,6 dup(0)
        bg      db 0ah, 0dh, ' ai er lan hua mei is playing...$'
        songend db 0ah, 0dh, '      **** The End ****$'

	;//////////////////////////////////proc3
        huamei  dw 659, 523, 578, 440, 659, 578, 523, 578, 440
                dw 659, 523, 578, 578, 784, 659, 494, 523, 523, 494
                dw 440, 494, 523, 578, 392, 880, 784, 659, 659
                dw 578, 523, 578, 659, 578, 659, 578, 784, 784, 784,784,784, 784,0
        time    dw 5, 5, 5, 5, 3, 3, 3, 3, 10 
         	dw 5, 5, 5, 5, 3, 3, 5, 5, 3, 3
                dw 5, 3, 3, 5, 5, 3, 3, 5, 5
                dw 2, 5, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2,2


	;////////////////////////////////////proc4
        SPACE DB 1000 DUP (' ')
        PATTERN DB 6 DUP (' '),0C9H,26 DUP (0CDH),0BBH,6 DUP (' ')
                DB 6 DUP (' '),0BAH,26 DUP (20H),0BAH,6 DUP (' ')
                DB 6 DUP (' '),0C8H,26 DUP (0CDH),0BCH,6 DUP (' ')
        DBUFFER DB 8 DUP (':'),12 DUP (' ')
        DBUFFER1 DB 20 DUP (' ')
        STR  DB 0DH,0AH, '   DATE(D) OR TIME(T) OR QUIT(Q): $'
        
        ;/////////////////////////////////////各框的信息提示
        proc1_infor0 db 'ADD PROCEDURE$'
        proc1_infor1 db 'Input a limit number,it will add$'
        proc1_infor2 db 'until it will be over the number$'
        proc1_infor3 db 'you have inputed$'
        proc2_infor0 db 'DRAW Triangle$'
        proc2_infor1 db 'Input a length number and it will$'
        proc2_infor2 db 'draw a triangle and every length $'
        proc2_infor3 db 'is the number$'
        proc3_infor0 db 'MUSIC PLAYER$'
        proc3_infor1 db 'Ready to sing a Little Apple Song$'
        proc3_infor2 db 'Press anykey to start...$'
        ;proc3_infor3 db 'Little Apple Song End...$'
        proc4_infor0 db 'SHOW OS TIME\DATE$'
        proc4_infor1 db 'Press anykey to have a try!$'
        
data ends

code segment
        assume cs:code, ds:data,ss:stack
	start: mov ax, data
        mov ds, ax
        push ds
        mov dx, offset disp60
        mov ax, seg disp60
        mov ds, ax
        mov ah, 25h
        mov al, 60h
        int 21h
        pop ds
        int 60h
        mov ah, 4ch
        int 21h
        
disp60 proc far
	set_cursor 010bh 	;定位welcome		
	call welcome
	set_cursor 150ch        ;定位校徽
	extra_info load_mesg
	input_char		
	cmp al, 27
	jz disp_ret
        ;***************进行信息交互的部分************
	set_cursor 150ch
	extra_info null
	set_cursor 150ch
	extra_info nice_mesg
	set_cursor 160ch
	call nice
	call face
 disp_ret: iret 							;主程序返回

;-------------Welcome--------------
welcome proc near
		call clear_screen	
		extra_info well2
		extra_info well3
		extra_info well4
		extra_info well5
		extra_info well6
		extra_info well7
		set_cursor 0915h	
		extra_info well8
		extra_info well9
		extra_info wella
		extra_info wellb
		extra_info wellc
		extra_info welld
		extra_info welle
		;mov bl, 1110b        ; yellow color
		extra_info wellf
		extra_info wellg
		extra_info wellh
		ret
welcome endp

;------------------------nice---------------------------
nice proc near
	push_reg
 ;设置优先级
        mov si, offset set_pri     
        mov cx, 4h
  input_pri:
	;set_cursor 160ch
	;extra_info nice_proc
	input_char 
        mov [si], al 
        inc si
        loop input_pri

	extra_info null
	set_cursor 160ch
	extra_info start_mesg	
	input_char 

        pop_reg
        ret
nice endp

;-------------------------------face--menu---------------------------------
face proc near 
	push_reg
        mov ah,0
        mov al,2
        int 10h
        mov ah,15
        int 10h
        mov cx,0100h
        set_cursor 0100h
	extra_info buf1
show:   inc ch 			;行号增加
        cmp ch,13 	
        je show_line 		;该打印另一行了
        cmp ch,24 
        je show_line 		;打印最后一行,顺序往下执行
        set_cursor cx 		;打印小竖杠字符串
        extra_info buf3        
	jmp show
show_line:
	set_cursor cx
        extra_info buf1
	cmp ch,13
        jz show
	call pre_action     ;需要将input优先级放回nice中，将排序与其分开
	call action
	pop_reg
        ret
face endp

pre_action proc near
	push_reg
        ;形成调度序列
        mov bl, 04h
        mov di, offset exec_que
  sort: mov al, set_pri   
        mov si, offset set_pri
        mov cx, 3
       
      ;找最大数，存入al中，4个数比三次
   pre_loop1:
        inc si
        cmp al, [si]
        jae pre_loop2
        mov al, [si]
   pre_loop2:
        dec cx
        jnz pre_loop1
  ;放al所在位置的下标
        mov si, offset set_pri 
        MOV dl,01H             ;记录下标
                   
    save_sub:
        cmp al, [si]
        je save_mem                     ;二者相等则存入y
        inc si                          ;不等，继续找下一个
        inc dl
        cmp dl, 04h                     ;如果dl没到4，就接着查找
        jne save_sub
   save_mem: 				;写入内存数据段
        mov [di], dl                    ;存入y 
        mov ah, 0
        mov [si],ah                     ;被找过的值为0
        inc di                          ;y指向下个位置

         dec bl
         cmp bl, 0
         jne sort  	     
	 pop_reg
         ret
pre_action endp

;---------------------------------action------------------------------
action proc near
	push_reg
        mov si, offset exec_que 	;程序调度序列
	mov cx, 0 		        ;记录si偏移，避免越界*******

	call choose_proc
lop:    input_char
        cmp al, 27h
        jz action_ret
        cmp al, 09h
        jz choose
	;push dx
        extra_info tab_error
	jmp lop
choose: call choose_proc
	jmp lop
;--------------------------choose_proc--------------------------
choose_proc proc near
        ;push_reg
	inc cx
	cmp byte ptr [si], 1
	je call1
	cmp byte ptr [si], 2
	je call2
	cmp byte ptr  [si], 3
	je call3
        cmp byte ptr[si], 4
	je call4
call1:  call proc1
	inc si
	cmp cx, 4
	jne chooseproc_ret
        mov si, offset exec_que

call2:  call proc2
	inc si
	cmp cx, 4
	jne  chooseproc_ret 
        mov si, offset exec_que

call3:  call proc3
	inc si
	cmp cx, 4
        jne chooseproc_ret
        mov si, offset exec_que

call4:  call proc4
	inc si
	cmp cx, 4
        jne chooseproc_ret
        mov si, offset exec_que
chooseproc_ret:
        ret
choose_proc endp

action_ret:	
        pop_reg
	ret
action endp

;-------------------------------proc1-----------------------------
PROC1 PROC NEAR
    push_reg
    mov ax,data
    mov es,ax
    print_color proc1_infor0,0205h,13,04h
    print_color proc1_infor1,0302h,32,04h
    print_color proc1_infor2,0402h,32,04h
    print_color proc1_infor3,0502h,16,04h
    print_color INF1,0702h,28,04h
    MOV DX,OFFSET IBUF
    MOV AH,0AH
    INT 21H
    MOV CL,IBUF+1
    MOV CH,0
    MOV SI,OFFSET IBUF+2
    MOV AX,0
ZHUAN:MOV DX,10
    MUL DX
    AND BYTE PTR[SI],0FH
    ADD AL,[SI]
    ADC AH,0
    INC SI
    LOOP ZHUAN
    MOV DX,AX
    ; add your code here
    MOV CX,0
    MOV AX,0
    MOV BX,1
AGAIN: ADD AX,BX  
       INC BX
       CMP AX,DX
       JB AGAIN                                  
       DEC BX
       PUSH AX
       PUSH BX
       set_cursor 0802h
      MOV DX,OFFSET OBUF1
       MOV AH,09H
       INT 21H
      ; print_color OBUF1,0302h,24,04h
       POP AX
       MOV BX,OFFSET OBUF4+5
       MOV BYTE PTR [BX],'$'
       MOV CX,10
LOOP1: MOV DX,0
       DIV CX
       ADD DL,30H
       DEC BX
       MOV [BX],DL
       OR AX,AX
       JNZ LOOP1
       MOV DX,BX
       MOV AH,09H
       INT 21H
      MOV DX,OFFSET OBUF3
       MOV AH,09H
      INT 21H
       ;print_color OBUF3,031bh,1,04h
       POP AX
       MOV BX,OFFSET OBUF4+5
       MOV BYTE PTR [BX],'$'
       MOV CX,10
LOOP2: MOV DX,0
       DIV CX
       ADD DL,30H
       DEC BX
       MOV [BX],DL
       OR AX,AX
       JNZ LOOP2
       MOV DX,BX
       MOV AH,09H
       INT 21H
	pop_reg 
    ret
PROC1 ENDP
;----------------------------------proc2---------------------------- 
PROC2 PROC NEAR
	push_reg
        mov ax,data
        mov es,ax
        mov ds,ax
       print_color proc2_infor0,022fh,13,0dh
       print_color proc2_infor1,032ah,33,0dh
       print_color proc2_infor2,042ah,33,0dh
       print_color proc2_infor3,052ah,13,0dh
       print_color obuf,072fh,17,0dh
	input_str len
	;字符串转为二进制
        mov cl,len+1
        mov ch,0
        mov si,offset len+2
        mov ax,0
again2:  mov dx,10              
        mul dx
        and byte ptr [si],0fh
        add al,[si]
        adc ah,0
        inc si
        loop again2                             
        mov cx,ax  
        set_cursor 0544h
        push dx
        push dx
        mov bx,2
        mov dl,42
        mov ah,2
        int 21h
        cmp cx,1
        je undo
lmid:   pop dx
        cmp cx,bx
        je prermid
        inc bx
        mov ah,2
        inc dh
        dec dl
        int 10h
        push dx
        mov dl,42
        mov ah,2
        int 21h
        jmp lmid
prermid:mov bx,2
rmid:   pop dx
        cmp cx,bx
        je endline 
        inc bx
        mov ah,2
        inc dh
        inc dl
        int 10h
        push dx
        mov dl,42
        mov ah,2
        int 21h
        jmp rmid
endline:mov ah,2
        inc dh
        inc dl
        int 10h
        push dx
        mov dl,42
        mov ah,2
        int 21h
endll:  dec bx
        cmp bx,0
        je undo2
        pop dx
        mov ah,2
        sub dl,2
        push dx
        int 10h
        mov dl,42
        mov ah,2
        int 21h
        jmp endll
undo2:
	pop dx
        set_cursor 0537h
	jmp endproc
undo:
	pop dx
	pop dx
	jmp endproc
endproc:
	 pop_reg 
	 ret
PROC2 ENDP

;-------------------------------proc3--------------------------
PROC3 PROC NEAR
	push_reg
        mov ax,data
        mov es,ax
        mov ds,ax
        print_color proc3_infor0,0e05h,12,05h
        print_color proc3_infor1,0f02h,33,05h
        print_color proc3_infor2,1002h,24,05h
       ; print_color proc3_infor3,1102h,24,05h
	mov ah,1
	int 21h 
       mov     si, offset huamei
        mov     bp, offset time       
push di
push si
        push    bp
        push    bx
rept1:
        mov     di, [si]
        cmp     di, 0
        je      end_sing
        mov     bx, ds:[bp]
        call    sound
        add     si, 2
        add     bp, 2
        jmp     rept1
end_sing:
        pop     bx
        pop     bp
        pop     si
        pop     di
	pop_reg
        ;mov dx,offset songend
        ;mov ah,9
        ;int 21h
        print_color songend,1102h,24,05h
	ret	
sound proc near
        push    ax
        push    bx
        push    cx
        push    di
        mov     al, 0b6h
        out     43h, al
        mov     dx, 12h
        mov     ax, 34dch
        push    dx
        div     di
        out     42h, al
        mov     al, ah
        out     42h, al
        in      al, 61h
        mov     ah, al
        or      al, 3
        out     61h, al
        delayt  bx
        mov     al, ah
        out     61h, al
        pop     di
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        ret
sound endp 
PROC3 ENDP

;------------------------------------proc4--------------------------
PROC4 PROC NEAR
      ;  push_reg
        print_color proc4_infor0,0e2fh,17,02h
        print_color proc4_infor1,0f2ah,24,02h
        input_char
PROC4SON:
	MOV AX,0001H          ;设置显示方式为40*25彩色文本方式            
        INT 10H
        MOV AX,DATA
        MOV DS,AX
        MOV ES,AX
        MOV BP,OFFSET SPACE       
        MOV DX,0B00H
        MOV CX,1000
        MOV BX,0050H
        MOV AX,1300H
        INT 10H
        MOV BP,OFFSET PATTERN ;显示矩形条     
        MOV DX,0B00H
        MOV CX,120
        MOV BX,005EH
        MOV AX,1301H
        INT 10H
        LEA DX,STR            ;显示提示信息
        MOV AH,9
        INT 21H
        MOV AH,1              ;从键盘输入单个字符
        INT 21H
        CMP AL,44H            ;AL='D'？
        JNE A
        CALL DATE             ;显示系统日期
A:      CMP AL,54H            ;AL='T'？
        JNE B                 
        CALL TIME2             ;显示系统时间             
B:      CMP AL,51H            ;AL='Q'？            
        JNE PROC4SON
        push ds
        push cx
        push es
        mov ax,0b800h
        mov es,ax
        mov bx,0
        mov cx,2000
blank:  mov byte ptr es:[bx] ,' '
        mov byte ptr es:[bx+1],07h
        add bx,2
        loop blank
        pop es
        pop cx
        pop ds
        ;	pop_reg
        ret
     ;  MOV AH,4CH            ;返回dos状态
      ; INT 21H
;****************
 ; ret
;****************
DATE PROC NEAR             ;显示日期子程序
DISPLAY:MOV AH,2AH           ;取日期
       INT 21H
       MOV SI,0
       MOV AX,CX
       MOV BX,100
       DIV BL
       MOV BL,AH
       CALL BCDASC1         ;日期数值转换成相应的ASCII码字符
       MOV AL,BL
       CALL BCDASC1
       INC SI
       MOV AL,DH
       CALL BCDASC1
       INC SI
       MOV AL,DL
       CALL BCDASC1
       MOV BP,OFFSET DBUFFER1
       MOV DX,0C0DH
       MOV CX,20
       MOV BX,005EH
       MOV AX,1301H
       INT 10H
       MOV AH,02H          ;设置光标位置
       MOV DX,0300H
       MOV BH,0
       INT 10H
       MOV BX,0018H
REPEA: MOV CX,0FFFFH       ;延时
REPEAT:LOOP REPEAT
       DEC BX
       JNZ REPEA
       MOV AH,01H          ;读键盘缓冲区字符到AL寄存器
       INT 16H
       JE  DISPLAY
       JMP PROC4SON
       MOV AX,4C00H
       INT 21H
       RET
DATE ENDP

TIME2 PROC NEAR        ;显示时间子程序        ;;;;;;;;;;;;;;;;error
DISPLAY1:MOV SI,0
       MOV BX,100
       DIV BL
       MOV AH,2CH       ;取时间
       INT 21H
       MOV AL,CH
       CALL BCDASC      ;将时间数值转换成ASCII码字符
       INC SI
       MOV AL,CL
       CALL BCDASC
       INC SI
       MOV AL,DH
       CALL BCDASC
       MOV BP,OFFSET DBUFFER
       MOV DX,0C0DH
       MOV CX,20
       MOV BX,005EH
       MOV AX,1301H
       INT 10H
       MOV AH,02H
       MOV DX,0300H
       MOV BH,0
       INT 10H
       MOV BX,0018H
RE:    MOV CX,0FFFFH
REA:   LOOP REA
       DEC BX
       JNZ RE
       MOV AH,01H
       INT 16H
       JE  DISPLAY1
       JMP PROC4SON
       MOV AX,4C00H
       INT 21H
       RET
TIME2 ENDP                                               ;;;;;;;;;;;;;error

BCDASC PROC NEAR                ;时间数值转换成ASCII码字符子程序
       PUSH BX
       CBW
       MOV BL,10
       DIV BL
       ADD AL,'0'
       MOV DBUFFER[SI],AL
       INC SI
       ADD AH,'0'
       MOV DBUFFER[SI],AH
       INC SI
       POP BX
       RET
BCDASC ENDP

BCDASC1 PROC NEAR              ;日期数值转换成ASCII码字符子程序
       PUSH BX
       CBW
       MOV BL,10
       DIV BL
       ADD AL,'0'
       MOV DBUFFER1[SI],AL
       INC SI
       ADD AH,'0'
       MOV DBUFFER1[SI],AH
       INC SI
       POP BX
       RET
BCDASC1 ENDP
PROC4 ENDP   

;--------------------------clear_screen----------------------------
clear_screen proc near
		push ax
		push bx
		push cx
		push dx
			 
		mov ah, 06h
		mov al, 0   
		mov al, 0   
		mov ch, 0   
		mov cl, 0   
		mov dh, 24  
		mov dl, 79  
		mov bh, 7  
		int 10h
		pop dx
		pop cx
		pop bx
		pop ax
		ret
clear_screen  endp
 

;------------------------------------blink-------------------------
BLINK PROC NEAR
        push_reg
        push dx
        MOV CX,34
   BLINKSON1:
        PUSH CX
        DEC CX
        MOV AH,2
        INT 10H
        MOV AH,08H
        INT 10H
        OR AH,80H
        MOV AH,09H
        MOV BL,AH
        MOV CX,1
        INT 10H
        INC DL
        ;DEC CX
        POP CX
        LOOP BLINKSON1
        pop dx
        MOV CX,12
   BLINKSON2:
	PUSH CX
        DEC CX
        MOV AH,2
        INT 10H
        MOV AH,08H
        INT 10H
        OR AH,80H
        MOV AH,09H
        MOV BL,AH
        MOV CX,1
        INT 10H
        INC DH
        POP CX
        LOOP BLINKSON2
        pop_reg
        RET
BLINK ENDP

                         

E:   mov ah,4ch
     int 21h   
 
disp60 endp
code ends
        END START
