;
;	; kernel.asm — Manual H Drawing Test
bits 32
org 0x1000


section .bss
    alignb 16
    idt_table:          resb 256 * 8        ; 256 entries × 8 bytes
    key_a_state:  resb 1              ; 0 = not pressed, 1 = pressed
    key_states: resb 128
    vbe_info       resb 512
        framebuffer    resd 1

section .data
    idtr:
        dw 256*8 - 1                   ; limit
        dd idt_table                         ; base address

   Hello db "Franco is GAY FAGGOT",0
   big_title   db "CHECKMATE OS",0
   subtitle    db "v1.0 - PURE 32-BIT",0
   press_key   db "PRESS ANY KEY TO BEGIN",0
 
  hellos db "Hey",0
  char_x dd 10
  char_y dd 10
  shft db 0
  text_buffer times 80*25*8 db 0
  cursor_x dd 0
  cursor_y dd 10
	cursor_offset dd 0



 

  mouse_x:        dw 160      ; Current X position (center)
  mouse_y:        dw 100      ; Current Y position (center)
  old_mouse_x:    dw 160      ; Last frame's X position
  old_mouse_y:    dw 100      ; Last frame's Y position

  last_scan_code db 0
  ; change any 1 to 2 for black outline if you want
section .text
; =====================================================
; kernel.asm — protected mode kernel template
; Loaded at 0x1000 by bootloader
; =====================================================

; =====================================================
; kernel.asm — protected mode kernel for mode 13h
; Loaded at 0x1000 by bootloader
; Handles IDT, keyboard IRQ1, and graphics mode
; =====================================================

;bits 32
;org 0x1000

kernel_entry:

    ; ----------------------
    ; Reload segment registers and stack
    ; ----------------------
    mov ax, 0x10        ; data segment selector (GDT entry 2)
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x9FC00    ; stack top

    ; ----------------------
    ; Setup IDT
    ; ----------------------
    cli
    call init_idt
    
    call remap_pic
    sti                  ; enable interrupts safely (IRQ1 unmasked)

    ; ----------------------
    ; Main loop
    ; ----------------------
    mov edi, 0xA0000
    		    mov ecx, 320*200/4
    		    mov eax, 0x01010101

   		    rep stosd
mov ax,80
	mov bx,100
	mov cx,10
mov dl,-1
.main_loop:
	
	
	jmp .main_loop
	add dl,1
	add ax,20
	add bx,20
	cmp dl,10
	je reset1
	jmp done1
	
reset1:
	mov dl,0
	mov ax,-20
	mov bx,-20
	jmp done1
done1:
	call draw_char_C
	add dl,1
    jmp kernel_entry.main_loop       ; kernel idle, waits for keyboard IRQ

; =====================================================
; IDT definition (256 entries, all point to dummy ISR)
; =====================================================
idt_ptr:
    dw idt_end - idt - 1
    dd idt

idt:
times 256 dq 0
idt_end:

; =====================================================
; Initialize IDT
; =====================================================
init_idt:
    mov ecx, 256          ; 256 entries
    mov esi, idt
    mov eax, dummy_isr
.setup_loop:
    mov word [esi], ax         ; offset low
    mov word [esi + 2], 0x08   ; code segment selector
    mov byte [esi + 4], 0      ; always zero
    mov byte [esi + 5], 0x8E   ; type_attr = interrupt gate
    shr eax, 16
    mov word [esi + 6], ax     ; offset high
    add esi, 8
    loop .setup_loop

    ; Replace entry for IRQ1 (keyboard)
    mov eax, keyboard_isr
    mov word [idt + 0x21*8], ax        ; offset low
    mov word [idt + 0x21*8 + 2], 0x08 ; code segment selector
    mov byte [idt + 0x21*8 + 4], 0    ; always zero
    mov byte [idt + 0x21*8 + 5], 0x8E ; type_attr
    shr eax, 16
    mov word [idt + 0x21*8 + 6], ax   ; offset high

    ; Hook IRQ12 → mouse (vector 0x2C)
    ;    mov eax, mouse_isr
     ;   mov word [idt + 0x2C*8 + 0], ax          ; offset low
      ;  mov word [idt + 0x2C*8 + 2], 0x08        ; code segment
       ; mov byte [idt + 0x2C*8 + 4], 0
       ; mov byte [idt + 0x2C*8 + 5], 0x8E        ; interrupt gate
        ;shr eax, 16
        ;mov word [idt + 0x2C*8 + 6], ax          ; offset high

    lidt [idt_ptr]
    ret

; =====================================================
; Dummy ISR for all unhandled interrupts
; =====================================================
dummy_isr:
    iret

; =====================================================
; Keyboard ISR (IRQ1)
; =====================================================
keyboard_isr:
    pushad

    in al, 0x60          ; read scancode
    call key_pressed_handler

    
    mov al, 0x20
    out 0x20, al         ; master PIC


    popad
    iret

; =====================================================
; Example key pressed handler
; =====================================================
key_pressed_handler:
    	
	
;	in al, 0x64     ; Check status port (0x64)
;	test al, 1      ; Is the Output Buffer full? (Bit 0 set?)	
;jz .no_key      ; <--- If NOT full (ZF=1), skip the read and exit.
	
	; --- Key is Available (Read and Process Scan Code) ---
	;in al, 0x60     ; Read the scan code
	;test al, 0x80   ; Is it a break code (key up)?
	;jnz .key_up     ; If yes (key up), ignore and exit.

	;in al, 0x60
	
	;mov [last_scan_code], al     ; save it somewhere in memory

	;push last_scan_code
	;push 0
	;push 100
	;push 100
	;call put_str
	;add esp,16


	; --- Key Down Logic Starts Here ---
	;Key,Make Code (Key Down),Break Code (Key Up)
	;Up Arrow (↑),E0 48,E0 C8
	;Down Arrow (↓),E0 50,E0 D0
	;Left Arrow (←),E0 4B,E0 CB
	;Right Arrow (→),E0 4D,E0 CD
	; CMP/JE/etc. to check for a specific key (like 0x1D for Ctrl)
	cmp al, 0xE0
	je .ignore        ; ignore the prefix completely
	
	cmp al, 0xCB       ; ignore left-arrow break
	je .ignore
	cmp al, 0x4B	
	    je .left_arrow   ; If it's E0, go handle the next byte
	cmp al,0x0E
	je .backspace


	
	cmp al,0x1E
	je .A_Key ;rel 0x9E

	;cmp al,0x30
	;jl .B_Key

	;cmp al,0x2E
	;je .C_Key

	

		
	;cmp al, 0x1D 
	;je .ctrl_press

	;cmp al,0x23
	;je .H_Key

	cmp al, 0xAA         ; left shift release
	je .left_shift_up

	cmp al,0x2A
	je .shft ;0xAA

	
	
	ret

.left_arrow:
	mov eax, [cursor_x]
	sub eax, 20
	mov [cursor_x], eax	

	sub dword [cursor_offset],8		
	ret
.ignore:
	mov bl,1
	ret	
.key_up:
;cmp al,0xAA
;je .shft_up
ret
	;jmp .done       ; Exit after processing the key
.backspace:
    ; move back one cell
    sub dword [cursor_offset], 8

    ; compute address in buffer
    mov eax, [cursor_offset]      ; get offset
    add eax, text_buffer          ; compute address (AL is enough if <256 bytes, otherwise use AX/EAX)
    
    ; read the previous char and color
    mov ebx, dword [eax]                 ; original char
    mov ecx, dword [eax+4]               ; original color

    ; overwrite char/color in buffer
    ;mov byte [eax], 0             ; erase char
    ;mov byte [eax+1], 0           ; erase color

    ; move cursor on screen
    mov eax, [cursor_x]
    sub eax, 20
    mov [cursor_x], eax

    ; redraw the erased character (black)
    
    push  ebx               ; char (or you could use bl if you want to keep it)
    push 1                       ; color (black)
    mov eax, [cursor_y]
    push eax
    mov eax, [cursor_x]
    push eax
    call put_str
    add esp, 16
    ret

.left_shift_up:
	mov byte[shft],0
	ret
	;jmp .done
.shft:

	mov byte [shft],1
	ret
	;jmp .done

.A_Key:
push ebx
movzx ebx, byte [shft]
cmp ebx,1
pop ebx
je .A_shft


jmp .normala


.A_shft:
	;mov eax,A
	mov ecx,A
	push A

	jmp .do_a
	

.normala:
	;mov eax,a
	mov ecx,a
	push a

.do_a:
	mov eax, [cursor_offset]   ; get buffer offset
	        add eax, text_buffer       ; compute address
	    
	        mov [eax], ecx             ; store pointer to char (4 bytes)
	        mov [eax+4], edi           ; store pointer to color (4 bytes)
	    
	        add dword [cursor_offset], 8   ; move to next cell
	
		
	push 10
	mov eax,[cursor_y]
	push eax
		mov eax,[cursor_x]
		push eax
		call put_str
		add esp,16
		
		mov eax,[cursor_x]
		cmp eax,290
		jl a_same
		
	a_x:
		mov eax,0
		mov dword [cursor_x],eax
	
		mov eax,[cursor_y]
		add eax,20
		mov dword [cursor_y],eax
		ret
	a_same:
		mov eax,[cursor_x]
		add eax,20
		mov dword [cursor_x],eax
		ret


.B_Key:
push ebx
mov ebx,[shft]
cmp ebx,1
pop ebx
je .B_shft


jmp .normalb


.B_shft:
	mov eax,B
	
	ret

.normalb:
	mov eax,b
	
	ret


.C_Key:
push ebx
mov ebx,[shft]
cmp ebx,1
pop ebx
je .C_shft


jmp .normalc


.C_shft:
	mov eax,C
	
	ret

.normalc:
	mov eax,c
	
	ret
	
.H_Key:

	mov ax,100
	mov bx,10
	mov cx,5
	mov dl,10
	call draw_char_H
	mov eax,H
	ret
	jmp .done
.no_key:
	; Code executes here if no key was pressed or if it was a key-up event.
	cmp al,0xAA
		;je .shft_up
	jmp .done
.ctrl_press:
	jmp .done
.done:
	ret

.char_data:
	a db "a",0
	b db "b",0
	c db "c",0
	d db "d",0
	e db "e",0
	f db "f",0
	g db "g",0
	h db "h",0
	i db "i",0
	j db "j",0
	k db "k",0
	l db "l",0
	m db 'm',0
	n db 'n',0
	o db 'o',0
	p db 'p',0
	q db 'q',0
	r db 'r',0
	s db 's',0
	t db 't',0
	u db 'u',0
	v db 'v',0
	w db 'w',0
	x db 'x',0
	y db 'y',0
	z db 'z',0



	
	A db "A",0
	B db "B",0
	C db "C",0
	D db "D",0
	E db "E",0
	F db "F",0
	G db "G",0
	H db "H",0
	I db "I",0
	J db "J",0
	K db "K",0
	L db "L",0
	M db "M",0
	N db "N",0
	O db "O",0
	P db "P",0
	Q db "Q",0
	R db "R",0
	S db "S",0
	T db "T",0
	U db "U",0
	V db "V",0
	W db "W",0
	X db "X",0
	Y db "Y",0
	Z db "Z",0
    




; =====================================================
; Remap PIC
; =====================================================
remap_pic:
    ; Start initialization
    mov al, 0x11
    out 0x20, al
    out 0xA0, al

    ; Vector offsets
    mov al, 0x20          ; master offset 0x20
    out 0x21, al
    mov al, 0x28          ; slave offset 0x28
    out 0xA1, al

    ; Wiring
    mov al, 0x04
    out 0x21, al
    mov al, 0x02
    out 0xA1, al

    ; 8086 mode
    mov al, 0x01
    out 0x21, al
    out 0xA1, al

    ; Mask all IRQs except IRQ1 (keyboard)
    mov al, 0xFD          ; 11111101 → unmask IRQ1
    out 0x21, al
    mov al, 0xBF          ; 10111111 → IRQ12 enabled on slave (bit 4 = IRQ12)
       out 0xA1, al

    in al, 0xA1           ; slave mask
        and al, 0xEF          ; clear bit 4 = enable IRQ12 (mouse)
         out 0xA1, al
    ;mov al, 0xFF
    ;out 0xA1, al
    ret


start:


	
	
    ; black background
    mov edi, 0xA0000
    mov ecx, 320*200/4
    mov eax, 0x01010101
    rep stosd
      ; freeze CPU
	;call init_mouse
;	call unmask_irq12
main:
	;call update_cursor
	call read_key_pm_non_block
	cmp eax,0x0
	je main
	
	push eax
	push 10
	mov eax,[char_y]
	push eax
	mov eax,[char_x]
	push eax
	call put_str
	add esp,16
	
	mov eax,[char_x]
	cmp eax,290
	jl same

no:
	mov eax,10
	mov dword [char_x],eax

	mov ebx,[char_y]
	add ebx,20
	mov  [char_y],ebx
	
	jmp main
	
	
same:
	add eax,20
	mov [char_x],eax
	
	jmp main



	
    ; title — huge yellow
    push big_title
    push 40
    push 60
    push 14
    call put_str

	add esp,16
    ; subtitle — white
    push subtitle
    push 70
    push 90
    push 15
    call put_str
	add esp,16
    ; press any key — blinking green
   	jmp $
;start:
		
	
		
    ; Fill screen with palette index 200 (blue-ish)
    ;mov edi, 0xA0000       ; VGA framebuffer start
    
    ; 1️⃣ Set palette 200 to RGB(63,7,62)
    ;mov al, 0xC8
    ;out 0x3C8, al          ; select palette index
    
    ;mov al, 63              ; Red
    ;out 0x3C9, al
    
    ;mov al, 7               ; Green
    ;out 0x3C9, al
    
    ;mov al, 62              ; Blue
    ;out 0x3C9, al
    
    ;STEP 1 — FILL WITH COLOR 12 (default bright red)
        mov edi, 0xA0000
        mov ecx, 320*200 / 4
        mov eax, 0x0C0C0C0C          ; 12 = 0x0C
        rep stosd


		
    
		; Colour 4 = pure red (for errors)
		    mov al, 20
		    mov bh, 63
		    mov bl, 34
		    mov ah, 0
		    call set_color

    
    
                ; screen MUST now be bright green
	
	
	push Hello
	push 20
	push 100
	push 0

	call put_str
	
	add esp, 16     ; 4 arguments * 4 bytes/arg = 16 bytes
	jmp $
	mov ax, 100     ; X position
		    mov bx,0           ; Y position
		    mov dl, 10             ; Color (White)
			;mov bl,5
		    mov al, 0x39        ; <--- Use character code 0x00 (the 'A')
		    
		    ; Note: The size (CX) might be needed depending on your putchar_gfx definition
		    ; mov cx, 16            ; Set height/size if your function uses it
		
		call putchar_gfx
		;call put_font_char_8_16		
		
	   
    ; --- Draw H Manually ---
    
    mov dl, 0 ; Color (White)

    mov ax,100
    mov bx,100
    mov cx,2

	call draw_char_H

	mov ax,10
	mov bx,10
	mov cx,20
	call draw_char_C



	    
	mov ax,0
	mov bx,0
	mov cx,100
	mov dl,0
    call draw_diagonal_line
    
    mov ax,0
    	mov bx,1
    	mov cx,100
    	mov dl,0
        call draw_diagonal_line
        
   mov ax,120
       	mov bx,100
       	mov cx,20
       	mov dl,0
           call draw_char_A

   mov ax,0
   mov bx,100
   mov cx,10
   mov dl,10
   call draw_char_L
	
mov ax,80
mov bx,100
mov cx,10

Loop1:
	;pusha
	;call read_key_pm_non_block
	;popa
	push eax
	add bx,2
	cmp bx,160
	mov eax,0
	cmove bx,ax
	pop eax
		
	call draw_char_C
	
	cmp dl,0xC8
	jne Blue
White:
	mov dl,10
	jmp Loop1
Blue:
	call delay_1_sec
	mov dl,0xC8
	call draw_char_C

	jmp Loop1

	
    
        jmp $







set_mode_13h:
    ; Misc Output
    mov dx, 0x3C2
    mov al, 0x63
    out dx, al

    ; Sequencer — THE EXACT SEQUENCE THAT WORKS
    mov dx, 0x3C4
    mov ax, 0x0100
    out dx, ax          ; reset
    mov ax, 0x0300
    out dx, ax          ; reset off
    mov ax, 0x0604      ; ← disable Chain4 = 256-colour linear
    out dx, ax
    mov ax, 0x0F02      ; enable all planes
    out dx, ax

    ; Unlock CRTC
    mov dx, 0x3D4
    mov al, 0x11
    out dx, al
    inc dx
    in al, dx
    and al, 0x7F
    dec dx
    out dx, al
    dec dx

    ; CRTC register table
    mov si, crtc_data
    mov cx, 25
.crtc_loop:
    lodsw
    out dx, ax
    loop .crtc_loop

    ; Graphics Controller
    mov dx, 0x3CE
    mov ax, 0x0005
    out dx, ax
    mov ax, 0x4005
    out dx, ax
    mov ax, 0x0F01
    out dx, ax

    ret

; ================================================
; CRTC DATA — THIS MUST BE HERE OR NASM SAYS "NOT INITIALIZED"
; ================================================
crtc_data:
    dw 0x5F00, 0x4F01, 0x5002, 0x8203, 0x5404, 0x8005, 0xBF06
    dw 0x1F07, 0x0008, 0x4109, 0x000A, 0x000B, 0x000C, 0x000D
    dw 0x000E, 0x000F, 0x9C10, 0x8E11, 0x8F12, 0x2813, 0x0014
    dw 0x9615, 0xB916, 0xE317, 0xFF18
; -------------------------------------------------
; CHANGE PALETTE COLOR — GUARANTEED TO WORK
; AL = palette index (0–255)
; BH = Red   (0–63)
; BL = Green (0–63)
; AH = Blue  (0–63)
; -------------------------------------------------
set_color:
    push ax
    push dx

    mov dx, 0x3C8
    out dx, al          ; tell VGA which colour we’re changing

    inc dx              ; 0x3C9

    mov al, bh
    out dx, al          ; Red
    mov al, bl
    out dx, al          ; Green
    mov al, ah
    out dx, al          ; Blue

    pop dx
    pop ax
    ret

read_key_pm_non_block:
		
	
	in al, 0x64     ; Check status port (0x64)
	test al, 1      ; Is the Output Buffer full? (Bit 0 set?)
	jz .no_key      ; <--- If NOT full (ZF=1), skip the read and exit.
	
	; --- Key is Available (Read and Process Scan Code) ---
	in al, 0x60     ; Read the scan code
	test al, 0x80   ; Is it a break code (key up)?
	jnz .no_key     ; If yes (key up), ignore and exit.
	
	; --- Key Down Logic Starts Here ---
	
	; CMP/JE/etc. to check for a specific key (like 0x1D for Ctrl)

	cmp al,0x1E
	je .A_Key ;rel 0x9E

	cmp al,0x30
	je .B_Key

	cmp al,0x2E
	je .C_Key

	

		
	cmp al, 0x1D 
	je .ctrl_press

	cmp al,0x23
	je .H_Key

	cmp al,0x001E
	je .A_Key ;rel 0x9E

	cmp al,0x2A
	je .shft ;0xAA

	
	
	jmp .done       ; Exit after processing the key
	
.shft_up:
	mov byte[shft],0
	
	jmp .done
.shft:

	mov byte [shft],1
	jmp .done

.A_Key:
push ebx
mov ebx,[shft]
cmp ebx,1
pop ebx
je .A_shft


jmp .normala


.A_shft:
	mov eax,A
	
	ret

.normala:
	mov eax,a
	
	ret


.B_Key:
push ebx
mov ebx,[shft]
cmp ebx,1
pop ebx
je .B_shft


jmp .normalb


.B_shft:
	mov eax,B
	
	ret

.normalb:
	mov eax,b
	
	ret


.C_Key:
push ebx
mov ebx,[shft]
cmp ebx,1
pop ebx
je .C_shft


jmp .normalc


.C_shft:
	mov eax,C
	
	ret

.normalc:
	mov eax,c
	
	ret
	
.H_Key:

	mov ax,100
	mov bx,10
	mov cx,5
	mov dl,10
	call draw_char_H
	mov eax,H
	ret
	jmp .done
.no_key:
	; Code executes here if no key was pressed or if it was a key-up event.
	cmp al,0xAA
		je .shft_up
	jmp .done
.ctrl_press:
	jmp .done
.done:
	mov eax,0x0
	ret

.char_data1:
	

		
keyboard_handler:
	pushad
    push eax

    in al, 0x60
    movzx eax, al

    test al, 0x80
    jnz .key_up

.key_down:
    and eax, 0x7F
    mov byte [key_states + eax], 1
    jmp .done

.key_up:
    and eax, 0x7F
    mov byte [key_states + eax], 0

.done:
    mov al, 0x20
    out 0x20, al
    pop eax
    popad
    iret
unmask_irq1:
    in al, 0x21
    and al, 0xFD        ; clear bit 1 → enable IRQ1
    out 0x21, al
    ret
  
remap_pic1:
    mov al, 0x11
    out 0x20, al
    out 0xA0, al
    mov al, 0x20        ; master offset = 0x20
    out 0x21, al
    mov al, 0x28        ; slave offset = 0x28
    out 0xA1, al
    mov al, 0x04
    out 0x21, al
    mov al, 0x02
    out 0xA1, al
    mov al, 0x01
    out 0x21, al
    out 0xA1, al
    xor al, al
    out 0x21, al        ; unmask master
    out 0xA1, al        ; unmask slave
    ret

; set_idt_entry index, handler
; input: eax = index, ebx = handler address
set_idt_entry:
    ; entry = idt_table + index*8

    shl eax, 3                     ; eax *= 8  (each entry 8 bytes)
    mov edx, idt_table
    add edx, eax                   ; edx = entry address

    ; write offset low
    mov ax, bx
    mov [edx], ax

    ; selector = 0x08 (kernel code)
    mov word [edx+2], 0x08

    ; type = 0x8E (present, ring0, 32bit interrupt gate)
    mov byte [edx+4], 0
    mov byte [edx+5], 0x8E

    ; offset high
    shr ebx, 16
    mov word [edx+6], bx

    ret


draw_diagonal_line_reverse:
	;/
        ; Save caller's registers that will be used for calculation (EAX, EBX)
        push eax
        push ebx
        push ecx  
        
        ; --- 1. Set Up State (Use ESI/EBP for safe tracking) ---
        movzx ebp, bx           ; EBP = Y coordinate (safe 32-bit storage)
        movzx esi, ax           ; ESI = X coordinate (safe 32-bit storage)
        

       
        
       movzx eax, cx           ; EAX = Length (from input CX)
        
    .Loops:
    	push eax
        ; --- 2. Set Pixel Inputs ---
        mov ax, si              ; AX = Current X (from ESI)
        mov bx, bp              ; BX = Current Y (from EBP)
        
        call Set_Pixel          ; Assuming Set_Pixel is safe
        
        ; --- 3. Update Coordinates and Count ---
        inc esi                 ; X = X + 1
        dec ebp                 ; Y = Y + 1
        
        pop eax
        dec eax                ; Decrement counter
        cmp eax, 0              
        jge .Loops
        
        ; --- 4. Clean up and Return ---
        ; Restore registers in REVERSE order of push
        pop ecx                 
        pop ebx                 
        pop eax                 
        
        ret                     ; Returns successfully
draw_diagonal_line:
	;\
        ; Save caller's registers that will be used for calculation (EAX, EBX)
        push eax
        push ebx
        push ecx  
        
        ; --- 1. Set Up State (Use ESI/EBP for safe tracking) ---
        movzx ebp, bx           ; EBP = Y coordinate (safe 32-bit storage)
        movzx esi, ax           ; ESI = X coordinate (safe 32-bit storage)
        
        ; EAX = Loop Counter (Length) - Use EAX for counting!
        movzx eax, cx           ; EAX = Length (from input CX)
        
    .Loops:
    	push eax
        ; --- 2. Set Pixel Inputs ---
        mov ax, si              ; AX = Current X (from ESI)
        mov bx, bp              ; BX = Current Y (from EBP)
        
        call Set_Pixel          ; Assuming Set_Pixel is safe
        
        ; --- 3. Update Coordinates and Count ---
        inc esi                 ; X = X + 1
        inc ebp                 ; Y = Y + 1
        
        pop eax
        dec eax                ; Decrement counter
        cmp eax, 0              
        jge .Loops
        
        ; --- 4. Clean up and Return ---
        ; Restore registers in REVERSE order of push
        pop ecx                 
        pop ebx                 
        pop eax                 
        
        ret                     ; Returns successfully


draw_char_A:

	pusha


	movzx esi,ax
	movzx ebp,bx
	movzx ebx,cx
	shl ebx,1

	push ecx
	        
	        shl ecx,1
	       movzx ecx,cx
	       add ebp,ecx
	       pop ecx
	
	mov ax,si
	mov cx,bx



	
	
	push ebx
	mov bx,bp
	call  draw_diagonal_line_reverse
	pop ebx
	


	mov ax,si
	mov cx,bx
	

	
	push ebx
	mov bx,bp
	
	call  draw_diagonal_line
	pop ebx



	mov ax,si
	mov cx,bx
	
	
	

	popa
	;after here ebx isnt 2x
	
	
	pusha
	
	;shl ebx,1
	
	
	
	shr ebx,1
	push ebx
	
	mov bx,bp
	add bx,cx


	add ax,cx
	

	push eax
	mov ax,cx
	shr eax,2
	sub cx,ax
	sub cx,ax

	pop eax
	
	add cx,cx	
	add cx,cx	
		

	
	call  draw_h_line
	pop ebx
	
	popa


	ret        
draw_char_N:
 pusha
    
    ; 1. Store Inputs (Make them safe 32-bit variables)
    movzx esi, ax           ; ESI = Base X Position (e.g., 50)
    movzx ebp, bx           ; EBP = Base Y Position (e.g., 80)
    movzx ebx,cx
    shl ebx,1   ;;*2
    ; We are assuming the horizontal bar is 7 pixels below the start (87)
    ; and the right bar is 7 pixels right of the start (57).
    
    ; --- 1. Draw LEFT vertical bar ---
    
    ; Setup Inputs for draw_v_line: AX=X, BX=Y, DL=Color
    mov ax, si              ; AX = ESI (Base X position)
    mov cx,bx
    push ebx
    mov bx, bp              ; BX = EBP (Base Y position)
    
    ; DL is already set to color
    call draw_v_line
    pop ebx
	
    ; --- 2. Draw RIGHT vertical bar ---
    
    ; X position = Base X + 7 pixels (8 pixels wide character)
    mov eax, esi            ; EAX = Base X
    ;add eax, 16             ; EAX = Base X + 7 (e.g., 50 + 7 = 57)
    mov ax, si              ; AX = ESI (Base X position)
    
    add ax,bx
    
    ;mov cx,bx
    
    push ebx
    mov bx, bp              ; BX = EBP (Base Y position)
    call draw_v_line
    pop ebx

	; --- Inside draw_char_N, where the DIAGONAL calculation goes ---
	
	    ; X position: Starts at Base X + 1
	    mov ax, si                 ; AX = Base X
	    inc ax                     ; AX = Base X + 1
	    mov esi,ebx
	    ; Y position: Starts at Base Y
	    mov bx, bp                 ; BX = Base Y
	
	    ; --- CALCULATE DIAGONAL LENGTH ---
	    
	    
	    ; Step 1: Get the side length (L_side = EBX / 2)
	    push edx                ; Save EDX (which holds color DL)
	    push eax
	        mov eax, esi            ; EAX = Scaled Width (L_side = 10)
	        
	        ; Multiply by 3 and divide by 2: (10 * 3) / 2 = 15
	        ;mov cl, 2               ; Use CL for multiplier
	        ;imul cl                 ; AX = AX * 2 (20)
	        ;shr ax, 1               ; AX = AX / 2 (10)
	    	sub ax,1
	        mov cx, ax              ; CX = Final Loop Count (15)
	        pop eax
	        pop edx                 ; Restore EDX                  ; Restore EDX
	
	    ; --- Call Drawing Routine ---
	    call draw_diagonal_line
    popa
    ret



   	   
draw_char_C:
    pusha
    
    ; 1. Store Inputs (ESI=X, EBP=Y)
    movzx esi, ax           ; ESI = Base X Position (e.g., 50)
    movzx ebp, bx           ; EBP = Base Y Position (e.g., 80)
    movzx ebx,cx
    ; DL is already set to color
    
    ; --- 1. Draw TOP Horizontal Segment (Curving in) ---
    
    ;Adds some space away from | 
    mov ax, si
    push ebx
    ;pushes it releative to the right
   	 shr ebx,2
    add ax, bx              ; AX = X + 3 (e.g., 53)
    pop ebx
    
	mov cx, bx               ; CX = Width
	
	
    push ebx
    ; Y position: Base Y (Top row)
    mov bx, bp              ; BX = Base Y (e.g., 80)



	        
  
    ;;Draws the top part
    call draw_h_line        
    
    pop ebx
    ; --- 2. Draw MAIN Vertical Bar ---
    
    ; makes the bar longer 1.5 times
    mov ax, si              ; AX = Base X
    push ebx
    mov cx, bx               ; CX = Length
    shl cx,1
   
    shr ebx,1
    sub cx,bx
   	pop ebx 
   	
    ; Y position Pushes the bar down a bit
	push ebx
    mov bx, bp
   	push ecx
   	shr ecx,2
   	        ;pushes it down the V line | relative
   	add bx, cx              ; AX = X + 3 (e.g., 53)
   	 pop ecx
                  ; BX = Base Y + 4
    
    
    
    call draw_v_line        
    pop ebx
    ; --- 3. Draw BOTTOM Horizontal Segment (Curving in) ---
    
    ; moves the _ line to the side as done with the top Part
    mov ax, si
    push ebx
    shr ebx,2
    add ax, bx              ; AX = X + 3 (e.g., 53)
    pop ebx
    
    mov cx, bx
    
    push ebx
    ;pushes it down by the lenght of | 1,5
    mov bx, bp
   	
   	push ecx
	shl cx,1
	add bx,cx	

   	pop ecx
    ; Draw a short 4-pixel line
               
    call draw_h_line
    pop ebx
    
    popa
    ret


draw_char_L:
	pusha

	movzx esi, ax           ; ESI = Base X Position (e.g., 50)
	movzx ebp, bx           ; EBP = Base Y Position (e.g., 80)
	movzx ebx,cx
	shl ebx,1


	mov ax,si
	mov cx,bx
	push ebx
	mov bx,bp
	call draw_v_line
	pop ebx
	
	mov cx,bx
	mov ax,si
		
	push ebx
	mov bx,bp
	add bx,cx
	call draw_h_line
	pop ebx


	popa


	ret
draw_char_E:
	pusha
	movzx esi, ax           ; ESI = Base X Position (e.g., 50)
	movzx ebp, bx           ; EBP = Base Y Position (e.g., 80)
	movzx ebx,cx

	mov ax,si

	mov cx,bx
	
	push ebx
	mov bx,bp
	call draw_char_F
	pop ebx
	
	shl ebx,1

	mov cx,bx
	mov ax,si
	
	push ebx
	mov bx,bp
	add bx,cx
	call draw_h_line
	pop ebx
	


	popa

	ret
draw_char_F:
	pusha
	    
	    ; 1. Store Inputs (ESI=X, EBP=Y)
	    movzx esi, ax           ; ESI = Base X Position (e.g., 50)
	    movzx ebp, bx           ; EBP = Base Y Position (e.g., 80)
	    movzx ebx,cx
	    shl ebx,1
	    ; DL is already set to color
	    
	    ; --- 1. Draw TOP Horizontal Segment (Curving in) ---
	    
	    ;Adds some space away from | 
	    mov ax, si
	    
	    
		mov cx, bx               ; CX = Width
		
		
	    push ebx
	    ; Y position: Base Y (Top row)
	    mov bx, bp              ; BX = Base Y (e.g., 80)
	
	
	
		        
	  
	    ;;Draws the top part
	    call draw_h_line        
	    
	    pop ebx
	    ; --- 2. Draw MAIN Vertical Bar ---
	    
	    ; makes the bar longer 1.5 times
	    mov ax, si              ; AX = Base X
	    ;push ebx
	    ;mov cx, bx               ; CX = Length
	    ;shl cx,1
	   
	    ;shr ebx,1
	    ;sub cx,bx
	   	;pop ebx 
	   	
	    ; Y position Pushes the bar down a bit
		push ebx
	    mov bx, bp
	   	
	                  ; BX = Base Y + 4
	    
	    
	    
	    call draw_v_line        
	    pop ebx
	    ; --- 3. Draw BOTTOM Horizontal Segment (Curving in) ---
	    
	    ; moves the _ line to the side as done with the top Part
	    mov ax, si
	    
	    
	    mov cx, bx
	    
	    push ebx
	    ;pushes it down by the lenght of | 1,5
	    mov bx, bp
	   	push ecx
		shr ecx,1
		add bx,cx

	   	pop ecx
	   
	    ; Draw a short 4-pixel line
	               
	    call draw_h_line
	    pop ebx
	    
	    popa
	    ret




    
    
draw_char_H:
    pusha
    
    ; 1. Store Inputs (Make them safe 32-bit variables)
    movzx esi, ax           ; ESI = Base X Position (e.g., 50)
    movzx ebp, bx           ; EBP = Base Y Position (e.g., 80)
    movzx ebx,cx
    shl ebx,1   ;;*2
    ; We are assuming the horizontal bar is 7 pixels below the start (87)
    ; and the right bar is 7 pixels right of the start (57).
    
    ; --- 1. Draw LEFT vertical bar ---
    
    ; Setup Inputs for draw_v_line: AX=X, BX=Y, DL=Color
    mov ax, si              ; AX = ESI (Base X position)
    mov cx,bx
    push ebx
    mov bx, bp              ; BX = EBP (Base Y position)
    
    ; DL is already set to color
    call draw_v_line
    pop ebx


    
    ; --- 2. Draw RIGHT vertical bar ---
    
    ; X position = Base X + 7 pixels (8 pixels wide character)
    mov eax, esi            ; EAX = Base X
    ;add eax, 16             ; EAX = Base X + 7 (e.g., 50 + 7 = 57)
    mov ax, si              ; AX = ESI (Base X position)
    
    add ax,bx
    
    ;mov cx,bx
    
    push ebx
    mov bx, bp              ; BX = EBP (Base Y position)
    call draw_v_line
    pop ebx
    ; --- 3. Draw MIDDLE horizontal bar ---
    
    ; X position: Start 1 pixel right of Base X
    mov cx,bx            ;font times 2 width the h line 
    
    mov ax, si              ; AX = Base X
    inc ax                  ; AX = Base X + 1 (e.g., 51)
    
    
	
    push ebx
    
    ; Y position: Draw in the middle (e.g., Base Y + 7)
    mov bx, bp
                  ; BX = Base Y
    push ecx
    shr ecx,1              ;then we div by 2 to get the mid point
    add bx, cx             ; BX = Base Y + 7 (e.g., 80 + 7 = 87)
    pop ecx
    ; NOTE: This assumes draw_h_line knows to draw 5 pixels wide
    call draw_h_line 
	pop ebx
    popa
    ret
; draw_v_line: Draws a vertical line (16 pixels high)
; Input: AX = X position, BX = Y position, DL = Color
; draw_v_line: Draws a vertical line (16 pixels high)
    ; Input: AX = X position, BX = Y position, DL = Color
    ; draw_v_line: Draws a vertical line (16 pixels high)
        ; Input: AX = X position, BX = Y position, DL = Color
        draw_v_line:
            pusha
			push edx            
            ; Save AX (X) and BX (Y) to safe 32-bit registers for calculation
            movzx esi, ax           ; ESI = X coordinate
            movzx ebp, bx           ; EBP = Y coordinate
            movzx ebx,cx
            ; 1. Calculate starting address (Address = 0xA0000 + (Y * 320) + X)
            mov edi, 0xA0000
            
            ; Calculate Row Offset: Y * 320
            mov eax, 320            ; EAX = Screen width
            imul ebp                ; EDX:EAX = EAX * EBP. EAX now holds (Y * 320)
            
            ; Add Column Offset: EAX = EAX + ESI (Row Offset + X)
            add eax, esi            ; EAX = Total Offset
            add edi, eax            ; EDI = Starting Pixel Address (0xA0000 + Offset)
            
            mov ecx, ebx             ; Reset ECX for the vertical loop counter
            pop edx
        .v_loop:
        	
            mov byte [edi], dl      ; Plot pixel (White)
            add edi, 320            ; Jumps exactly 320 bytes down for the next row
            loop .v_loop            ; Decrement ECX, jump if not zero
            
            popa
            ret
draw_h_line:
	pusha
	movzx esi, ax           ; ESI = X coordinate
	movzx ebp, bx           ; EBP = Y coordinate
	movzx ebx,cx	
	; 1. Calculate starting address (Address = 0xA0000 + (Y * 320) + X)
	mov edi, 0xA0000


	        
	mov eax, ebp             ; EAX = Y coordinate (87)
	imul eax, 320           ; EAX = Y * 320 (Row Offset)
	add eax, esi             ; EAX = EAX + X (Total Offset)
	add edi, eax            ; EDI = Start Address (X=51, Y=87)
	        
	mov ecx, ebx           ; 7 pixels wide
	mov al, dl              ; <--- FIX: Copy the white color (15) from DL into AL
	rep stosb               ; Draw 7 white pixels horizontally
	       


	popa
	ret


Set_Pixel:
	pusha
	movzx esi,ax
	movzx ebp,bx

	mov edi, 0xA0000
	
	
		        
	mov eax, ebp             ; EAX = Y coordinate (87)
	imul eax, 320           ; EAX = Y * 320 (Row Offset)
	add eax, esi             ; EAX = EAX + X (Total Offset)
	add edi, eax            ; EDI = Start Address (X=51, Y=87)
		        
	mov ecx, 1           ; 7 pixels wide
	mov al, dl              ; <--- FIX: Copy the white color (15) from DL into AL
		rep stosb     
	popa

	ret
delay_1_sec:
    push ecx
    
    ; *** Experiment with this value (e.g., 50,000,000 or 0x02FAF080) ***
    ;1s 5000000000000000
    mov ecx, 120000000
.delay_loop:
    loop .delay_loop ; Decrements ECX and jumps if non-zero
    
    pop ecx
    ret



putchar_gfx:
    pusha
    push cx
    ; Save ASCII code to DH for later use
    mov dh, cl              
	    
    ; ESI = Base X, EBP = Base Y (Using 32-bit registers for calculation)
    movzx esi, ax           ; ESI = Base X
    movzx ebp, bx           ; EBP = Base Y
	
    ; --- 1. Calculate Font Data Offset (EDI = font_data + (char_code * 16)) ---
    
    movzx eax, dh           ; EAX = ASCII Code
    mov cl, 4               
    shl eax, cl             ; EAX = EAX * 16 (The offset into the font data)
    
    mov edi, font_data      ; EDI = Start address of the font data
    add edi, eax            ; EDI now points to the character's first bitmap byte

    ; --- 2. Setup Screen Address (ESI = VRAM Base + Y*320 + X) ---
    
   push edi
	mov edi, 0xA0000


	        
	mov eax, ebp             ; EAX = Y coordinate (87)
	imul eax, 320           ; EAX = Y * 320 (Row Offset)
	add eax, esi             ; EAX = EAX + X (Total Offset)
	add edi, eax            ; EDI = Start Address (X=51, Y=87)
	        
    
    ;mov eax, ebp            ; EAX = Y coordinate
    ;imul eax, 320           ; EAX = Y * 320 (Row Offset)
    
    ;add eax, esi            ; EAX = Row Offset + X (Total Offset)
	
	    	   
    
    ;mov esi, 0xA0000        ; ESI = VRAM Base
    mov esi, edi            ; ESI now points to the screen pixel address (Top Left)
    pop edi
    ; --- 3. Outer Loop: 16 Rows High ---
    mov ecx, 16             ; ECX = Outer loop counter (16 font rows)
    
.font_row_loop:
    push ecx               ; Save row counter
    
    mov al, [edi]           ; AL = Current font row's bitmap byte (8 bits)

    
    ; --- 4. Vertical Scaling Loop (Draws 2 rows for every 1 font row) ---
	
    mov ch, 1             ; CH = Vertical Scale Factor (S=2)
    
    
.v_scale_loop:
    push ecx                ; Save outer and vertical counters
    push esi                ; Save current screen address (ESI)
    
    ; --- 5. Horizontal Rasterization (Draw 16 pixels for the row) ---
    mov cl, 8               ; CL = Inner pixel counter (8 bits in the byte)
    
.pixel_loop:
    test al, 0x80           ; Check leftmost bit
    jz .no_pixel
    
    ; Plot 2 foreground pixels (Horizontal Scale)
    mov byte [esi], dl
    inc esi
    mov byte [esi], dl
    inc esi
    jmp .cont_h_shift
.no_pixel:
    ; Skip 2 blank columns
    add esi, 2
    
.cont_h_shift:
    shl al, 1               ; Shift bitmap left to check next bit
    loop .pixel_loop        ; Decrement CL, jump if not zero
    
    ; --- 6. Prepare for Next Vertical Scale Run ---
    
    ; Restore original X position (ESI) to start the next vertical pass
    pop esi                 ; Restore ESI to the beginning of the current screen row
    
    ; Move screen address down one row: (320 bytes down)
    add esi, 320
    
    pop ecx                 ; Restore outer and vertical counters
    dec ch                  ; Decrement vertical scale counter
    jnz .v_scale_loop       ; Loop 2 times vertically
    
    ; --- 7. Advance Font Pointers ---
    
    ; Restore outer counter and font pointer
    pop ecx                 ; Restore font row counter
    inc edi                 ; Move font pointer to the next font row's bitmap byte
    loop .font_row_loop     ; Loop 16 times

	pop cx    
    popa


    
    ret
; --- Data Section ---
font_data:
    

	resb 768

	db 0x7E, 0x81, 0x81, 0x81, 0x81, 0x81, 0x81, 0x7E ; 0
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x0C, 0x1C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x1E ; 1
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x7E, 0x81, 0x01, 0x02, 0x04, 0x08, 0x10, 0x7E ; 2
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x7E, 0x01, 0x01, 0x3E, 0x01, 0x01, 0x81, 0x7E ; 3
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x0C, 0x14, 0x24, 0x44, 0x84, 0xFE, 0x04, 0x04 ; 4
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0xFE, 0x80, 0x80, 0xFE, 0x01, 0x01, 0x81, 0x7E ; 5
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x7E, 0x80, 0x80, 0xFE, 0x81, 0x81, 0x81, 0x7E ; 6
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0xFF, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40 ; 7
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x7E, 0x81, 0x81, 0x7E, 0x81, 0x81, 0x81, 0x7E ; 8
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	
	db 0x7E, 0x81, 0x81, 0x81, 0x7E, 0x01, 0x01, 0x7E ; 9
	db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ; Reserve remaining space for the full 256-character font

	resb 112
    ; Index 	 (The A character): 16 bytes 0x41
        db 0x18, 0x24, 0x42, 0x42, 0x7E, 0x81, 0x81, 0x81
        db 0x81, 0x81, 0x81, 0x81, 0x00, 0x00, 0x00, 0x00
    
    	; Index 0x01 (The B character - 16 bytes)
    	    db 0xF8, 0x84, 0x84, 0xF8, 0x84, 0x84, 0xF8, 0x80
    	    db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

    	 ;C
    	db 0x3C,0x42,0x81,0x80,0x80,0x80,0x80,0x81
    	db 0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00

    	;D
    	db 0xF8,0x84,0x82,0x81,0x81,0x81,0x82,0x84
    	db 0xF8,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	;E
    	db 0xFF,0x80,0x80,0xF8,0x80,0x80,0x80,0xFF
    	db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	;F
    	db 0xFF,0x80,0x80,0xF8,0x80,0x80,0x80,0x80
    	db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	;G
    	db 0x3C,0x42,0x81,0x80,0x80,0x8F,0x81,0x41
    	db 0x3E,0x00,0x00,0x00,0x00,0x00,0x00,0x00

    	;H
    	db 0x81,0x81,0x81,0xFF,0x81,0x81,0x81,0x81
    	db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; I
    	db 0x7E,0x18,0x18,0x18,0x18,0x18,0x18,0x7E,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; J
    	db 0x3E,0x06,0x06,0x06,0x06,0x06,0x86,0x44,0x38,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; K
    	db 0x81,0x82,0x84,0xF8,0x84,0x82,0x81,0x81,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; L
    	db 0x80,0x80,0x80,0x80,0x80,0x80,0x80,0xFF,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; M
    	db 0x81,0xC3,0xA5,0x99,0x81,0x81,0x81,0x81,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; N
    	db 0x81,0xC1,0xA1,0x91,0x89,0x85,0x83,0x81,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; O
    	db 0x3C,0x42,0x81,0x81,0x81,0x81,0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; P
    	db 0xF8,0x84,0x84,0xF8,0x80,0x80,0x80,0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; Q
    	db 0x3C,0x42,0x81,0x81,0x81,0x8D,0x42,0x3D,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; R
    	db 0xF8,0x84,0x84,0xF8,0x84,0x82,0x81,0x81,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; S
    	db 0x3E,0x41,0x40,0x3C,0x02,0x02,0x42,0x7C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; T
    	db 0xFF,0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; U
    	db 0x81,0x81,0x81,0x81,0x81,0x81,0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; V
    	db 0x81,0x81,0x42,0x42,0x24,0x24,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; W
    	db 0x81,0x81,0x81,0x99,0xA5,0xC3,0x81,0x81,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; X
    	db 0x81,0x42,0x24,0x18,0x18,0x24,0x42,0x81,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; Y
    	db 0x81,0x42,0x24,0x18,0x18,0x18,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; Z
    	db 0xFF,0x02,0x04,0x18,0x20,0x40,0x80,0xFF,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	
    	resb 96

    	;0x61
    	; a
    	db 0x00,0x00,0x3C,0x02,0x3E,0x42,0x3E,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; b
    	db 0x80,0x80,0xBC,0xC2,0xC2,0xC2,0xBC,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; c
    	db 0x00,0x00,0x3C,0x42,0x80,0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; d
    	db 0x02,0x02,0x3E,0x42,0x42,0x42,0x3E,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; e
    	db 0x00,0x00,0x3C,0x42,0x7E,0x80,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; f
    	db 0x1C,0x22,0x20,0xF8,0x20,0x20,0x20,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; g
    	db 0x00,0x00,0x3E,0x42,0x3E,0x02,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; h
    	db 0x80,0x80,0xBC,0xC2,0xC2,0xC2,0xC2,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; i
    	db 0x18,0x00,0x38,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; j
    	db 0x06,0x00,0x06,0x06,0x06,0x46,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; k
    	db 0x80,0x80,0xC4,0x88,0xF0,0x88,0xC4,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; l
    	db 0x38,0x18,0x18,0x18,0x18,0x18,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; m
    	db 0x00,0x00,0xEC,0x92,0x92,0x92,0x92,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; n
    	db 0x00,0x00,0xBC,0xC2,0xC2,0xC2,0xC2,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; o
    	db 0x00,0x00,0x3C,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; p
    	db 0x00,0x00,0xBC,0xC2,0xBC,0x80,0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; q
    	db 0x00,0x00,0x3E,0x42,0x3E,0x02,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; r
    	db 0x00,0x00,0xBC,0xC2,0x80,0x80,0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; s
    	db 0x00,0x00,0x3E,0x40,0x3C,0x02,0x7C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; t
    	db 0x20,0x20,0xF8,0x20,0x20,0x22,0x1C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; u
    	db 0x00,0x00,0xC2,0xC2,0xC2,0xC6,0x3A,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; v
    	db 0x00,0x00,0xC2,0xC2,0x42,0x24,0x18,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; w
    	db 0x00,0x00,0xC2,0x92,0x92,0x92,0x6C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; x
    	db 0x00,0x00,0xC2,0x44,0x18,0x44,0xC2,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; y
    	db 0x00,0x00,0xC2,0xC2,0x3E,0x02,0x3C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    	; z
    	db 0x00,0x00,0x7E,0x04,0x18,0x20,0x7E,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    	
    resb (4096 - 16) 



	ret



put_str:

		push ebp        ; Save caller's EBP
		mov ebp, esp    ; EBP points to the base of the current stack frame
		push    ebx
				    push    esi
				    push    edi		
		; --- Load Arguments (32-bit to 32-bit registers) ---
		mov ebx, [ebp + 8]      ; EBX = X_Pos (Master X register)
		mov edi, [ebp + 12]     ; Edi = Y_Pos (Master Y register)
		mov edx, [ebp + 16]     ; EDX = Color (32-bit, only DL is needed)
		
		mov esi, [ebp + 20]     ; ESI = Address of the string
	
	.loop:
		
		; 1. Load Character Code (from ESI)
		mov cl, [esi]           ; AL = Current character (8-bit)
		;mov cl,si
		; 2. Check for Null-Terminator
		cmp cl, 0               
		je .done
		push ebx
		push edi
		; --- Prepare Call Arguments ---
		
		; X-Position (AX)
		mov ax, bx              ; AX = X_Pos (low 16 bits of EBX)
		
		; Y-Position (BX)
		mov bx, di            ; BX = Y_Pos (low 16 bits of ECX)
		
		; Character Code (CL) - THIS IS THE CRITICAL SWAP
		;mov cl, al              ; CL = Character Code (from AL)
		
		; Color (DL) - Already in the correct place
		
		; Char Width (CH) - Assume this is passed in a general register or fixed
		;mov ch, 8               ; Assuming fixed character width of 8 pixels
		
		; 3. DRAW CHARACTER
		call putchar_gfx
		pop edi
		pop ebx
		; 4. Advance Pointers
		inc esi                 ; ESI++ (Move to next character in string)
		add ebx, 15               ; BX = BX + 8 (Advance X-position by 8 pixels)
		
		jmp .loop
	
	.done:
		; Restore registers and return
		    pop     edi
		    pop     esi
		    pop     ebx
		; --- Standard Stack Frame Cleanup ---
		mov esp, ebp    ; Restore ESP to EBP's starting position
		pop ebp         ; Restore caller's EBP
		
		ret

put_font_char_8_16:
	pusha
	movzx esi,ax
	movzx ebp,bx


	movzx eax, dh           ; EAX = ASCII Code
	mov cl, 4               
	shl eax, cl             ; EAX = EAX * 16 (The offset into the font data)
	    
	mov ebx, .font_data      ; Edx = Start address of the font data
	add ebx, eax            ; Edx now points to the character's first bitmap byte


	mov edi, 0xA0000
	
	
		        
	mov eax, ebp             ; EAX = Y coordinate (87)
	imul eax, 320           ; EAX = Y * 320 (Row Offset)
	add eax, esi             ; EAX = EAX + X (Total Offset)
	add edi, eax            ; EDI = Start Address (X=51, Y=87)


	  
mov ecx, 16             ; ECX = Outer loop counter (16 font rows)
    
.font_row_loop:
    push ecx               ; Save row counter
    
    mov al, [ebx]           ; AL = Current font row's bitmap byte (8 bits)

    
    ; --- 4. Vertical Scaling Loop (Draws 2 rows for every 1 font row) ---
	
    mov ch, 2             ; CH = Vertical Scale Factor (S=2)
 
	
.v_scale_loop:
    push ecx                ; Save outer and vertical counters
    push edi                ; Save current screen address (ESI)
    
    ; --- 5. Horizontal Rasterization (Draw 16 pixels for the row) ---
    mov cl, 8               ; CL = Inner pixel counter (8 bits in the byte)
    
.pixel_loop:
    test al, 0x80           ; Check leftmost bit
    jz .no_pixel
    
    ; Plot 2 foreground pixels (Horizontal Scale)
    mov byte [edi], dl
    inc edi
    mov byte [edi], dl
    inc edi
    mov byte [edi], dl
                        inc edi
                        mov byte [edi], dl
                       	inc edi
    
	add edi,320   
    sub edi,3
    mov byte [edi], dl
                    inc edi
                    mov byte [edi], dl
                    inc edi      
                    sub edi,320    
    mov byte [edi], dl
                          inc edi
                          mov byte [edi], dl
                                
                          sub edi,320  
    
    jmp .cont_h_shift
.no_pixel:
    ; Skip 2 blank columns
    add edi, 1
    
.cont_h_shift:
    shl al, 1               ; Shift bitmap left to check next bit
    loop .pixel_loop        ; Decrement CL, jump if not zero
    
    ; --- 6. Prepare for Next Vertical Scale Run ---
    
    ; Restore original X position (ESI) to start the next vertical pass
    pop edi                 ; Restore ESI to the beginning of the current screen row
    
    ; Move screen address down one row: (320 bytes down)
    add edi, 320
         
    pop ecx                 ; Restore outer and vertical counters
    dec ch                  ; Decrement vertical scale counter
    jnz .v_scale_loop       ; Loop 2 times vertically
    
    ; --- 7. Advance Font Pointers ---
    
    ; Restore outer counter and font pointer
    pop ecx                 ; Restore font row counter
    inc ebx                 ; Move font pointer to the next font row's bitmap byte
    loop .font_row_loop     ; Loop 16 times	
	
popa

	ret
.font_data:
	db 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
	db 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
	db 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
	db 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF

	;0
	db 0x30, 0x7E, 0x81, 0x81, 0x81, 0x81, 0x81, 0x81
	db 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;1
	db 0x31, 0x0C, 0x1C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C
	db 0x1E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;2
	db 0x32,0x7E, 0x81, 0x01, 0x02, 0x04, 0x08, 0x10, 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;3
	db 0x33,0x7E, 0x01, 0x01, 0x3E, 0x01, 0x01, 0x81, 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;4
	db 0x34,0x0C, 0x14, 0x24, 0x44, 0x84, 0xFE, 0x04, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;5
	db 0x35,0xFE, 0x80, 0x80, 0xFE, 0x01, 0x01, 0x81, 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;6
	db 0x36,0x7E, 0x80, 0x80, 0xFE, 0x81, 0x81, 0x81, 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;7,
	db 0x37,0xFF, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;8,
	db 0x38,0x7E, 0x81, 0x81, 0x7E, 0x81, 0x81, 0x81, 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	;9,
	db 0x39, 0x7E, 0x81, 0x81, 0x81, 0x7E, 0x01, 0x01, 0x7E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	resb (4096-16)
	ret
	

