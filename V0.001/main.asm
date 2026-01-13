org 0x7C00
bits 16
jmp main
;bootloader
msg db "Loading 320x200x256 + Mouse...",13,10,0

main:
    xor ax,ax
    mov ds,ax
    mov es,ax
    mov ss,ax
    mov sp,0x7C00

    mov si,msg
    call puts

    ; SET MODE 13h IN REAL MODE ← THIS IS THE KEY
    mov ax,0x0013
    int 0x10

    ; Load kernel
    mov ah,0x02
    mov al,30
    mov ch,0
    mov cl,2
    mov dh,0
    mov dl,0          ; floppy
    mov bx,0x1000
    int 0x13
    jc error

    cli
    lgdt [gdtr]
    mov eax,cr0
    or al,1
    mov cr0,eax

    jmp 0x08:0x1000   ; jump to kernel

puts:
    lodsb
    or al,al
    jz .d
    mov ah,0x0E
    mov bx,7
    int 0x10
    jmp puts
.d: ret

error:
    mov si,errmsg
    call puts
    jmp $
errmsg db "Read error",13,10,0

gdtr:
    dw gdt_end-gdt-1
    dd gdt
gdt:
    dq 0
    dq 0x00CF9A000000FFFF
    dq 0x00CF92000000FFFF
gdt_end:

times 510-($-$$) db 0
dw 0xAA55
