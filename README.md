# Diamant_OS

A tiny operating system written from scratch in **x86 assembly (NASM)**: a boot sector, a switch into 32-bit
protected mode, its own interrupt handling and keyboard driver, and text drawn pixel by pixel with a hand-made
bitmap font. About **2,300 lines**, no C, no BIOS calls after boot.

<!-- Screenshot: QEMU window after typing a few letters -->

## What happens when it boots

1. **Boot sector** (`main.asm`, 512 bytes, ends in the `0xAA55` signature). The BIOS loads it at `0x7C00`. It:
   - switches the screen to **VGA mode 13h** (320×200, 256 colours) while BIOS calls are still available;
   - reads the kernel from the floppy into memory at `0x1000` (`int 13h`);
   - loads a **GDT** (code and data segments covering all 4 GB), sets the protected-mode bit in `cr0`
     and far-jumps into the kernel.
2. **Kernel** (`Kernel/Kernel.asm`, 32-bit):
   - reloads the segment registers and sets up a stack;
   - builds an **IDT** (256 entries) and remaps the **8259 PIC** so hardware interrupts don't collide with CPU
     exceptions;
   - enables interrupts and fills the screen by writing straight to video memory at `0xA0000`.
3. **Keyboard driver**: the IRQ1 handler reads the scancode from port `0x60`, handles it, and acknowledges the
   PIC.

## What you can do

| Key | Result |
|---|---|
| `a` | Types `a` |
| `Shift` + `a` | Types `A` (Shift is tracked as held / released) |
| `Backspace` | Steps the cursor back and redraws the cell |
| `←` | Moves the cursor left |

Typed characters are kept in a text buffer (character and colour per cell), and the cursor wraps to the next line
at the right edge.

## Graphics, all by hand

There's no text mode and no font from the BIOS. Everything on screen is plotted pixel by pixel into video memory:

- `Set_Pixel`, horizontal, vertical and diagonal lines;
- hand-drawn letters (`draw_char_A`, `N`, `C`, `L`, `E`, `F`, `H`);
- an **8×16 bitmap font** (`font_data`) drawn by `putchar_gfx` / `put_str`.

## Build and run

Needs NASM and QEMU.

```sh
nasm -f bin V0.001/main.asm -o boot.bin
nasm -f bin V0.001/Kernel/Kernel.asm -o kernel.bin

# floppy image: boot sector, then the kernel straight after it
cat boot.bin kernel.bin > floppy.img                   # Linux / macOS
copy /b boot.bin + kernel.bin floppy.img               # Windows cmd

qemu-system-i386 -fda floppy.img
```

## Limitations

- Only the keys above are mapped so far.
- No mouse yet (the boot message mentions it and the cursor variables exist, but there's no mouse driver).
- No file system yet.

---

Part of my Diamant projects. Each repository is its own separate project.
