ASM = nasm
SRC_DIR = os_jack
KERNEL_DIR = kernel
FS_DIR = $(KERNEL_DIR)/file_sys
BUILD_DIR = build

BOOTLOADER = $(SRC_DIR)/main.asm
KERNEL = $(KERNEL_DIR)/kernel.asm
FS = $(FS_DIR)/fs.asm

BOOT_BIN = $(BUILD_DIR)/bootloader.bin
KERNEL_BIN = $(BUILD_DIR)/kernel.bin
FS_BIN = $(BUILD_DIR)/fs.bin
IMG = $(BUILD_DIR)/m_floppy.img

# Default target
all: $(IMG)

#########################################
# Build floppy disk image
#########################################

$(IMG): $(BOOT_BIN) $(KERNEL_BIN) $(FS_BIN)
	mkdir -p $(BUILD_DIR)
	cp $(BOOT_BIN) $(IMG)
	truncate -s $$(expr 512 \* 1) $(IMG)
	dd if=$(KERNEL_BIN) of=$(IMG) bs=512 seek=1 conv=notrunc
	dd if=$(FS_BIN)     of=$(IMG) bs=512 seek=11 conv=notrunc

#########################################
# NASM Assembly rules
#########################################

$(BOOT_BIN): $(BOOTLOADER)
	mkdir -p $(BUILD_DIR)
	$(ASM) $< -f bin -o $@

$(KERNEL_BIN): $(KERNEL)
	mkdir -p $(BUILD_DIR)
	$(ASM) $< -f bin -o $@

$(FS_BIN): $(FS)
	mkdir -p $(BUILD_DIR)
	$(ASM) $< -f bin -o $@

#########################################
# GRUB ISO
#########################################



#########################################
# Clean
#########################################

clean:
	rm -rf $(BUILD_DIR)
	rm -rf Iso
	rm -f graphicskernel.iso
