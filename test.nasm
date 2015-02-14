;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nasm -f macho64 test.nasm
;; ld -macosx_version_min 10.6 -o test -e _main test.o
;; ./test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	SECTION .text

	global _main
_main:
	lea rdi, [rel buffer]
	mov rsi, 0
	mov rax, 0x2000074
	syscall

	mov rdi, [rel buffer]
	mov rax, 0x2000001
	syscall

	SECTION .data
buffer:
	times 16 db 0
