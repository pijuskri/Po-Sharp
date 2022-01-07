all: run

build:
	mkdir -p compiled && \
	cd compiled/ && \
	nasm -felf64 hello.asm && \
	gcc -no-pie hello.o -o hello

run: build 
	compiled/hello

# for some example on the internet, the gcc compiler has issues
standalone:
	nasm -f elf hello.asm && ld -m elf_i386 hello.o -o hello && ./hello

