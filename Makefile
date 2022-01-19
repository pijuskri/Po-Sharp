all: run

#assemble hello.asm
build:
	mkdir -p compiled && \
	cd compiled/ && \
	nasm -felf64 hello.asm && \
	gcc -no-pie hello.o -o hello

#compile and run asm 
run: build
	compiled/hello

#compile Po# using sbt and then run it, also running the generated .asm
full: sbt run

#compile Po# compiler
sbt: 
	sbt --batch -Dsbt.server.forcestart=true run


# for some example on the internet, the gcc compiler has issues
standalone:
	nasm -f elf hello.asm && ld -m elf_i386 hello.o -o hello && ./hello
