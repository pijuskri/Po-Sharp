TARGET_FILE = 'hello'

all: run

#assemble hello.asm
build:
	mkdir -p compiled && \
	cd compiled/ && \
	nasm -felf64 $(TARGET_FILE).asm && \
	gcc -O0 -ggdb -no-pie $(TARGET_FILE).o -o $(TARGET_FILE)

build_all:
	$(bash ./build.sh)

#compile and run asm 
run: build_all
	compiled/$(TARGET_FILE)

#compile Po# using sbt and then run it, also running the generated .asm
full: sbt run

#compile Po# compiler
sbt: 
	sbt --batch -Dsbt.server.forcestart=true run

# for some example on the internet, the gcc compiler has issues
standalone:
	nasm -f elf hello.asm && ld -m elf_i386 hello.o -o hello && ./hello

#valgrind --leak-check=full --track-origins=yes --dsymutil=yes ./hello
