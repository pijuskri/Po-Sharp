TARGET_FILE = 'hello'

all: run_llvm

#assemble hello.asm
build:
	mkdir -p compiled && \
	cd compiled/ && \
	llc $(TARGET_FILE).ll -opaque-pointers && \
	gcc -O0 -ggdb -no-pie $(TARGET_FILE).s -o $(TARGET_FILE)
	compiled/$(TARGET_FILE)

#compile all files in directory
.PHONY: build_all
build_all:
	bash ./build.sh

build_all_llvm:
	bash ./build-llvm.sh

#compile and run asm 
run_llvm: build_all_llvm
	compiled/$(TARGET_FILE)

run_asm: build_all
	compiled/$(TARGET_FILE)

#valgrind --leak-check=full --track-origins=yes --dsymutil=yes ./hello
