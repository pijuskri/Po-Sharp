#!/bin/bash

mkdir -p compiled && \
cd compiled/ || exit

dir_succeeded=$?

if [ $dir_succeeded -ne 0 ];
then
    exit 1
fi

files=$( cut -d '.' -f 1 <<< "$(ls | grep .asm)" )
files_asm=()

for i in $files;
do
    files_asm+=($i.o)
    nasm -felf64 $i.asm
done

files="${files_asm[*]}"

gcc -O0 -ggdb -no-pie $files -o "hello"