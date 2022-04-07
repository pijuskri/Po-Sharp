#!/bin/bash

mkdir -p compiled && \
cd compiled/

dir_succeeded=$?

if [ $dir_succeeded -ne 0 ];
then
    exit 1
fi

for i in $( cut -d '.' -f 1 <<< "$(ls | grep .asm)" ); 
do
    nasm -felf64 $i.asm && \
    gcc -O0 -ggdb -no-pie $i.o -o $i
done