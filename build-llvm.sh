#!/bin/bash

mkdir -p compiled && \
cd compiled/ || exit

dir_succeeded=$?

if [ $dir_succeeded -ne 0 ];
then
    exit 1
fi

files=$( cut -d '.' -f 1 <<< "$(ls -- *.ll)" )

files_asm=()

for i in $files;
do
    files_asm+=("$i".s)
    llc-15 "$i".ll -O0 -opaque-pointers --stackrealign --stack-size-section  --debugify-level=location+variables --frame-pointer=all -align-all-nofallthru-blocks=4 -align-all-functions=4
    #  --stackrealign --asm-show-inst --align-loops=64
done

files="${files_asm[*]}"

#gcc -O0 -ggdb -mpreferred-stack-boundary=4 -no-pie $files -o "hello"
clang-15 -O0 -no-pie $files -o "hello" # -mstack-alignment=4
