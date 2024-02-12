#!/bin/bash

#for compatibility between installs
#clang() {
#    if hash clang-15 2>/dev/null; then
#        clang-15 "$@"
#    else
#        clang "$@"
#    fi
#}
#llc() {
#    if hash llc-15 2>/dev/null; then
#        llc-15 "$@"
#    else
#        llc "$@"
#    fi
#}

#make compiled directory and go to it
mkdir -p compiled && \
cd compiled/ || exit

dir_succeeded=$?

#check if moving to directory succeeded
if [ $dir_succeeded -ne 0 ];
then
    echo 'Could not go to compiled/ directory'
    exit 1
fi

#sus code that gets all files that have a .ll ending
files=$( cut -d '.' -f 1 <<< "$(ls -- *.ll)" )

files_asm=()

#compile all .ll files to assembly
for i in $files;
do
    files_asm+=("$i".s)
    echo "$i".ll
    llc "$i".ll -O0 -opaque-pointers
    # --stackrealign --stack-size-section  --debugify-level=location+variables --frame-pointer=all -align-all-nofallthru-blocks=4 -align-all-functions=4
    #  --stackrealign --asm-show-inst --align-loops=64
done

#llc test.ll -O0 -opaque-pointers
#put all asm files to files
files="${files_asm[*]}"

#gcc -O0 -ggdb -mpreferred-stack-boundary=4 -no-pie $files -o "hello"
#link and compile all assembly files to a single object file
clang -O0 -no-pie $files -o "hello" # -mstack-alignment=4

#cd compiled/
#llc test.ll -O0 -opaque-pointers
#clang -O0 -no-pie test.s -o "hello" # -mstack-alignment=4