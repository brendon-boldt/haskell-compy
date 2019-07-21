
stack build --fast --verbosity warn --profile || exit

result () {
    if [[ $1 == Passed ]]; then
        printf "\e[32m%20s\e[0m\n" "$1"
    else
        printf "\e[31m%20s\e[0m\n" "$1"
    fi
}

for src in $(ls test/*.src); do
    printf "%-30s" "$(basename $src)"
    expected=${src%.src}.out
    rm -f asm/main.s
    stack exec -- haskell-compy-exe $src +RTS -xc 2> error.out > /dev/null
    #stack exec -- haskell-compy-exe $src 2> error.out > /dev/null
    if [[ $? != 0 ]]; then
        result "Failed (compilation)"
        cat error.out
        continue
    fi
    gcc -o asm/main.out asm/main.s asm/lib.s 2> error.out
    if [[ $? != 0 ]]; then
        echo -e "\e[31mFailed (assembling)\e[0m"
        cat error.out
        continue
    fi
    asm/./main.out > actual.out
    exitcode=$?
    if [[ $exitcode != 0 ]]; then
        result "Failed (non-zero exit)"
        cat actual.out
        continue
    fi
    diffout=$(diff -y -W10 actual.out "$expected")
    if [[ $? != 0 ]]; then
        result "Failed (output)"
        printf "$diffout"
        echo
        continue
    fi
    result Passed
done

rm -f actual.out error.out
echo Done.
