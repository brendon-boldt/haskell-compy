
stack build --fast --verbosity warn --profile || exit

for src in $(ls test/*.src); do
    echo -n "$(basename $src) . . . "
    expected=${src%.src}.out
    rm -f asm/main.s
    stack exec -- haskell-compy-exe $src +RTS -xc 2> error.out > /dev/null
    #stack exec -- haskell-compy-exe $src 2> error.out > /dev/null
    if [[ $? != 0 ]]; then
        echo -e "\e[31mFailed (compilation)\e[0m"
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
        echo -e "\e[31mFailed (non-zero exit)\e[0m"
        cat actual.out
        continue
    fi
    diffout=$(diff -y -W10 actual.out "$expected")
    if [[ $? != 0 ]]; then
        echo -e "\e[31mFailed (output)\e[0m"
        printf "$diffout"
        echo
        continue
    fi
    echo -e "\e[32mPassed\e[0m"
done

rm -f actual.out error.out
echo Done.
