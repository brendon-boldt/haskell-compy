            .data

int_f:      .asciz  "%d\n"

            .text

.global show_int
show_int: # rsi -> rax
            mov     %rsi, %r12
            xor     %rax, %rax
            lea     int_f(%rip), %rdi
            call    printf
            mov     %r12, %rax
            ret
