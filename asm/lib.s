            .data

int_f:      .asciz  "%d\n"

            .text

.global show_int
show_int:
            push    %rdi
            mov     %rdi, %rsi
            xor     %rax, %rax
            lea     int_f(%rip), %rdi
            call    printf
            pop     %rax
            ret
