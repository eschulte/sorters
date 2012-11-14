              .section .bss
              .lcomm buf, 500
              .lcomm lst, 500
              .section .text
              .globl _start
_start:
              popl  %eax
              popl  %eax
              movl  $0, %esi
              movl  $10, %edx
              jmp   read_arg
store_arg:
              movl  %ecx, lst(,%esi,4)
              incl  %esi
read_arg:
              popl  %eax
              cmpl  $0, %eax
              je    sort
              movl  $0, %edi
              movl  $0, %ecx
read_ch:
              movb  0(%eax,%edi), %bl
              cmpl  $0, %ebx
              je    store_arg
              imul  %edx, %ecx
              subl  $48, %ebx
              addl  %ebx, %ecx
              incl  %edi
              jmp   read_ch
sort:
              movl  $0, %ecx
              movl  $0, %edi
              movl  lst(,%edi,4), %eax
main_loop:
              movl  %eax, %ebx
              movl  %edi, %edx
              incl  %edi
              cmpl  %edi, %esi
              je    cond_restart
              movl  lst(,%edi,4), %eax
              cmpl  %ebx, %eax
              jge   main_loop
              incl  %ecx
              movl  %ebx, lst(,%edi,4)
              movl  %eax, lst(,%edx,4)
              jmp   sort
cond_restart:
              cmpl  $0, %ecx
              jne   sort
              movl  $0, %edi
              movl  $0, %edi
print_loop:
              pushl %edi
              movl  lst(,%edi,4), %eax
              pushl $0
              movl  $10, %ecx
cnv_int_down:
              movl  $0, %edx
              idiv  %ecx
              addl  $48, %edx
              pushl %edx
              cmpl  $0, %eax
              jne   cnv_int_down
              movl  $0, %edi
cnv_int_up:
              popl  %eax
              cmpl  $0, %eax
              je    do_print
              movb  %al, buf(,%edi,1)
              incl  %edi
              jmp   cnv_int_up
do_print:
              movl  $4, %eax
              movl  $buf, %ecx
              movl  $1, %ebx
              movl  %edi, %edx
              int   $0x80
              movl  $32, buf
              movl  $buf, %ecx
              movl  $4, %eax
              movl  $1, %ebx
              movl  $1, %edx
              int   $0x80
              popl  %edi
              incl  %edi
              cmpl  %edi, %esi
              jg    print_loop
              movl  $10, buf
              movl  $buf, %ecx
              movl  $4, %eax
              movl  $1, %ebx
              int   $0x80
              movl  $1, %eax
              movl  $0, %ebx
              int   $0x80
