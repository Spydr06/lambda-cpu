; print the alphabet
mov %x0, $'A'       ; lower bound (A)
mov 0xfffe, $1, %x0 ; write byte in x0 to stdio device
add %x0, $1         ; increment to the next letter
cmp %x0, $'['       ; upper bound (Z)
jnz $1              ; if 'Z' isn't reached yet, repeat
mov %x0, $'\n'      ; ascii '\n'
mov 0xfffe, $1, %x0 ; write newline to stdio device
hlt                 ; halt the cpu

