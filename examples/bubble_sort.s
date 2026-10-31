# bubble_sort.s: sort an array of signed 64-bit integers in place, then
# print it. Uses scaled indexing (slli + add) and nested loops.

    .data
array:  .dword 42, -7, 1000000000000, 3, 0, -99999, 17, 3, 8, -1
    .equ N, 10

    .text
_start:
    la    s0, array
    li    s1, N

    # for (i = n-1; i > 0; i--) for (j = 0; j < i; j++) swap if a[j] > a[j+1]
    addi  t0, s1, -1            # i
outer:
    blez  t0, done
    li    t1, 0                 # j
inner:
    bge   t1, t0, next_i
    slli  t2, t1, 3
    add   t2, s0, t2            # &a[j]
    ld    t3, 0(t2)
    ld    t4, 8(t2)
    ble   t3, t4, no_swap
    sd    t4, 0(t2)
    sd    t3, 8(t2)
no_swap:
    addi  t1, t1, 1
    j     inner
next_i:
    addi  t0, t0, -1
    j     outer

done:
    li    t0, 0                 # print a[0..n)
print:
    slli  t2, t0, 3
    add   t2, s0, t2
    ld    a0, 0(t2)
    li    a7, 1
    ecall
    addi  t0, t0, 1
    li    a0, ' '
    li    a7, 11
    blt   t0, s1, 1f
    li    a0, '\n'
1:  ecall
    blt   t0, s1, print
    li    a7, 10
    ecall
