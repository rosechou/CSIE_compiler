.data
_n: .word 0
.align 3
.text
_start_fact:
sd ra,0(sp)
sd fp,-8(sp)
add fp, sp, -8
add sp, sp, -16
la ra, _frameSize_fact
lw ra,0(ra)
sub sp,sp,ra
sd t0,8(sp)
sd t1,16(sp)
sd t2,24(sp)
sd t3,32(sp)
sd t4,40(sp)
sd t5,48(sp)
sd t6,56(sp)
sd t2,64(sp)
sd t3,72(sp)
sd t4,80(sp)
sd t5,88(sp)
sd t6,96(sp)
sd t7,104(sp)
sd t8,112(sp)
sd t9,120(sp)
sd t10,128(sp)
sd t11,136(sp)
sd fp,144(sp)
fsw ft0,152(sp)
fsw ft1,156(sp)
fsw ft2,160(sp)
fsw ft3,164(sp)
fsw ft4,168(sp)
fsw ft5,172(sp)
fsw ft6,176(sp)
fsw ft7,180(sp)
lw t0,_n
sw t0,0(sp)
addi sp, sp, -8
.data
_int_const_1: .word 1
.align 3
.text
lw t0, _int_const_1
addi sp, sp, 8
lw t1,0(sp)
beq t1, t0 _binaryOpLabel_2
mv t1,x0
j _END_binaryOp_2
_binaryOpLabel_2:
addi t1,x0,1
_END_binaryOp_2:
beqz t1, _elseLabel_0
lw t0,_n
mv a0,t0
j _end_fact
j _ifExitLabel_0
_elseLabel_0:
lw t0,_n
sw t0,0(sp)
addi sp, sp, -8
.data
_int_const_3: .word 1
.align 3
.text
lw t0, _int_const_3
addi sp, sp, 8
lw t1,0(sp)
subw t1, t1, t0
la t0,_n
sw t1,0(t0)
lw t0,_n
sw t0,0(sp)
addi sp, sp, -8
jal _start_fact
mv t0,a0
addi sp, sp, 8
lw t1,0(sp)
mulw t1, t1, t0
mv a0,t1
j _end_fact
_ifExitLabel_0:
_end_fact:
ld t0,8(sp)
ld t1,16(sp)
ld t2,24(sp)
ld t3,32(sp)
ld t4,40(sp)
ld t5,48(sp)
ld t6,56(sp)
ld t2,64(sp)
ld t3,72(sp)
ld t4,80(sp)
ld t5,88(sp)
ld t6,96(sp)
ld t7,104(sp)
ld t8,112(sp)
ld t9,120(sp)
ld t10,128(sp)
ld t11,136(sp)
ld fp,144(sp)
flw ft0,152(sp)
flw ft1,156(sp)
flw ft2,160(sp)
flw ft3,164(sp)
flw ft4,168(sp)
flw ft5,172(sp)
flw ft6,176(sp)
flw ft7,180(sp)
ld ra,8(fp)
mv sp,fp
add sp,sp,8
ld fp,0(fp)
jr ra
.data
_frameSize_fact .word 184
.text
_start_main:
sd ra,0(sp)
sd fp,-8(sp)
add fp, sp, -8
add sp, sp, -16
la ra, _frameSize_main
lw ra,0(ra)
sub sp,sp,ra
sd t0,8(sp)
sd t1,16(sp)
sd t2,24(sp)
sd t3,32(sp)
sd t4,40(sp)
sd t5,48(sp)
sd t6,56(sp)
sd t2,64(sp)
sd t3,72(sp)
sd t4,80(sp)
sd t5,88(sp)
sd t6,96(sp)
sd t7,104(sp)
sd t8,112(sp)
sd t9,120(sp)
sd t10,128(sp)
sd t11,136(sp)
sd fp,144(sp)
fsw ft0,152(sp)
fsw ft1,156(sp)
fsw ft2,160(sp)
fsw ft3,164(sp)
fsw ft4,168(sp)
fsw ft5,172(sp)
fsw ft6,176(sp)
fsw ft7,180(sp)
.data
_string_const_4: .ascii "Enter a number:"
.align 3
.text
la t0, _string_const_4
mv a0,t0
jal _write_str
jal _read_int
mv t0,a0
la t1,_n
sw t0,0(t1)
lw t0,_n
sw t0,0(sp)
addi sp, sp, -8
.data
_int_const_5: .word 1
.align 3
.text
lw t0, _int_const_5
addi sp, sp, 8
lw t1,0(sp)
addw t1, t1, t0
la t0,_n
sw t1,0(t0)
lw t0,_n
sw t0,0(sp)
addi sp, sp, -8
.data
_int_const_7: .word 1
.align 3
.text
lw t0, _int_const_7
addi sp, sp, 8
lw t1,0(sp)
blt t0, t1 _binaryOpLabel_8
mv t0,x0
j _END_binaryOp_8
_binaryOpLabel_8:
addi t0,x0,1
_END_binaryOp_8:
beqz t0, _elseLabel_6
jal _start_fact
mv t0,a0
.data
_const_offset_9: .word 188
.align 3
.text
lw t1, _const_offset_9
sub t1, fp, t1
sw t0,0(t1)
j _ifExitLabel_6
_elseLabel_6:
.data
_int_const_10: .word 1
.align 3
.text
lw t0, _int_const_10
.data
_const_offset_11: .word 188
.align 3
.text
lw t1, _const_offset_11
sub t1, fp, t1
sw t0,0(t1)
_ifExitLabel_6:
.data
_string_const_12: .ascii "The factorial is "
.align 3
.text
la t0, _string_const_12
mv a0,t0
jal _write_str
.data
_const_offset_13: .word 188
.align 3
.text
lw t0, _const_offset_13
sub t0, fp, t0
lw t0,0(t0)
mv a0, t0
jal _write_int
.data
_string_const_14: .ascii "\n"
.align 3
.text
la t0, _string_const_14
mv a0,t0
jal _write_str
_end_main:
ld t0,8(sp)
ld t1,16(sp)
ld t2,24(sp)
ld t3,32(sp)
ld t4,40(sp)
ld t5,48(sp)
ld t6,56(sp)
ld t2,64(sp)
ld t3,72(sp)
ld t4,80(sp)
ld t5,88(sp)
ld t6,96(sp)
ld t7,104(sp)
ld t8,112(sp)
ld t9,120(sp)
ld t10,128(sp)
ld t11,136(sp)
ld fp,144(sp)
flw ft0,152(sp)
flw ft1,156(sp)
flw ft2,160(sp)
flw ft3,164(sp)
flw ft4,168(sp)
flw ft5,172(sp)
flw ft6,176(sp)
flw ft7,180(sp)
ld ra,8(fp)
mv sp,fp
add sp,sp,8
ld fp,0(fp)
jr ra
.data
_frameSize_main .word 188
