from nmigen import *
from nmigen.back.pysim import *

from isa import *
from rom import *
from cpu import *

#####################################
# ROM images for CPU test programs: #
#####################################

# "Infinite Loop" program: I think this is the simplest error-free
# application that you could write, equivalent to "while(1){};".
loop_rom = ROM( [ JMP( 29, 31 ) ] )

# "Quick Test" program: this application contains at least one of
# each supported machine code instruction, but it does not perform
# exhaustive tests for any particular instruction.
quick_rom = ROM( [
  # ADDC, ADD (expect r0 = 0x00001234, r1 = 0x00002468)
  ADDC( 0, 31, 0x1234 ), ADD( 1, 0, 0 ),
  # BNE (expect r27 = 0x0C, PC skips over the following dummy data)
  BNE( 27, 0, 0x0004 ),
  0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF,
  # BEQ (expect r26 = 0x20, PC skips over the following dummy data)
  BEQ( 26, 31, 0x0002 ), 0xDEADBEEF, 0xDEADBEEF,
  # BEQ, BNE (expect r25 = 0x2C, r24 = 0x30, but no branching.)
  BEQ( 25, 0, 0xFFFF ), BNE( 24, 22, 0xFFFF ),
  # ANDC, AND (expect r2 = r3 = 0x00001200)
  ANDC( 2, 0, 0x1200 ), AND( 3, 2, 0 ),
  # DIVC, DIV (expect r5 = 0x00001234, r6 = 0x00000002)
  DIVC( 5, 1, 0x0002 ), DIV( 6, 1, 5 ),
  # MULC, MUL (expect r7 = 0x0000002A = 42, r8 = 0x000006E4 = 1764)
  MULC( 7, 6, 21 ), MUL( 8, 7, 7 ),
  # CMPEQC, CMPEQ (expect r4 = 1, 0, 1, 0)
  CMPEQC( 4, 0, 0x1234 ), CMPEQC( 4, 0, 0x4321 ),
  CMPEQ( 4, 2, 3 ), CMPEQ( 4, 0, 3 ),
  # CMPLEC, CMPLE (expect r4 = 1, 1, 0, 1, 1, 0)
  CMPLEC( 4, 0, 0x1235 ), CMPLEC( 4, 0, 0x1234 ),
  CMPLEC( 4, 0, 0x1233 ), CMPLE( 4, 0, 1 ),
  CMPLE( 4, 0, 0 ), CMPLE( 4, 1, 0 ),
  # CMPLTC, CMPLT (expect r4 = 1, 0, 0, 1, 0, 0)
  CMPLTC( 4, 0, 0x1235 ), CMPLTC( 4, 0, 0x1234 ),
  CMPLTC( 4, 0, 0x1233 ), CMPLT( 4, 0, 1 ),
  CMPLT( 4, 0, 0 ), CMPLT( 4, 1, 0 ),
  # ORC, OR (expect r9 = 0x00005674, r10 = 0x0000567E)
  ORC( 9, 5, 0x4444 ), OR( 10, 9, 7 ),
  # SHLC, SHL (expect r11 = 0x12340000, r12 = 0x000159D0)
  SHLC( 11, 0, 16 ), SHL( 12, 9, 6 ),
  # SHRC, SHR (expect r13 = 0x00123400, r14 = 0x00048D00)
  SHRC( 13, 11, 8 ), SHR( 14, 13, 6 ),
  # SUBC, SUB (expect r15 = 0xFFFF8003, r16 = 0x00001234)
  SUBC( 15, 6, 0x7FFF ), SUB( 16, 1, 0 ),
  # SRAC, SRA (expect r17 = 0xFFFFFFFF, r18 = 0xFFFFFE00)
  SRAC( 17, 15, 16 ), SRAC( 18, 15, 6 ),
  # XORC, XOR (expect r19 = 0x0000690F, r20 = 0x000067FF)
  XORC( 19, 0, 0x84C4 ), XOR( 20, 17, 19 ),
  # XNORC, XNOR (expect r21 = 0xFFEDD9CB, r22 = 0x0000690F)
  XNORC( 21, 13, 0x1234 ), XNOR( 22, 17, 20 ),
  # LD (expect r23 = 0x77600004, r24 = 0x00000000)
  LD( 23, 31, 0x0008 ), LD( 24, 31, 0x0003 ),
  # ST (store 0x00001234 in RAM address 0x04.)
  ADDC( 25, 31, 1 ), SHLC( 25, 25, 29 ), ST( 0, 25, 0x0004 ),
  # LDR (expect r26 = 0x7F5FFFFF, 0x63590004, 0x63590004)
  # Remember, LDR increments the PC *before* applying the offset.
  LDR( 26, 0xFFFF ), LDR( 26, 0x0001 ), LDR( 26, 0x0000 ),
  # LD (expect r26 = 0x00001234, loaded from 0x20000004 in RAM)
  LD( 26, 25, 0x0004 ),
  # JMP (rc = r28, ra = r29, PC returns to 0x00000000)
  JMP( 28, 29 ),
  # Dummy data (should not be reached).
  0x01234567, 0x89ABCDEF, 0xDEADBEEF, 0xFFFFFFFF, 0xFFFFFFFF
] )

# "Addition Test" program: check that the 'ADD' and 'ADDC'
# operations work correctly.
add_rom = ROM( [
  # r0 = 0 + 1 (= 1), r1 = r0 + r0 (= 2), r2 = r1 + 3 (= 5)
  ADDC( 0, 31, 1 ), ADD( 1, 0, 0 ), ADDC( 2, 1, 3 ),
  # r3 = r2 + -2 (= 3), r4 = r1 + -4 (= -2), r5 = r0 + 32767 (= 32768)
  ADDC( 3, 2, -2 ), ADDC( 4, 1, -4 ), ADDC( 5, 0, 0x7FFF ),
  # r6 = r5 + -32768 (= 0), r7 = r5 + r5 (= 65536)
  ADDC( 6, 5, 0x8000 ), ADD( 7, 5, 5 ),
  # Set r8 to (int max) and r9 to (int min)
  SHLC( 9, 0, 31 ), SRAC( 8, 9, 32 ), XOR( 8, 9, 8 ),
  # r10 = min + max = r11 = max + min (= -1)
  ADD( 10, 9, 8 ), ADD( 11, 8, 9 ),
  # r12 = max + max (= -2), r13 = min + min (= 0)
  ADD( 12, 8, 8 ), ADD( 13, 9, 9 ),
  # r14 = min + 1 (= 0x80000001), r15 = min + -1 (= max)
  ADDC( 14, 9, 1 ), ADDC( 15, 9, -1 ),
  # r16 = max + 1 (= min), r17 = max + -1 (= 0x7FFFFFFE)
  ADDC( 16, 8, 1 ), ADDC( 17, 8, -1 ),
  # r18 = 42, r19 = 82, r20 = 999
  ADDC( 18, 4, 44 ), ADD( 19, 18, 18 ),
  ADD( 19, 19, 4 ), ADDC( 20, 19, 917 ),
  # Done; infinite loop. (r28 = jump address, r29 = r28 + 4)
  LDR( 28, 0x0000 ), JMP( 29, 28 )
] )

# "Subtraction test" program: check that the 'SUB' and 'SUBC'
# operations work correctly.
sub_rom = ROM( [
  # r0 = 0 - 1 (= -1), r1 = r0 - 8 (= -9), r2 = r1 - -12 (= 3)
  SUBC( 0, 31, 1 ), SUBC( 1, 0, 8 ), SUBC( 2, 1, -12 ),
  # r3 = r0 - r0 (= 0), r4 = r1 - -12 (= 3)
  SUB( 3, 0, 0 ), SUBC( 4, 1, -12 ),
  # r5 = r0 - -32768 (= 32767), r6 = r5 - 32767 (= 0)
  SUBC( 5, 0, 0x8000 ), SUBC( 6, 5, 0x7FFF ),
  # r7 = min, r8 = max.
  SHRC( 8, 0, 1 ), XOR( 7, 8, 0 ),
  # r9 = min - max (=), r10 = max - min (=)
  SUB( 9, 7, 8 ), SUB( 10, 8, 7 ),
  # r11 = max - max (= 0), r12 = min - min (= 0)
  SUB( 11, 8, 8 ), SUB( 12, 7, 7 ),
  # r13 = min - 1 (= -1), r14 = max - 1 (=)
  SUBC( 13, 7, 1 ), SUBC( 14, 8, 1 ),
  # r15 = min - -1 (=), r16 = max - -1 (=)
  SUBC( 15, 7, -1 ), SUBC( 16, 8, -1 ),
  # r17 = 42, r18 = -84, r19 = 999
  SUBC( 17, 2, -39 ), SUBC( 18, 17, 126 ), SUBC( 19, 5, 31768 ),
  # Done; infinite loop. (r28 = jump address, r29 = r28 + 4)
  LDR( 28, 0x0000 ), JMP( 29, 28 )
] )

# Boolean test program: check that 'AND', 'ANDC', 'OR', 'ORC',
# 'XOR', 'XORC', 'XNOR', and 'XNORC' operations work correctly.
bool_rom = ROM( [
  # Initial values: r0 = 0xFFFFFFFF, r1 = 0x00000000, r2 = 0x5555AAAA
  SUBC( 0, 31, 1 ), ADD( 1, 31, 31 ), ADDC( 2, 31, 0xAAAA ),
  SHLC( 2, 2, 16 ), XORC( 2, 2, 0xAAAA ),
  # r3 = 0x5555AAAA, r4 = 0x00000000
  AND( 3, 0, 2 ), AND( 4, 1, 0 ),
  # r5 = 0x5555AAAA, r6 = 0xFFFF8000, r7 = 0x00000001
  ANDC( 5, 2, 0xAAAA ), ANDC( 6, 0, 0x8000 ), ANDC( 7, 0, 0x0001 ),
  # r8 = 0x00000001, r9 = 0xFFFF8000, r10 = 0x5555AAAB
  OR( 8, 7, 4 ), OR( 9, 4, 6 ), OR( 10, 5, 7 ),
  # r11 = 0x00000000, r12 = 0x00001234, r13 = 0xFFFFCC00
  OR( 11, 1, 1 ), ORC( 12, 4, 0x1234 ), ORC( 13, 4, 0xCC00 ),
  # r14 = 0xAAAA5555, r15 = 0x00000000, r16 = 0x0000785A
  XOR( 14, 5, 0 ), XOR( 15, 5, 5 ), XORC( 16, 0, 0x8765 ),
  # r17 = 0xFFFFFFFF, r18 = 0x5555D231, r19 = 0xAAAA023C
  XOR( 17, 5, 14 ), XOR( 18, 10, 16 ), XORC( 19, 18, 0xD00D ),
  # r20 = 0x00000000, r21 = 0xAAAA023C, r22 = 0x5555FDC3
  XNOR( 20, 18, 19 ), XNOR( 21, 0, 19 ), XNOR( 22, 1, 19 ),
  # r23 = 0x, r24 = 0x
  XNORC( 23, 21, 0xC001 ), XNORC( 24, 21, 0x1337 ),
  # r25 = 0x, r26 = 0x
  XNORC( 25, 0, 0x0000 ), XNORC ( 25, 0, 0xFFFF ),
  # Done; infinite loop. (r28 = jump address, r29 = r28 + 4)
  LDR( 28, 0x0000 ), JMP( 29, 28 )
] )

# Comparison test program: check that 'CMPEQ', 'CMPLE', 'CMPLT'
# operations work correctly.
cmp_rom = ROM( [
  # Load some initial values: r0 = 2, r1 = 7, r2 = -2, r3 = -7
  ADDC( 0, 31, 2 ), ADDC( 1, 31, 7 ),
  SUBC( 2, 31, 2 ), SUBC( 3, 31, 7 ),
  # r4 = 1, r5 = 1, r6 = 1
  CMPEQ( 4, 0, 0 ), CMPEQ( 5, 2, 2 ), CMPEQC( 6, 1, 7 ),
  # r7 = 1, r8 = 0, r9 = 0
  CMPEQC( 7, 3, -7 ), CMPEQ( 8, 0, 1 ), CMPEQ( 9, 0, 2 ),
  # r10 = 0, r11 = 0, r12 = 0,
  CMPEQ( 10, 1, 3 ), CMPEQC( 11, 0, -2 ), CMPEQC( 12, 3, 7 ),
  # r13 = 1, r14 = 0, r15 = 1
  CMPLE( 13, 0, 1 ), CMPLE( 14, 1, 0 ), CMPLE( 15, 3, 2 ),
  # r16 = 0, r17 = 1, r18 = 0
  CMPLE( 16, 2, 3 ), CMPLE( 17, 2, 1 ), CMPLE( 18, 1, 2 ),
  # r19 = 1, r20 = 1, r21 = 1
  CMPLE( 19, 0, 0 ), CMPLE( 20, 3, 3 ), CMPLEC( 21, 0, 4 ),
  # r22 = 1, r23 = 0, r24 = 0
  CMPLEC( 22, 0, 2 ), CMPLEC( 23, 0, 1 ), CMPLEC( 24, 0, -8 ),
  # r25 = 1, r26 = 0, r27 = 1
  CMPLEC( 25, 3, -6 ), CMPLEC( 26, 3, -8 ), CMPLEC( 27, 3, 7 ),
  # r28 = 1, r29 = 1
  CMPLEC( 28, 0, 2 ), CMPLEC( 29, 3, -7 ),
  # r13 = 1, r14 = 0, r15 = 1
  CMPLT( 13, 0, 1 ), CMPLT( 14, 1, 0 ), CMPLT( 15, 3, 2 ),
  # r16 = 0, r17 = 1, r18 = 0
  CMPLT( 16, 2, 3 ), CMPLT( 17, 2, 1 ), CMPLT( 18, 1, 2 ),
  # r19 = 0, r20 = 0, r21 = 1
  CMPLT( 19, 0, 0 ), CMPLT( 20, 3, 3 ), CMPLTC( 21, 0, 4 ),
  # r22 = 0, r23 = 0, r24 = 0
  CMPLTC( 22, 0, 2 ), CMPLTC( 23, 0, 1 ), CMPLTC( 24, 0, -8 ),
  # r25 = 1, r26 = 0, r27 = 1
  CMPLTC( 25, 3, -6 ), CMPLTC( 26, 3, -8 ), CMPLTC( 27, 3, 7 ),
  # r28 = 0, r29 = 0
  CMPLTC( 28, 0, 2 ), CMPLTC( 29, 3, -7 ),
  # Done; infinite loop. (r28 = jump address, r29 = r28 + 4)
  LDR( 28, 0x0000 ), JMP( 29, 28 )
] )

# Branch and jump test program: check that 'BEQ', 'BNE', and 'JMP'
# operations work correctly.
jmp_rom = ROM( [
  # Load initial values: r0 = 1, r1 = -1
  ADDC( 0, 31, 1 ), SUBC( 1, 31, 1 ),
  # BEQ tests: first two branches shouldn't be taken, third should.
  BEQ( 2, 0, -1 ), BEQ( 3, 1, -1 ), BEQ( 4, 31, 2 ),
  # (PC should skip over this dummy data.)
  0xDEADBEEF, 0xDEADBEEF,
  # BNE tests: first branch shouldn't be taken, third should.
  BNE( 5, 31, -1 ), BNE( 6, 0, 2 ), 0xDEADBEEF, 0xDEADBEEF,
  BNE( 7, 1, 2 ), 0xDEADBEEF, 0xDEADBEEF,
  # Jump forward, then back, then forward.
  BNE( 8, 31, 1 ), ADDC( 9, 8, 0x0014 ), ADDC( 10, 8, 0x0028 ),
  ADDC( 11, 8, 0x0010 ), JMP( 12, 9 ), JMP( 12, 10 ), JMP( 12, 11 ),
  # Dummy data that should be jumped over.
  0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF,
  # Done; infinite loop. (r28 = jump address, r29 = r28 + 4)
  BNE( 28, 31, 0 ), JMP( 29, 28 )
] )

########################################
# Expected runtime register values for #
# the CPU test programs defined above: #
########################################

# Expected runtime values for the "Infinite Loop" program.
# Since the application only contains a single 'jump' instruction,
# we can expect the PC to always equal 0 and r29 to hold 0x04 (the
# 'return PC' value) after the first 'jump' instruction is executed.
loop_exp = {
  0: [ { 'r': 'pc', 'e': 0x00000000 } ],
  1: [
       { 'r': 'pc', 'e': 0x00000000 },
       { 'r': 29,   'e': 0x00000004 }
     ],
  2: [
       { 'r': 'pc', 'e': 0x00000000 },
       { 'r': 29,   'e': 0x00000004 }
     ],
  'end': 2
}

# Expected runtime values for the "Quick Test" program.
# These values are commented in the program above for each operation.
quick_exp = {
  # Starting state: PC = 0.
  0:  [ { 'r': 'pc', 'e': 0x00000000 } ],
  # After the first 'ADD' ops, r0 = 0x00001234 and r1 = 0x00002468.
  2:  [
        { 'r': 'pc', 'e': 0x00000008 },
        { 'r': 0,    'e': 0x00001234 },
        { 'r': 1,    'e': 0x00002468 }
      ],
  # After the next 'BNE' op, r27 = 0x0000000C and the PC skips ahead.
  3:  [
        { 'r': 'pc', 'e': 0x0000001C },
        { 'r': 27,   'e': 0x0000000C }
      ],
  # After the next 'BEQ' op, r26 = 0x00000020, and PC skips again.
  4:  [
        { 'r': 'pc', 'e': 0x00000028 },
        { 'r': 26,   'e': 0x00000020 }
      ],
  # Two more branch operations set r25 = 0x0000002C, r24 = 0x00000030,
  # but the PC does not jump ahead.
  6:  [
        { 'r': 'pc', 'e': 0x00000030 },
        { 'r': 25,   'e': 0x0000002C },
        { 'r': 24,   'e': 0x00000030 }
      ],
  # Next are two 'AND' ops: r2 = r3 = 0x00001200.
  8:  [
        { 'r': 2, 'e': 0x00001200 },
        { 'r': 3, 'e': 0x00001200 }
      ],
  # Then come some 'DIV' and 'MUL' operations.
  12: [
        { 'r': 5, 'e': 0x00001234 },
        { 'r': 6, 'e': 0x00000002 },
        { 'r': 7, 'e': 0x0000002A },
        { 'r': 8, 'e': 0x000006E4 }
      ],
  # A series of compare operations produce 1s or 0s.
  13: [ { 'r': 4, 'e': 1 } ],
  14: [ { 'r': 4, 'e': 0 } ],
  15: [ { 'r': 4, 'e': 1 } ],
  16: [ { 'r': 4, 'e': 0 } ],
  17: [ { 'r': 4, 'e': 1 } ],
  18: [ { 'r': 4, 'e': 1 } ],
  19: [ { 'r': 4, 'e': 0 } ],
  20: [ { 'r': 4, 'e': 1 } ],
  21: [ { 'r': 4, 'e': 1 } ],
  22: [ { 'r': 4, 'e': 0 } ],
  23: [ { 'r': 4, 'e': 1 } ],
  24: [ { 'r': 4, 'e': 0 } ],
  25: [ { 'r': 4, 'e': 0 } ],
  26: [ { 'r': 4, 'e': 1 } ],
  27: [ { 'r': 4, 'e': 0 } ],
  28: [ { 'r': 4, 'e': 0 } ],
  # Two 'OR' ops: r9 = 0x00005674, r10 = 0x0000567E
  30: [
        { 'r': 9,  'e': 0x00005674 },
        { 'r': 10, 'e': 0x0000567E }
      ],
  # Two 'SUB' and six shift operations set r11-r18.
  38: [
        { 'r': 11, 'e': 0x12340000 },
        { 'r': 12, 'e': 0x000159D0 },
        { 'r': 13, 'e': 0x00123400 },
        { 'r': 14, 'e': 0x00048D00 },
        { 'r': 15, 'e': 0xFFFF8003 },
        { 'r': 16, 'e': 0x00001234 },
        { 'r': 17, 'e': 0xFFFFFFFF },
        { 'r': 18, 'e': 0xFFFFFE00 }
      ],
  # Four more 'XOR' and 'XNOR' ops set r19-r22.
  42: [
        { 'r': 19, 'e': 0xFFFF96F0 },
        { 'r': 20, 'e': 0x0000690F },
        { 'r': 21, 'e': 0xFFEDD9CB },
        { 'r': 22, 'e': 0x0000690F }
      ],
  # Two 'LD' operations load r23 = LD( 26, 25, 0x0004 ), and verify
  # that a mis-aligned load places 0 in r24.
  44: [
        { 'r': 23, 'e': 0x77600004 },
        { 'r': 24, 'e': 0x00000000 },
      ],
  # Three more ops store 0x00001234 into the second word of RAM.
  47: [ { 'r': "RAM%d"%( 0x04 ), 'e': 0x00001234 } ],
  # Four more 'load' ops place different values in r26.
  48: [ { 'r': 26, 'e': 0x7F5FFFFF } ],
  49: [ { 'r': 26, 'e': 0x63590004 } ],
  50: [ { 'r': 26, 'e': 0x63590004 } ],
  51: [ { 'r': 26, 'e': 0x00001234 } ],
  # Finally, the PC should jump back to address 0 and start again.
  52: [ { 'r': 'pc', 'e': 0x00000000 } ],
  'end': 52
}

# Expected runtime values for the addition test program.
add_exp = {
  # The first 8 add operations should set: r0 = 1, r1 = 2, r2 = 5,
  # r3 = 3, r4 = -2, r5 = 32768, r6 = 0, r7 = 65536.
  1:  [ { 'r': 0, 'e': 1, 'n': 0, 'z': 0, 'v': 0 } ],
  2:  [ { 'r': 1, 'e': 2, 'n': 0, 'z': 0, 'v': 0 } ],
  3:  [ { 'r': 2, 'e': 5, 'n': 0, 'z': 0, 'v': 0 } ],
  4:  [ { 'r': 3, 'e': 3, 'n': 0, 'z': 0, 'v': 0 } ],
  5:  [ { 'r': 4, 'e': -2, 'n': 1, 'z': 0, 'v': 0 } ],
  6:  [ { 'r': 5, 'e': 0x8000, 'n': 0, 'z': 0, 'v': 0 } ],
  7:  [ { 'r': 6, 'e': 0, 'n': 0, 'z': 1, 'v': 0 } ],
  8:  [ { 'r': 7, 'e': 0x10000, 'n': 0, 'z': 0, 'v': 0 } ],
  # The next 3 ops should set r8 = 0x7FFFFFFF and r9 = 0x80000000.
  11: [ { 'r': 8, 'e': 0x7FFFFFFF }, { 'r': 9, 'e': 0x80000000 } ],
  # The next 4 ops add (max/min) + (max/min) in r10-r13
  12: [ { 'r': 10, 'e': -1, 'n': 1, 'z': 0, 'v': 0 } ],
  13: [ { 'r': 11, 'e': -1, 'n': 1, 'z': 0, 'v': 0 } ],
  14: [ { 'r': 12, 'e': -2, 'n': 1, 'z': 0, 'v': 1 } ],
  15: [ { 'r': 13, 'e': 0, 'n': 0, 'z': 1, 'v': 1 } ],
  # Then, min/max +/- 1 in r14-17.
  16: [ { 'r': 14, 'e': 0x80000001, 'n': 1, 'z': 0, 'v': 0 } ],
  17: [ { 'r': 15, 'e': 0x7FFFFFFF, 'n': 0, 'z': 0, 'v': 1 } ],
  18: [ { 'r': 16, 'e': 0x80000000, 'n': 1, 'z': 0, 'v': 1 } ],
  19: [ { 'r': 17, 'e': 0x7FFFFFFE, 'n': 0, 'z': 0, 'v': 0 } ],
  # A few more small-number checks.
  20: [ { 'r': 18, 'e': 42, 'n': 0, 'z': 0, 'v': 0 } ],
  22: [ { 'r': 19, 'e': 82, 'n': 0, 'z': 0, 'v': 0 } ],
  23: [ { 'r': 20, 'e': 999, 'n': 0, 'z': 0, 'v': 0 } ],
  'end': 25
}

# Expected runtime values for the subtraction test program.
sub_exp = {
  # The first 7 add operations should set: r0 = -1, r1 = -2, r2 = 3,
  # r3 = 0, r4 = 3, r5 = 32767, r6 = 0.
  1:  [ { 'r': 0, 'e': -1, 'n': 1, 'z': 0, 'v': 0 } ],
  2:  [ { 'r': 1, 'e': -9, 'n': 1, 'z': 0, 'v': 0 } ],
  3:  [ { 'r': 2, 'e': 3, 'n': 0, 'z': 0, 'v': 0 } ],
  4:  [ { 'r': 3, 'e': 0, 'n': 0, 'z': 1, 'v': 0 } ],
  5:  [ { 'r': 4, 'e': 3, 'n': 0, 'z': 0, 'v': 0 } ],
  6:  [ { 'r': 5, 'e': 0x7FFF, 'n': 0, 'z': 0, 'v': 0 } ],
  7:  [ { 'r': 6, 'e': 0, 'n': 0, 'z': 1, 'v': 0 } ],
  # The next 2 ops should set r7 = 0x80000000 and r8 = 0x7FFFFFFF.
  8:  [ { 'r': 8, 'e': 0x7FFFFFFF, 'n': 0, 'z': 0, 'v': 0 } ],
  9:  [ { 'r': 7, 'e': 0x80000000, 'n': 1, 'z': 0, 'v': 0 } ],
  # The next 4 ops add (max/min) + (max/min) in r9-r12
  10: [ { 'r': 9, 'e': 1, 'n': 0, 'z': 0, 'v': 1 } ],
  11: [ { 'r': 10, 'e': -1, 'n': 1, 'z': 0, 'v': 1 } ],
  12: [ { 'r': 11, 'e': 0, 'n': 0, 'z': 1, 'v': 0 } ],
  13: [ { 'r': 12, 'e': 0, 'n': 0, 'z': 1, 'v': 0 } ],
  # Then, min/max +/- 1 in r13-16.
  14: [ { 'r': 13, 'e': 0x7FFFFFFF, 'n': 0, 'z': 0, 'v': 1 } ],
  15: [ { 'r': 14, 'e': 0x7FFFFFFE, 'n': 0, 'z': 0, 'v': 0 } ],
  16: [ { 'r': 15, 'e': 0x80000001, 'n': 1, 'z': 0, 'v': 0 } ],
  17: [ { 'r': 16, 'e': 0x80000000, 'n': 1, 'z': 0, 'v': 1 } ],
  # A few more small-number checks.
  18: [ { 'r': 17, 'e': 42, 'n': 0, 'z': 0, 'v': 0 } ],
  19: [ { 'r': 18, 'e': -84, 'n': 1, 'z': 0, 'v': 0 } ],
  20: [ { 'r': 19, 'e': 999, 'n': 0, 'z': 0, 'v': 0 } ],
  'end': 20
}

# Expected runtime values for the 'boolean operators' test program.
bool_exp = {
  # The first 5 operations set initial values.
  5: [
       { 'r': 0, 'e': 0xFFFFFFFF },
       { 'r': 1, 'e': 0x00000000 },
       { 'r': 2, 'e': 0x5555AAAA }
     ],
  # 5 AND operations set r3-r7.
  10: [
       { 'r': 3, 'e': 0x5555AAAA },
       { 'r': 4, 'e': 0x00000000 },
       { 'r': 5, 'e': 0x5555AAAA },
       { 'r': 6, 'e': 0xFFFF8000 },
       { 'r': 7, 'e': 0x00000001 }
     ],
  # 6 OR operations set r8-r13.
  16: [
        { 'r': 8,  'e': 0x00000001 },
        { 'r': 9,  'e': 0xFFFF8000 },
        { 'r': 10, 'e': 0x5555AAAB },
        { 'r': 11, 'e': 0x00000000 },
        { 'r': 12, 'e': 0x00001234 },
        { 'r': 13, 'e': 0xFFFFCC00 }
      ],
  # 6 XOR operations set r14-r19.
  24: [
        { 'r': 14, 'e': 0xAAAA5555 },
        { 'r': 15, 'e': 0x00000000 },
        { 'r': 16, 'e': 0x0000789A },
        { 'r': 17, 'e': 0xFFFFFFFF },
        { 'r': 18, 'e': 0x5555D231 },
        { 'r': 19, 'e': 0xAAAA023C }
      ],
  # Finally, 7 XNOR operations set r20-r26
  31: [
        { 'r': 20, 'e': 0x00002FF2 },
        { 'r': 21, 'e': 0xAAAA023C },
        { 'r': 22, 'e': 0x5555FDC3 },
        { 'r': 23, 'e': 0xAAAA3DC2 },
        { 'r': 24, 'e': 0x5555EEF4 },
        { 'r': 25, 'e': 0xFFFFFFFF },
        { 'r': 26, 'e': 0x00000000 }
      ],
  'end': 32
}

# Expected runtime values for the 'comparison operators' test program.
cmp_exp = {
  # The first 4 operations set initial values.
  4:  [
        { 'r': 0, 'e':  2 }, { 'r': 1, 'e':  7 },
        { 'r': 2, 'e': -2 }, { 'r': 3, 'e': -7 }
      ],
  # The next 9 operations test CMPEQ and CMPEQC in r4-r12.
  13: [
        { 'r':  4, 'e': 1 }, { 'r':  5, 'e': 1 }, { 'r':  6, 'e': 1 },
        { 'r':  7, 'e': 1 }, { 'r':  8, 'e': 0 }, { 'r':  9, 'e': 0 },
        { 'r': 10, 'e': 0 }, { 'r': 11, 'e': 0 }, { 'r': 12, 'e': 0 }
      ],
  # The next 17 operations test CMPLE and CMPLEC r13-r29.
  30: [
        { 'r': 13, 'e': 1 }, { 'r': 14, 'e': 0 }, { 'r': 15, 'e': 1 },
        { 'r': 16, 'e': 0 }, { 'r': 17, 'e': 1 }, { 'r': 18, 'e': 0 },
        { 'r': 19, 'e': 1 }, { 'r': 20, 'e': 1 }, { 'r': 21, 'e': 1 },
        { 'r': 22, 'e': 1 }, { 'r': 23, 'e': 0 }, { 'r': 24, 'e': 0 },
        { 'r': 25, 'e': 1 }, { 'r': 26, 'e': 0 }, { 'r': 27, 'e': 1 },
        { 'r': 28, 'e': 1 }, { 'r': 29, 'e': 1 }
      ],
  # The next 17 operations test CMPLT and CMPLTC r13-r29.
  47: [
        { 'r': 13, 'e': 1 }, { 'r': 14, 'e': 0 }, { 'r': 15, 'e': 1 },
        { 'r': 16, 'e': 0 }, { 'r': 17, 'e': 1 }, { 'r': 18, 'e': 0 },
        { 'r': 19, 'e': 0 }, { 'r': 20, 'e': 0 }, { 'r': 21, 'e': 1 },
        { 'r': 22, 'e': 0 }, { 'r': 23, 'e': 0 }, { 'r': 24, 'e': 0 },
        { 'r': 25, 'e': 1 }, { 'r': 26, 'e': 0 }, { 'r': 27, 'e': 1 },
        { 'r': 28, 'e': 0 }, { 'r': 29, 'e': 0 }
      ],
  'end': 50
}

# Expected runtime values for the 'branch / jump' test program.
jmp_exp = {
  # First two instructions set initial values.
  2: [ { 'r': 0, 'e': 1 }, { 'r': 1, 'e': -1 } ],
  # The next two 'BEQ' instructions should not be taken.
  3: [ { 'r': 'pc', 'e': 0x0000000C } ],
  4: [ { 'r': 'pc', 'e': 0x00000010 } ],
  # But the third 'BEQ' should skip ahead.
  5: [ { 'r': 'pc', 'e': 0x0000001C } ],
  # The first 'BNE' should not be taken.
  6: [ { 'r': 'pc', 'e': 0x00000020 } ],
  # But the second two 'BNE's should skip ahead.
  7: [ { 'r': 'pc', 'e': 0x0000002C } ],
  8: [ { 'r': 'pc', 'e': 0x00000038 } ],
  # The next 4 instructions generate PC addresses to jump between.
  12: [
        { 'r':  8, 'e': 0x0000003C },
        { 'r':  9, 'e': 0x00000050 },
        { 'r': 10, 'e': 0x00000064 },
        { 'r': 11, 'e': 0x0000004C }
      ],
  # The next 3 jumps should skip forwards, back, forwards.
  13: [ { 'r': 'pc', 'e': 0x00000050 } ],
  14: [ { 'r': 'pc', 'e': 0x0000004C } ],
  15: [ { 'r': 'pc', 'e': 0x00000064 } ],
  # Finally, there's an infinite loop.
  16: [ { 'r': 'pc', 'e': 0x00000068 } ],
  17: [ { 'r': 'pc', 'e': 0x00000068 } ],
  20: [ { 'r': 'pc', 'e': 0x00000068 } ],
  'end': 20
}

############################################
# Collected definitions for test programs. #
# These are just arrays with string names, #
# ROM images, and expected runtime values. #
############################################

loop_test  = [ 'inifinite loop test', 'cpu_loop', loop_rom, loop_exp ]
quick_test = [ 'quick test', 'cpu_quick', quick_rom, quick_exp ]
add_test   = [ 'addition test', 'cpu_add', add_rom, add_exp ]
sub_test   = [ 'subtraction test', 'cpu_sub', sub_rom, sub_exp ]
bool_test  = [ 'boolean test', 'cpu_bool', bool_rom, bool_exp ]
cmp_test  = [ 'comparison test', 'cpu_cmp', cmp_rom, cmp_exp ]
jmp_test  = [ 'branch / jump test', 'cpu_jmp', jmp_rom, jmp_exp ]
