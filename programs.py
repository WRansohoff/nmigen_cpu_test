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
loop_test = ROM( [ JMP( 29, 31 ) ] )

# "Quick Test" program: this application contains at least one of
# each supported machine code instruction, but it does not perform
# exhaustive tests for any particular instruction.
quick_test = ROM( [
  # ADDC, ADD (expect r0 = 0x00001234, r1 = 0x00002468)
  ADDC( 0, 31, 0x1234 ), ADD( 1, 0, 0 ),
  # BNE (expect r27 = 0x0C, PC skips over the following dummy data)
  BNE( 27, 0, 0x0004 ),
  0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF,
  # BEQ (expect r26 = 0x20, PC skips over the following dummy data)
  SUB( 22, 22, 22 ), BEQ( 26, 22, 0x0002 ), 0xDEADBEEF, 0xDEADBEEF,
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
  # SRAC, SRA (expect r17 = 0xFFFFFFFF, r18 = 0xFFFFE000)
  SRAC( 17, 15, 16 ), SRAC( 18, 15, 6 ),
  # XORC, XOR (expect r19 = 0xFFFF96F0, r20 = 0x000067FF)
  XORC( 19, 0, 0x84C4 ), XOR( 20, 17, 19 ),
  # XNORC, XNOR (expect r21 = 0xFFEDD9CB, r22 = 0x000067FF)
  XNORC( 21, 13, 0x1234 ), XNOR( 22, 17, 20 ),
  # LD (expect r23 = 0x77600004, r24 = 0x00000000)
  LD( 23, 31, 0x0008 ), LD( 24, 31, 0x0003 ),
  # ST (store 0x00001234 in RAM address 0x04.)
  ADDC( 25, 31, 1 ), SHLC( 25, 25, 29 ), ST( 0, 25, 0x0004 ),
  # LDR (expect r26 = 0x7F5FFFF, 0x63590004, 0x63590004)
  # Remember, LDR increments the PC *before* applying the offset.
  LDR( 26, 0xFFFF ), LDR( 26, 0x0001 ), LDR( 26, 0x0000 ),
  # LD (expect r26 = 0x00001234, loaded from 0x20000004 in RAM)
  LD( 26, 25, 0x00004 ),
  # JMP (rc = r28, ra = r29, PC returns to 0x00000000)
  JMP( 28, 29 ),
  # Dummy data (should not be reached).
  0x01234567, 0x89ABCDEF, 0xDEADBEEF, 0xFFFFFFFF, 0xFFFFFFFF
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
  0: [ { 'r': 'pc', 'v': 0x00000000 } ],
  1: [
       { 'r': 'pc', 'v': 0x00000000 },
       { 'r': 29,   'v': 0x00000004 }
     ],
  2: [
       { 'r': 'pc', 'v': 0x00000000 },
       { 'r': 29,   'v': 0x00000004 }
     ]
}

# Expected runtime values for the "Quick Test" program.
# These values are commented in the program above for each operation.
quick_exp = {
  # TODO: fill in actual expected values for the 'quick test' program.
  0: [ { 'r': 'pc', 'v': 0x00000000 } ],
  1: [ { 'r': 'pc', 'v': 0x00000004 } ],
  2: [ { 'r': 'pc', 'v': 0x00000008 } ],
  3: [ { 'r': 'pc', 'v': 0x0000001C } ]
}
