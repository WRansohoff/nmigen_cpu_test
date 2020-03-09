###############################################
# "Beta" ISA definitions and helper methods.  #
# This is an educational RISC instruction set #
# used in MIT's free online 6004.x2 edX class #
###############################################

from nmigen import *

from alu import *

# ISA overview: ('C' suffix means one arg is a 16-bit constant)
#   0b100000: ADD    (addition)
#   0b110000: ADDC   (addition w/ a constant)
#   0b101000: AND    (bitwise and)
#   0b111000: ANDC   (bitwise and w/ a constant)
#   0b011100: BEQ    (branch if equal)
#   0b011101: BNE    (branch if not equal)
#   0b100100: CMPEQ  (compare, check if ==)
#   0b110100: CMPEQC (compare, check if == a constant)
#   0b100110: CMPLE  (compare, check if <=)
#   0b110110: CMPLEC (compare, check if <= a constant)
#   0b100101: CMPLT  (compare, check if <)
#   0b110101: CMPLTC (compare, check if < a constant)
#   0b100011: DIV    (division)
#   0b110011: DIVC   (division w/ a constant)
#   0b011011: JMP    (unconditional jump)
#   0b011000: LD     (load)
#   0b011111: LDR    (load relative)
#   0b100010: MUL    (multiply)
#   0b110010: MULC   (multiply w/ a constant)
#   0b101001: OR     (bitwise or)
#   0b111001: ORC    (bitwise or w/ a constant)
#   0b101100: SHL    (left shift)
#   0b111100: SHLC   (left shift by constant # of bits)
#   0b101101: SHR    (right shift)
#   0b111101: SHRC   (right shift by constant # of bits)
#   0b101110: SRA    (right shift w/ sign extend)
#   0b111110: SRAC   (right shift w/ sign extend by constant)
#   0b100001: SUB    (subtraction)
#   0b110001: SUBC   (subtraction w/ a constant)
#   0b011001: ST     (store)
#   0b101010: XOR    (bitwise exclusive-or)
#   0b111010: XORC   (bitwise exclusive-or w/ a constant)
#   0b101011: XNOR   (bitwise exclusive-nor)
#   0b111011: XNORC  (bitwise exclusive-nor w/ a constant)
# CPU operation definitions: [ opcode, name ]
OP_ADD    = [ 0b100000, "ADD" ]
OP_ADDC   = [ 0b110000, "ADDC" ]
OP_AND    = [ 0b101000, "AND" ]
OP_ANDC   = [ 0b111000, "ANDC" ]
OP_BEQ    = [ 0b011100, "BEQ" ]
OP_BNE    = [ 0b011101, "BNE" ]
OP_CMPEQ  = [ 0b100100, "CMPEQ" ]
OP_CMPEQC = [ 0b110100, "CMPEQC" ]
OP_CMPLE  = [ 0b100110, "CMPLE" ]
OP_CMPLEC = [ 0b110110, "CMPLEC" ]
OP_CMPLT  = [ 0b100101, "CMPLT" ]
OP_CMPLTC = [ 0b110101, "CMPLTC" ]
OP_DIV    = [ 0b100011, "DIV" ]
OP_DIVC   = [ 0b110011, "DIVC" ]
OP_JMP    = [ 0b011011, "JMP" ]
OP_LD     = [ 0b011000, "LD" ]
OP_LDR    = [ 0b011111, "LDR" ]
OP_MUL    = [ 0b100010, "MUL" ]
OP_MULC   = [ 0b110010, "MULC" ]
OP_OR     = [ 0b101001, "OR" ]
OP_ORC    = [ 0b111001, "ORC" ]
OP_SHL    = [ 0b101100, "SHL" ]
OP_SHLC   = [ 0b111100, "SHLC" ]
OP_SHR    = [ 0b101101, "SHR" ]
OP_SHRC   = [ 0b111101, "SHRC" ]
OP_SRA    = [ 0b101110, "SRA" ]
OP_SRAC   = [ 0b111110, "SRAC" ]
OP_SUB    = [ 0b100001, "SUB" ]
OP_SUBC   = [ 0b110001, "SUBC" ]
OP_ST     = [ 0b011001, "ST" ]
OP_XOR    = [ 0b111010, "XOR" ]
OP_XORC   = [ 0b111010, "XORC" ]
OP_XNOR   = [ 0b101011, "XNOR" ]
OP_XNORC  = [ 0b111011, "XNORC" ]
ALU_OPS   = {
  OP_ADD[ 0 ]    : C_ADD[ 0 ],
  OP_ADDC[ 0 ]   : C_ADD[ 0 ],
  OP_AND[ 0 ]    : C_AND[ 0 ],
  OP_ANDC[ 0 ]   : C_AND[ 0 ],
  OP_CMPEQ[ 0 ]  : C_CEQ[ 0 ],
  OP_CMPEQC[ 0 ] : C_CEQ[ 0 ],
  OP_CMPLE[ 0 ]  : C_CLE[ 0 ],
  OP_CMPLEC[ 0 ] : C_CLE[ 0 ],
  OP_CMPLT[ 0 ]  : C_CLT[ 0 ],
  OP_CMPLTC[ 0 ] : C_CLT[ 0 ],
  OP_DIV[ 0 ]    : C_DIV[ 0 ],
  OP_DIVC[ 0 ]   : C_DIV[ 0 ],
  OP_MUL[ 0 ]    : C_MUL[ 0 ],
  OP_MULC[ 0 ]   : C_MUL[ 0 ],
  OP_OR[ 0 ]     : C_OR[ 0 ],
  OP_ORC[ 0 ]    : C_OR[ 0 ],
  OP_SHL[ 0 ]    : C_SHL[ 0 ],
  OP_SHLC[ 0 ]   : C_SHL[ 0 ],
  OP_SHR[ 0 ]    : C_SHR[ 0 ],
  OP_SHRC[ 0 ]   : C_SHR[ 0 ],
  OP_SRA[ 0 ]    : C_SRA[ 0 ],
  OP_SRAC[ 0 ]   : C_SRA[ 0 ],
  OP_SUB[ 0 ]    : C_SUB[ 0 ],
  OP_SUBC[ 0 ]   : C_SUB[ 0 ],
  OP_XOR[ 0 ]    : C_XOR[ 0 ],
  OP_XORC[ 0 ]   : C_XOR[ 0 ],
  OP_XNOR[ 0 ]   : C_XNOR[ 0 ],
  OP_XNORC[ 0 ]  : C_XNOR[ 0 ],
}
