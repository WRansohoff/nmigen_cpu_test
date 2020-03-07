from nmigen import *
from nmigen.back.pysim import *

from alu import *
from rom import *
from ram import *

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

###############
# CPU module: #
###############

class CPU( Elaboratable ):
  def __init__( self, rom_module ):
    # Program Counter register.
    self.pc = Signal( 32, reset = 0x00000000 )
    # Exception Pointer register.
    self.xp = Signal( 32, reset = 0x00000000 )
    # The main 32 CPU registers.
    self.r  = [
      Signal( 32, reset = 0x00000000, name = "r%d"%i )
      for i in range( 32 )
    ]
    # The ALU submodule which performs logical operations.
    self.alu = ALU()
    # The ROM submodule which acts as simulated program data storage.
    self.rom = rom
    # The RAM submodule which simulates re-writable data storage.
    # (512 bytes of RAM = 128 words)
    self.ram = RAM( 128 )

  def elaborate( self, platform ):
    # Core CPU module.
    m = Module()
    # Register the ALU, ROM and RAM submodules.
    m.submodules.alu = self.alu
    m.submodules.rom = self.rom
    m.submodules.ram = self.ram

    # Intermediate instruction storage.
    opcode = Signal( 6, reset = 0b000000 )
    ra = Signal( 5, reset = 0b00000 )
    rb = Signal( 5, reset = 0b00000 )
    rc = Signal( 5, reset = 0b00000 )
    imm = Signal( 32, reset = 0x00000000 )
    imm_signex = Signal( 32, reset = 0x00000000 )

    # r31 is hard-wired to 0.
    self.r[ 31 ].eq( 0x00000000 )
    # r30 is the exception pointer.
    self.r[ 30 ].eq( self.xp )
    # Hard-wire the program counter to the simulated ROM address.
    m.d.comb += self.rom.addr.eq( self.pc )
    # Set the ALU's 'start' bit to 0 by default.
    m.d.comb += self.alu.start.eq( 0 )

    # Main CPU FSM.
    with m.FSM() as fsm:
      # "Load PC": Fetch the memory location in the program counter
      #            from ROM, to prepare for decoding.
      with m.State( "CPU_PC_LOAD" ):
        m.next = "CPU_PC_ROM_FETCH"
      # "ROM Fetch": Wait for the instruction to load from ROM, and
      #              populate register fields to prepare for decoding.
      with m.State( "CPU_PC_ROM_FETCH" ):
        with m.If( self.rom.out[ 15 ] ):
          m.d.comb += imm_signex.eq( 0xFFFF0000 )
        with m.Else():
          m.d.comb += imm_signex.eq( 0x00000000 )
        m.d.sync += [
          opcode.eq( self.rom.out.bit_select( 26, 6 ) ),
          rc.eq( self.rom.out.bit_select( 21, 5 ) ),
          ra.eq( self.rom.out.bit_select( 16, 5 ) ),
          rb.eq( self.rom.out.bit_select( 11, 5 ) ),
          imm.eq( self.rom.out.bit_select( 0, 16 ) | imm_signex )
        ]
        m.next = "CPU_PC_DECODE"
      # "Decode PC": Figure out what sort of instruction to execute,
      #              and prepare associated registers.
      with m.State( "CPU_PC_DECODE" ):
        # Load/Store ops: LD, LDR, ST.
        with m.If( ( opcode == OP_LD[ 0 ] ) |
                   ( opcode == OP_LDR[ 0 ] ) |
                   ( opcode == OP_ST[ 0 ] ) ):
          # TODO: Load/Store logic.
          m.next = "CPU_LS"
        # Branch/Jump ops: JMP, BEQ, BNE.
        # JMP "Jump" operation: Place the next PC value in Rc, then
        # set PC to (Ra & 0xFFFFFFFC) to ensure it is word-aligned.
        with m.Elif( opcode == OP_JMP[ 0 ] ):
          for i in range( 32 ):
            with m.If( rc == i ):
              m.d.sync += self.r[ i ].eq( self.pc + 4 )
            with m.If( ra == i ):
              m.d.sync += self.pc.eq( self.r[ i ] & 0xFFFFFFFC )
          m.next = "CPU_PC_LOAD"
        # TODO: BEQ / BNE "Branch" operations.
        with m.Elif( ( opcode == OP_BEQ[ 0 ] ) |
                     ( opcode == OP_BNE[ 0 ] ) ):
          m.next = "CPU_JMP"
        # ALU instructions: ADD, AND, OR, XOR, XNOR, SUB, MUL, DIV,
        #                   SHL, SHR, SRA, CMPEQ, CMPLE, CMPLT.
        # (And the corresponding operations ending in 'C'.)
        # ADD operation:
        with m.Elif( ( opcode == OP_ADD[ 0 ] ) ):
          m.d.sync += self.alu.f.eq( 0b010000 )
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.sync += self.alu.a.eq( self.r[ i ] )
            with m.If( rb == i ):
              m.d.sync += self.alu.b.eq( self.r[ i ] )
          m.next = "CPU_ALU_IN"
        # ADDC operation:
        with m.Elif( ( opcode == OP_ADDC[ 0 ] ) ):
          m.d.sync += [
            self.alu.f.eq( 0b010000 ),
            self.alu.b.eq( imm )
          ]
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.sync += self.alu.a.eq( self.r[ i ] )
          m.next = "CPU_ALU_IN"
        # AND operation:
        with m.Elif( ( opcode == OP_AND[ 0 ] ) ):
          m.d.sync += self.alu.f.eq( 0b101000 )
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.sync += self.alu.a.eq( self.r[ i ] )
            with m.If( rb == i ):
              m.d.sync += self.alu.b.eq( self.r[ i ] )
          m.next = "CPU_ALU_IN"
        # ANDC operation:
        with m.Elif( ( opcode == OP_ANDC[ 0 ] ) ):
          m.d.sync += [
            self.alu.f.eq( 0b101000 ),
            self.alu.b.eq( imm )
          ]
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.sync += self.alu.a.eq( self.r[ i ] )
          m.next = "CPU_ALU_IN"
        # CMPEQ operation:
        with m.Elif( ( opcode == OP_CMPEQ[ 0 ] ) ):
          m.d.sync += self.alu.f.eq( 0b000011 )
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.sync += self.alu.a.eq( self.r[ i ] )
            with m.If( rb == i ):
              m.d.sync += self.alu.b.eq( self.r[ i ] )
          m.next = "CPU_ALU_IN"
        # CMPEQC operation:
        with m.Elif( ( opcode == OP_CMPEQC[ 0 ] ) ):
          m.d.sync += [
            self.alu.f.eq( 0b000011 ),
            self.alu.b.eq( imm )
          ]
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.sync += self.alu.a.eq( self.r[ i ] )
          m.next = "CPU_ALU_IN"
        # Go back to incrementing the PC on an unrecognized opcode.
        # TODO: error state?
        with m.Else():
          m.next = "CPU_PC_INCR"
      # "ALU Input": Send a boolean / logical / arithmetic
      #              operation result from the ALU.
      with m.State( "CPU_ALU_IN" ):
        m.d.comb += self.alu.start.eq( 1 )
        with m.If( self.alu.done == 0 ):
          m.next = "CPU_ALU_OUT"
      # "ALU Output": Store a boolean / logical / arithmetic
      #               operation result from the ALU.
      with m.State( "CPU_ALU_OUT" ):
        for i in range( 30 ):
          with m.If( rc == i ):
            m.d.sync += self.r[ i ].eq( self.alu.y )
        m.next = "CPU_PC_INCR"
      # TODO: "Branch or Jump": Perform a branch or jump operation,
      #                   including conditional checks if necessary.
      with m.State( "CPU_JMP" ):
        m.next = "CPU_PC_LOAD"
      # TODO: "Load or Store": Read ROM or read/write RAM data.
      with m.State( "CPU_LS" ):
        m.next = "CPU_PC_INCR"
      # "Increment PC": Increment the program counter by one word.
      with m.State( "CPU_PC_INCR" ):
        m.d.sync += self.pc.eq( self.pc + 4 )
        m.next = "CPU_PC_LOAD"
    
    # End of CPU module definition.
    return m

##################
# CPU testbench: #
##################

# Dummy test method to let the CPU run (TODO: tests)
def cpu_run( cpu, ticks ):
  # Let the CPU run for N ticks.
  for i in range( ticks ):
    yield Tick()

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Create a simulated ROM module with a dummy program.
  rom = ROM( [
    # ADDC, ADD (expect r0 = 0x00001234, r1 = 0x00002468)
    0xC01F1234, 0x80200000,
    # ANDC, AND (expect r2 = r3 = 0x00001200)
    0xE0401200, 0xA0601000,
    # CMPEQC, CMPEQ (expect r4 = 1, 0, 1, 0)
    0xD0801234, 0xD0804321, 0x90821800, 0x90801800,
    # JMP (rc = r28, ra = r29)
    0x6F9D0000,
    # Dummy data (should not be reached).
    0x01234567, 0x89ABCDEF, 0xDEADBEEF, 0xFFFFFFFF, 0xFFFFFFFF
  ] )
  # Instantiate the CPU module.
  dut = CPU( rom )

  # Run the CPU tests.
  with Simulator( dut, vcd_file = open( 'cpu.vcd', 'w' ) ) as sim:
    def proc():
      # Run CPU tests.
      sim_ticks = 250
      yield from cpu_run( dut, sim_ticks )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
