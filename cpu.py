from nmigen import *
from nmigen.back.pysim import *

from alu import *
from isa import *
from rom import *
from ram import *

###############
# CPU module: #
###############

class CPU( Elaboratable ):
  def __init__( self, rom_module ):
    # Program Counter register.
    self.pc = Signal( 32, reset = 0x00000000 )
    # Exception Pointer register.
    self.xp = Signal( 32, reset = 0x00000000 )
    # Intermediate load/store memory pointer.
    self.mp = Signal( 32, reset = 0x00000000 )
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

  # Helper method to define shared logic for 'Rc = Ra ? Rb' ALU
  # operations such as 'ADD', 'AND', 'CMPEQ', etc.
  def alu_reg_op( self, cpu, ra, rb, op ):
    # TODO: Signals are not hashable, so 'ALU_OPS[ op ]' doesn't work
    # I can come up with a better way to handle these signals...
    cpu.d.sync += self.alu.f.eq( op )
    for i in range( 32 ):
      with cpu.If( ra == i ):
        cpu.d.sync += self.alu.a.eq( self.r[ i ] )
      with cpu.If( rb == i ):
        cpu.d.sync += self.alu.b.eq( self.r[ i ] )
    cpu.next = "CPU_ALU_IN"

  # Helper method to define shared logic for 'Rc = Ra ? Immediate'
  # ALU operations such as 'ADDC', 'ANDC', 'CMPEQC', etc.
  def alu_imm_op( self, cpu, ra, imm, op ):
    # TODO: Signals are not hashable, so 'ALU_OPS[ op ]' doesn't work
    # I can come up with a better way to handle these signals...
    cpu.d.sync += [
      self.alu.f.eq( op & 0b101111 ),
      self.alu.b.eq( imm )
    ]
    for i in range( 32 ):
      with cpu.If( ra == i ):
        cpu.d.sync += self.alu.a.eq( self.r[ i ] )
    cpu.next = "CPU_ALU_IN"

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
    # TODO: Signed immediate hints to simplify these signals?
    imm = Signal( 32, reset = 0x00000000 )
    imm_signex = Signal( 32, reset = 0x00000000 )

    # r31 is hard-wired to 0.
    self.r[ 31 ].eq( 0x00000000 )
    # r30 is the exception pointer.
    self.r[ 30 ].eq( self.xp )
    # Set the program counter to the simulated ROM address by default.
    m.d.comb += self.rom.addr.eq( self.pc )
    # Set the simulated RAM address to 0 by default.
    m.d.comb += self.ram.addr.eq( 0 )
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
        # Load/Store ops: LD, LDR, ST. For now, ROM is mapped to both
        # 0x00... and 0x08..., and RAM is mapped to 0x20...
        # Other prefixes will return 0 (loads) or do nothing (stores)
        # LD "LoaD" operation: Rc = Memory[ Ra + immediate ]
        with m.If( opcode == OP_LD[ 0 ] ):
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.comb += self.mp.eq( self.r[ i ] + imm )
              with m.If( self.r[ i ] & 0x20000000 ):
                m.d.comb += self.ram.addr.eq( self.mp & 0x1FFFFFFF ),
                m.d.sync += self.ram.ren.eq( 0b1 )
              with m.Else():
                m.d.comb += self.rom.addr.eq( self.mp & 0x07FFFFFF )
          m.next = "CPU_LD"
        with m.Elif( opcode == OP_LDR[ 0 ] ):
          m.next = "CPU_PC_INCR"
        with m.Elif( opcode == OP_ST[ 0 ] ):
          m.next = "CPU_PC_INCR"
        # Branch/Jump ops: JMP, BEQ, BNE.
        # JMP "JuMP" operation: Place the next PC value in Rc, then
        # set PC to (Ra & 0xFFFFFFFC) to ensure it is word-aligned.
        with m.Elif( opcode == OP_JMP[ 0 ] ):
          for i in range( 32 ):
            with m.If( rc == i ):
              m.d.sync += self.r[ i ].eq( self.pc + 4 )
            with m.If( ra == i ):
              m.d.sync += self.pc.eq( self.r[ i ] & 0xFFFFFFFC )
          m.next = "CPU_PC_LOAD"
        # BEQ "Branch if EQual" operation: Place the next PC value in
        # Rc, then jump to (next PC + immediate) if Ra == 0.
        with m.Elif( opcode == OP_BEQ[ 0 ] ):
          for i in range( 32 ):
            with m.If( rc == i ):
              m.d.sync += self.r[ i ].eq( self.pc + 4 )
            with m.If( ra == i ):
              with m.If( self.r[ i ] ):
                m.next = "CPU_PC_INCR"
              with m.Else():
                m.d.sync += self.pc.eq( self.pc + ( imm * 4 ) + 4 )
                m.next = "CPU_PC_LOAD"
        # BNE "Branch if Not Equal" operation: Same as BEQ, but
        # perform the jump if Ra != 0 instead.
        with m.Elif( opcode == OP_BNE[ 0 ] ):
          for i in range( 32 ):
            with m.If( rc == i ):
              m.d.sync += self.r[ i ].eq( self.pc + 4 )
            with m.If( ra == i ):
              with m.If( self.r[ i ] ):
                m.d.sync += self.pc.eq( self.pc + ( imm * 4 ) + 4 )
                m.next = "CPU_PC_LOAD"
              with m.Else():
                m.next = "CPU_PC_INCR"
        # ALU instructions: ADD, AND, OR, XOR, XNOR, SUB, MUL, DIV,
        #                   SHL, SHR, SRA, CMPEQ, CMPLE, CMPLT.
        # (And the corresponding operations ending in 'C'.)
        # 'RC = Ra ? Rb' ALU operations:
        with m.Elif( opcode.bit_select( 4, 2 ) == 0b10 ):
          self.alu_reg_op( m, ra, rb, opcode )
        # 'RC = Ra ? Constant' ALU operations:
        with m.Elif( opcode.bit_select( 4, 2 ) == 0b11 ):
          self.alu_imm_op( m, ra, imm, opcode )
        # Move on to incrementing the PC for unrecognized operations.
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
      # TODO: "Load or Store": Read ROM or read/write RAM data.
      with m.State( "CPU_LD" ):
        for i in range( 32 ):
          with m.If( ra == i ):
            m.d.comb += self.mp.eq( self.r[ i ] + imm )
          with m.If( rc == i ):
            with m.If( self.mp & 0x20000000 ):
              m.d.comb += self.ram.addr.eq( self.mp & 0x1FFFFFFF ),
              m.d.sync += [
                self.ram.ren.eq( 0b1 ),
                self.r[ i ].eq( self.ram.dout )
              ]
            with m.Elif( ( self.mp & 0x08000000 ) |
                         ( self.mp & 0x07FFFFFF ) ):
              m.d.comb += self.rom.addr.eq( self.mp & 0x07FFFFFF )
              m.d.sync += self.r[ i ].eq( self.rom.out )
            with m.Else():
              m.d.sync += self.r[ i ].eq( 0x00000000 )
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
    ADDC( 0, 31, 0x1234 ), ADD( 1, 0, 0 ),
    # BNE (expect r27 = 0x0C, PC skips over the following dummy data)
    BNE( 27, 0, 0x0004 ),
    0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF, 0xDEADBEEF,
    # BEQ (expect r26 = 0x20, PC skips over the following dummy data)
    BEQ( 26, 22, 0x0002 ), 0xDEADBEEF, 0xDEADBEEF,
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
    # JMP (rc = r28, ra = r29, PC returns to 0x00000000)
    JMP( 28, 29 ),
    # Dummy data (should not be reached).
    0x01234567, 0x89ABCDEF, 0xDEADBEEF, 0xFFFFFFFF, 0xFFFFFFFF
  ] )
  # Instantiate the CPU module.
  dut = CPU( rom )

  # Run the CPU tests.
  with Simulator( dut, vcd_file = open( 'cpu.vcd', 'w' ) ) as sim:
    def proc():
      # Run CPU tests.
      sim_ticks = 350
      yield from cpu_run( dut, sim_ticks )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
