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
    self.rom = rom_module
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
    imm = Signal( shape = Shape( width = 32, signed = True ),
                  reset = 0x00000000 )

    # r31 is hard-wired to 0.
    self.r[ 31 ].eq( 0x00000000 )
    # r30 is the exception pointer.
    self.r[ 30 ].eq( self.xp )
    # Set the program counter to the simulated ROM address by default.
    m.d.comb += self.rom.addr.eq( self.pc )
    # Set the simulated RAM address to 0 by default, and set
    # the RAM's read/write enable bits to 0 by default.
    m.d.comb += [
      self.ram.addr.eq( 0 ),
      self.ram.ren.eq( 0 ),
      self.ram.wen.eq( 0 )
    ]
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
          m.d.sync += imm.eq( self.rom.out | 0xFFFF0000 )
        with m.Else():
          m.d.sync += imm.eq( self.rom.out & 0x0000FFFF )
        m.d.sync += [
          opcode.eq( self.rom.out.bit_select( 26, 6 ) ),
          rc.eq( self.rom.out.bit_select( 21, 5 ) ),
          ra.eq( self.rom.out.bit_select( 16, 5 ) ),
          rb.eq( self.rom.out.bit_select( 11, 5 ) )
        ]
        m.next = "CPU_PC_DECODE"
      # "Decode PC": Figure out what sort of instruction to execute,
      #              and prepare associated registers.
      with m.State( "CPU_PC_DECODE" ):
        # Load/Store ops: LD, LDR, ST. For now, ROM is mapped to both
        # 0x00... and 0x08..., and RAM is mapped to 0x20...
        # Other prefixes will return 0 for loads. Stores can only
        # write to RAM; they will do nothing for other prefixes.
        # TODO: Memory space for peripherals.
        # LD "LoaD" operation: Rc = Memory[ Ra + immediate ]
        with m.If( opcode == OP_LD[ 0 ] ):
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.comb += self.mp.eq( self.r[ i ] + imm )
              with m.If( self.r[ i ] & 0x20000000 ):
                m.d.comb += [
                  self.ram.addr.eq( self.mp & 0x1FFFFFFF ),
                  self.ram.ren.eq( 0b1 )
                ]
              with m.Else():
                m.d.comb += self.rom.addr.eq( self.mp & 0x07FFFFFF )
          m.next = "CPU_LD"
        # LDR "LoaD Relative" operation: Rc = Memory[ PC + immediate ]
        with m.Elif( opcode == OP_LDR[ 0 ] ):
          m.d.comb += self.mp.eq( self.pc + ( imm * 4 ) + 4 )
          with m.If( self.mp & 0x20000000 ):
            m.d.comb += [
              self.ram.addr.eq( self.mp & 0x1FFFFFFF ),
              self.ram.ren.eq( 0b1 )
            ]
          with m.Elif( ( self.mp & 0x08000000 ) |
                       ( self.mp & 0x07FFFFFF ) ):
            m.d.comb += self.rom.addr.eq( self.mp & 0x07FFFFFF )
          m.next = "CPU_LD"
        # ST "STore" operation: Memory[ Ra + immediate ] = Rc
        with m.Elif( opcode == OP_ST[ 0 ] ):
          for i in range( 32 ):
            with m.If( ra == i ):
              m.d.comb += self.mp.eq( self.r[ i ] + imm )
              with m.If( self.r[ i ] & 0x20000000 ):
                m.d.comb += [
                  self.ram.addr.eq( self.mp & 0x1FFFFFFFF ),
                  self.ram.wen.eq( 0b1 )
                ]
            with m.If( rc == i ):
              m.d.sync += self.ram.din.eq( self.r[ i ] )
          m.next = "CPU_ST"
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
      # "Load state": Read ROM or RAM data.
      with m.State( "CPU_LD" ):
        with m.If( opcode == OP_LDR[ 0 ] ):
          m.d.comb += self.mp.eq( self.pc + ( imm * 4 ) + 4 )
        for i in range( 32 ):
          with m.If( ( ra == i ) & ( opcode == OP_LD[ 0 ] ) ):
            m.d.comb += self.mp.eq( self.r[ i ] + imm )
          with m.If( rc == i ):
            with m.If( self.mp & 0x20000000 ):
              m.d.comb += [
                self.ram.addr.eq( self.mp & 0x1FFFFFFF ),
                self.ram.ren.eq( 0b1 )
              ]
              m.d.sync += self.r[ i ].eq( self.ram.dout )
            with m.Elif( ( self.mp & 0x08000000 ) |
                         ( self.mp & 0x07FFFFFF ) ):
              m.d.comb += self.rom.addr.eq( self.mp & 0x07FFFFFF )
              m.d.sync += self.r[ i ].eq( self.rom.out )
            with m.Else():
              m.d.sync += self.r[ i ].eq( 0x00000000 )
        m.next = "CPU_PC_INCR"
      # Store state": Write RAM data.
      with m.State( "CPU_ST" ):
        for i in range( 32 ):
          with m.If( ra == i ):
            m.d.comb += self.mp.eq( self.r[ i ] + imm )
          with m.If( rc == i ):
            with m.If( self.mp & 0x20000000 ):
              m.d.comb += [
                self.ram.addr.eq( self.mp & 0x1FFFFFFF ),
                self.ram.wen.eq( 0b1 )
              ]
        m.next = "CPU_PC_INCR"
      # "Store state": Write RAM data.
      # "Increment PC": Increment the program counter by one word.
      with m.State( "CPU_PC_INCR" ):
        m.d.sync += self.pc.eq( self.pc + 4 )
        m.next = "CPU_PC_LOAD"
    
    # End of CPU module definition.
    return m

##################
# CPU testbench: #
##################

# Helper method to run a CPU device for a given number of cycles,
# and verify its expected register values over time.
def cpu_run( cpu, expected, ticks ):
  # Let the CPU run for N ticks.
  for i in range( ticks ):
    # Check 'expected' values, if any.
    if i in expected:
      for j in range( len( expected[ i ] ) ):
        ex = expected[ i ][ j ]
        # Special case: program counter.
        if ex[ 'r' ] == 'pc':
          cpc = yield cpu.pc
          if cpc == ex[ 'v' ]:
            print( "  \033[32mPASS:\033[0m pc  == %s @ t = %d ticks"
                   %( hexs( ex[ 'v' ] ), i ) )
          else:
            print( "  \033[31mFAIL:\033[0m pc  == %s @ t = %d ticks"
                   " (got: %s)"
                   %( hexs( ex[ 'v' ] ), i, hexs( cpc ) ) )
        # Numbered general-purpose registers.
        elif ex[ 'r' ] > 0 and ex[ 'r' ] < 32:
          cr = yield cpu.r[ ex[ 'r' ] ]
          if cr == ex[ 'v' ]:
            print( "  \033[32mPASS:\033[0m r%02d == %s @ t = %d ticks"
                   %( ex[ 'r' ], hexs( ex[ 'v' ] ), i ) )
          else:
            print( "  \033[31mFAIL:\033[0m r%02d == %s @ t = %d ticks"
                   " (got: %s)"
                   %( ex[ 'r' ], hexs( ex[ 'v' ] ), i, hexs( cr ) ) )
    yield Tick()

# Helper method to simulate running a CPU with the given ROM image
# for the specified number of CPU cycles. The 'name' field is used
# for printing and generating the waveform filename: "cpu_[name].vcd".
# The 'expected' dictionary contains a series of expected register
# values at specific points in time, defined by CPU cycles.
def cpu_sim( name, rom, expected, ticks ):
  print( "CPU '%s' test program:"%name )
  # Create the CPU device.
  cpu = CPU( rom )

  # Run the simulation.
  sim_name = 'cpu_%s.vcd'%name
  with Simulator( cpu, vcd_file = open( sim_name, 'w' ) ) as sim:
    def proc():
      # Run the program and print pass/fail for individual tests.
      yield from cpu_run( cpu, expected, ticks )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Create a "while(1){};" ROM program.
  loop_rom = ROM( [ JMP( 29, 31 ) ] )
  # The 'infinite loop' program is just one 'jump' instruction.
  # We can expect the PC to always equal 0 and r29 to hold 0x04.
  loop_exp = {
    0: [ { 'r': 'pc', 'v': 0x00000000 } ],
    4: [
         { 'r': 'pc', 'v': 0x00000000 },
         { 'r': 29, 'v': 0x00000004 }
       ],
    8: [
         { 'r': 'pc', 'v': 0x00000000 },
         { 'r': 29, 'v': 0x00000004 }
       ]
  }
  # Create a 'quick test' ROM program which contains at least one of
  # each supported machine code instruction, but does not perform
  # exhaustive tests for any particular instruction.
  test_rom = ROM( [
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

  # Simulate the 'infinite loop test' ROM.
  cpu_sim( 'loop', loop_rom, loop_exp, 10 )
  # Simulate the 'quick test' ROM.
  cpu_sim( 'test', test_rom, {}, 500 )
