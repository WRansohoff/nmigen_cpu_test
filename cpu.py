from nmigen import *
from nmigen.back.pysim import *

from alu import *
from isa import *
from rom import *
from ram import *

###############
# CPU module: #
###############

# FSM state definitions. TODO: Remove after figuring out how to
# access the internal FSM from tests. Also, consolidate these steps...
CPU_PC_LOAD      = 0
CPU_PC_ROM_FETCH = 1
CPU_PC_DECODE    = 2
CPU_ALU_IN       = 4
CPU_ALU_OUT      = 5
CPU_LD           = 6
CPU_ST           = 7
CPU_PC_INCR      = 8
CPU_STATES_MAX   = 8

# CPU module.
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

    # Debugging signal(s):
    # Track FSM state. TODO: There must be a way to access this
    # from the Module's FSM object, but I don't know how.
    self.fsms = Signal( range( CPU_STATES_MAX ), reset = CPU_PC_LOAD )

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
        m.d.comb += self.fsms.eq( CPU_PC_LOAD ) #TODO: Remove
        m.next = "CPU_PC_ROM_FETCH"
      # "ROM Fetch": Wait for the instruction to load from ROM, and
      #              populate register fields to prepare for decoding.
      with m.State( "CPU_PC_ROM_FETCH" ):
        m.d.comb += self.fsms.eq( CPU_PC_ROM_FETCH ) #TODO: Remove
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
        m.d.comb += self.fsms.eq( CPU_PC_DECODE ) #TODO: Remove
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
        m.d.comb += self.fsms.eq( CPU_ALU_IN ) #TODO: Remove
        m.d.comb += self.alu.start.eq( 1 )
        with m.If( self.alu.done == 0 ):
          m.next = "CPU_ALU_OUT"
      # "ALU Output": Store a boolean / logical / arithmetic
      #               operation result from the ALU.
      with m.State( "CPU_ALU_OUT" ):
        m.d.comb += self.fsms.eq( CPU_ALU_OUT ) #TODO: Remove
        for i in range( 30 ):
          with m.If( rc == i ):
            m.d.sync += self.r[ i ].eq( self.alu.y )
        m.next = "CPU_PC_INCR"
      # "Load state": Read ROM or RAM data.
      with m.State( "CPU_LD" ):
        m.d.comb += self.fsms.eq( CPU_LD ) #TODO: Remove
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
        m.d.comb += self.fsms.eq( CPU_ST ) #TODO: Remove
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
        m.d.comb += self.fsms.eq( CPU_PC_INCR ) #TODO: Remove
        m.d.sync += self.pc.eq( self.pc + 4 )
        m.next = "CPU_PC_LOAD"
    
    # End of CPU module definition.
    return m

##################
# CPU testbench: #
##################

# Import test programs and expected runtime register values.
from programs import *

# Helper method to run a CPU device for a given number of cycles,
# and verify its expected register values over time.
def cpu_run( cpu, expected, ticks ):
  # Record how many CPU instructions have executed.
  ni = -1
  # Let the CPU run for N ticks.
  for i in range( ticks ):
    # Let combinational logic settle before checking values.
    yield Settle()
    # Only check expected values once per instruction.
    fsm_state = yield cpu.fsms
    if fsm_state == CPU_PC_ROM_FETCH:
      ni += 1
      # Check 'expected' values, if any.
      if ni in expected:
        for j in range( len( expected[ ni ] ) ):
          ex = expected[ ni ][ j ]
          # Special case: program counter.
          if ex[ 'r' ] == 'pc':
            cpc = yield cpu.pc
            if cpc == ex[ 'v' ]:
              print( "  \033[32mPASS:\033[0m pc  == %s"
                     " after %d operations"
                     %( hexs( ex[ 'v' ] ), ni ) )
            else:
              print( "  \033[31mFAIL:\033[0m pc  == %s"
                     " after %d operations (got: %s)"
                     %( hexs( ex[ 'v' ] ), ni, hexs( cpc ) ) )
          # Numbered general-purpose registers.
          elif ex[ 'r' ] > 0 and ex[ 'r' ] < 32:
            cr = yield cpu.r[ ex[ 'r' ] ]
            if cr == ex[ 'v' ]:
              print( "  \033[32mPASS:\033[0m r%02d == %s"
                     " after %d operations"
                     %( ex[ 'r' ], hexs( ex[ 'v' ] ), ni ) )
            else:
              print( "  \033[31mFAIL:\033[0m r%02d == %s"
                     " after %d operations (got: %s)"
                     %( ex[ 'r' ], hexs( ex[ 'v' ] ), ni, hexs( cr ) ) )
    yield Tick()

# Helper method to simulate running a CPU with the given ROM image
# for the specified number of CPU cycles. The 'name' field is used
# for printing and generating the waveform filename: "cpu_[name].vcd".
# The 'expected' dictionary contains a series of expected register
# values at specific points in time, defined by elapsed instructions.
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
  # Simulate the 'infinite loop test' ROM.
  cpu_sim( 'loop', loop_test, loop_exp, 10 )
  # Simulate the 'quick test' ROM.
  cpu_sim( 'quick', quick_test, quick_exp, 500 )
