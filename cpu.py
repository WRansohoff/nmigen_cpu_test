from nmigen import *
from nmigen.back.pysim import *
from alu import *
from rom import *
from ram import *

###############
# CPU module: #
###############

class CPU( Elaboratable ):
  def __init__( self ):
    # Program Counter register.
    self.pc = Signal( 32, reset = 0x00000000 )
    # The main 32 CPU registers.
    self.r  = [
      Signal( 32, reset = 0x00000000, name = "r%d"%i )
      for i in range( 32 )
    ]
    # The ALU submodule which performs logical operations.
    self.alu = ALU()
    # The ROM submodule which acts as simulated program data storage.
    self.rom = ROM( [ 0x01234567, 0x89ABCDEF, 0x00000000, 0xFFFFFFFF ] )
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
    instr = Signal( 32, reset = 0x00000000 )

    # r31 is hard-wired to 0.
    m.d.comb += self.r[ 31 ].eq( 0x00000000 )
    # Hard-wire the program counter to the simulated ROM address.
    m.d.comb += self.rom.addr.eq( self.pc )

    # Main CPU FSM.
    with m.FSM() as fsm:
      # "Load PC": Fetch the memory location in the program counter
      #            from ROM, to prepare for decoding.
      with m.State( "CPU_PC_LOAD" ):
        m.d.sync += instr.eq( self.rom.out )
        m.next = "CPU_PC_DECODE"
      # TODO: "Decode PC": Figure out what sort of instruction to execute,
      #              and prepare associated registers.
      with m.State( "CPU_PC_DECODE" ):
        m.next = "CPU_PC_INCR"
      # TODO: "ALU Load": Send a boolean / logical / arithmetic
      #                 operation to the ALU.
      with m.State( "CPU_ALU_LOAD" ):
        m.next = "CPU_ALU_STORE"
      # TODO: "ALU Store": Store an ALU result in the requested register.
      with m.State( "CPU_ALU_STORE" ):
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
    

    # Dummy synchronous logic step so the test simulation works.
    # This performs a preset ALU operation, then loads the result
    # into R0 followed by R1. If you look at the waveform, it takes
    # time for the result to propagate from ALU -> R0 -> R1.
    m.d.sync += [
      self.alu.a.eq( 0xFFFFFFFF ),
      self.alu.b.eq( 0x1234ABCD ),
      self.alu.f.eq( 0b101000 ),
      self.alu.start.eq( 1 ),
      self.r[ 0 ].eq( self.alu.y ),
      self.r[ 1 ].eq( self.r[ 0 ] )
    ]

    # End of CPU module definition.
    return m

##################
# CPU testbench: #
##################
# Keep track of test pass / fail rates.
p = 0
f = 0

# Dummy test method (TODO: tests)
def cpu_test( cpu ):
  global p, f

  # Print a test header.
  print( "--- CPU Tests ---" )

  yield Tick()
  yield Tick()
  yield Tick()
  yield Tick()

  # Done.
  yield Tick()
  print( "CPU Tests: %d Passed, %d Failed"%( p, f ) )

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate the CPU module.
  dut = CPU()

  # Run the CPU tests.
  with Simulator( dut, vcd_file = open( 'cpu.vcd', 'w' ) ) as sim:
    def proc():
      # Run submodule tests.
      yield from alu_test( dut.alu )
      yield from rom_test( dut.rom )
      yield from ram_test( dut.ram )
      # Run CPU tests.
      yield from cpu_test( dut )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
