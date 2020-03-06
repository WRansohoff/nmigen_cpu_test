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
    self.rom = ROM( [ 0x00000000 ] )
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

    # r31 is hard-wired to 0.
    m.d.comb += self.r[ 31 ].eq( 0x00000000 )

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
  yield Tick()
  yield Tick()
  yield Tick()
  yield Tick()
  yield Tick()
  return p, f

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate the CPU module.
  dut = CPU()

  # Run the CPU tests.
  with Simulator( dut, vcd_file = open( 'cpu.vcd', 'w' ) ) as sim:
    def proc():
      print( "--- ALU Tests ---" )
      p, f = yield from alu_test( dut.alu )
      print( "ALU Tests: %d Passed, %d Failed"%( p, f ) )
      print( "--- ROM Tests ---" )
      p, f = yield from rom_test( dut.rom )
      print( "ROM Tests: %d Passed, %d Failed"%( p, f ) )
      print( "--- RAM Tests ---" )
      p, f = yield from ram_test( dut.ram )
      print( "RAM Tests: %d Passed, %d Failed"%( p, f ) )
      print( "--- CPU Tests ---" )
      p, f = yield from cpu_test( dut )
      print( "CPU Tests: %d Passed, %d Failed"%( p, f ) )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
