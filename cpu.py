from nmigen import *
from nmigen.back.pysim import *

from alu import *

###############
# CPU module: #
###############

class CPU( Elaboratable ):
  def __init__( self ):
    # Program Counter register.
    self.pc = Signal( 32, reset = 0x00000000 )
    # The main 32 CPU registers.
    self.r  = [ Signal( 32, reset = 0x00000000 ) for _ in range( 32 ) ]
    # The ALU which performs logical operations.
    self.alu = ALU()

  def elaborate( self, platform ):
    # Core CPU module.
    m = Module()
    # The ALU which performs logical operations.
    m.submodules.alu = self.alu

    # r31 is hard-wired to 0.
    m.d.comb += self.r[ 31 ].eq( 0x00000000 )

    # Dummy 'sync' logic to let the simulation step.
    m.d.sync += self.r[ 0 ].eq( self.alu.y )

    # End of CPU module definition.
    return m

##################
# CPU testbench: #
##################

# Dummy test method (TODO: tests)
def cpu_test( cpu ):
  yield Tick()

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate the CPU module.
  dut = CPU()

  # Run the CPU tests.
  with Simulator( dut, vcd_file = open( 'cpu.vcd', 'w' ) ) as sim:
    def proc():
      yield from cpu_test( dut )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
