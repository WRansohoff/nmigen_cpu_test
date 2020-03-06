from nmigen import *
from nmigen.back.pysim import *

###############
# ROM module: #
###############

class ROM( Elaboratable ):
  def __init__( self, data ):
    # Address bits to select up to `len( data )` words by byte.
    self.addr = Signal( range( len( data * 4 ) ), reset = 0 )
    # Data word output.
    self.out  = Signal( 32, reset = 0x00000000 )
    # Data storage.
    self.data = [
      Signal( 32, reset = data[ i ], name = "rom(0x%08X)"%( i * 4 ) )
      for i in range( len( data ) )
    ]
    # Record size.
    self.size = len( data )

  def elaborate( self, platform ):
    # Core ROM module.
    m = Module()

    # Set the 'output' value to 0 if the address is not word-aligned.
    with m.If( ( self.addr & 0b11 ) != 0 ):
      m.d.sync += self.out.eq( 0 )
    # Set the 'output' value to the requested 'data' array index.
    for i in range( self.size ):
      with m.Elif( self.addr == ( i * 4 ) ):
        m.d.sync += self.out.eq( self.data[ i ] )

    # End of ROM module definition.
    return m

##################
# ROM testbench: #
##################

def rom_test( rom ):
  yield Tick()
  yield Tick()
  yield Tick()

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate a test ROM module with 16 bytes of data.
  dut = ROM( [ 0x01234567, 0x89ABCDEF, 0x42424242, 0xDEADBEEF ] )

  # Run the ROM tests.
  with Simulator( dut, vcd_file = open( 'rom.vcd', 'w' ) ) as sim:
    def proc():
      yield from rom_test( dut )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
