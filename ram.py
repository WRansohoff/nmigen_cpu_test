from nmigen import *
from nmigen.back.pysim import *

###############
# RAM module: #
###############

class RAM( Elaboratable ):
  def __init__( self, size_words ):
    # Record size.
    self.words = size_words
    # Address bits to select up to `size_words * 4` bytes.
    self.addr = Signal( range( self.words * 4 ), reset = 0 )
    # Data word output.
    self.dout = Signal( 32, reset = 0x00000000 )
    # 'Read Enable' input bit.
    self.ren  = Signal( 1, reset = 0b1 )
    # Data word input.
    self.din  = Signal( 32, reset = 0x00000000 )
    # 'Write Enable' input bit.
    self.wen  = Signal( 1, reset = 0b0 )
    # Data storage.
    self.data = [
      Signal( 32, reset = 0x00000000, name = "ram(0x%08X)"%( i * 4 ) )
      for i in range( self.words )
    ]

  def elaborate( self, platform ):
    # Core RAM module.
    m = Module()

    # Set the 'dout' value if 'ren' is set.
    with m.If( self.ren ):
      # (Return 0 if the address is not word-aligned.)
      with m.If( ( self.addr & 0b11 ) != 0 ):
        m.d.sync += self.dout.eq( 0x00000000 )
      for i in range( self.words ):
        with m.Elif( self.addr == ( i * 4 ) ):
          m.d.sync += self.dout.eq( self.data[ i ] )

    # Write the 'din' value if 'wen' is set.
    with m.If( self.wen ):
      # (nop if the write address is not word-aligned.)
      with m.If( ( self.addr & 0b11 ) != 0 ):
        m.d.sync = m.d.sync
      for i in range( self.words ):
        with m.Elif( self.addr == ( i * 4 ) ):
          m.d.sync += self.data[ i ].eq( self.din )

    # End of RAM module definition.
    return m

##################
# RAM testbench: #
##################

def ram_test( ram ):
  yield Tick()
  yield Tick()
  yield Tick()

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate a test RAM module with 32 bytes of data.
  dut = RAM( 8 )

  # Run the RAM tests.
  with Simulator( dut, vcd_file = open( 'ram.vcd', 'w' ) ) as sim:
    def proc():
      yield from ram_test( dut )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
