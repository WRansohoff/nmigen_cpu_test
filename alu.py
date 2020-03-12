from nmigen import *
from nmigen.back.pysim import *

from isa import *

###############
# ALU module: #
###############

class ALU( Elaboratable ):
  def __init__( self ):
    # 'A' and 'B' data inputs.
    self.a = Signal( shape = Shape( width = 32, signed = True ),
                     reset = 0x00000000 )
    self.b = Signal( shape = Shape( width = 32, signed = True ),
                     reset = 0x00000000 )
    # 'F' function select input.
    self.f = Signal( 6,  reset = 0b000000 )
    # 'Y' data output.
    self.y = Signal( shape = Shape( width = 32, signed = True ),
                     reset = 0x00000000 )
    # 'N' arithmetic flag (last result was negative)
    self.n = Signal()
    # 'Z' arithmetic flag (last result == zero)
    self.z = Signal()
    # 'V' arithmetic flag (last result overflowed)
    self.v = Signal()
    # 'Start' signal to latch inputs.
    self.start = Signal( reset = 0b0 )

  def elaborate( self, platform ):
    # Core ALU module.
    m = Module()
    # Define both positive- and negative-edge clock domains.
    # TODO: If I try to use 'sync' for both domains, I get an error
    # which says that the domain names and dictionary keys must match.
    # But it would be nice to use both edges of one 'sync' domain...
    clock = ClockDomain( "sync", clk_edge = "pos" )
    nclock = ClockDomain( "nsync", clk_edge = "neg" )
    m.domains += clock
    m.domains += nclock

    # Latched input values for signed and unsigned operations.
    xa   = Signal( shape = Shape( width = 32, signed = True ) )
    xb   = Signal( shape = Shape( width = 32, signed = True ) )
    ua   = Signal( shape = Shape( width = 32, signed = False ) )
    ub   = Signal( shape = Shape( width = 32, signed = False ) )
    fn   = Signal( 6 )
    # Helper registers for signed division.
    diva = Signal( shape = Shape( width = 32, signed = False ) )
    divb = Signal( shape = Shape( width = 32, signed = False ) )
    divs = Signal()

    # Latch input values at falling clock edges if 'start' is set.
    with m.If( self.start ):
      m.d.nsync += [
        xa.eq( self.a ),
        xb.eq( self.b ),
        ua.eq( self.a ),
        ub.eq( self.b ),
        fn.eq( self.f ),
      ]
      with m.If( ( self.b < 0 ) & ( self.a < 0 ) ):
        m.d.nsync += [
          diva.eq( -self.a ),
          divb.eq( -self.b ),
          divs.eq( 0 )
        ]
      with m.Elif( self.a < 0 ):
        m.d.nsync += [
          diva.eq( -self.a ),
          divb.eq( self.b ),
          divs.eq( 1 )
        ]
      with m.Elif( self.b < 0 ):
        m.d.nsync += [
          diva.eq( self.a ),
          divb.eq( -self.b ),
          divs.eq( 1 )
        ]
      with m.Else():
        m.d.nsync += [
          diva.eq( self.a ),
          divb.eq( self.b ),
          divs.eq( 0 )
        ]

    # Combinatorial N, Z, and V arithmetic flags.
    # 'N' flag is always equal to the most significant result bit.
    m.d.comb += self.n.eq( self.y.bit_select( 31, 1 ) )
    # 'Z' flag is true if the result is zero.
    m.d.comb += self.z.eq( self.y == 0 )
    # 'V' flag is true if an overflow occurred.
    with m.If( fn == C_ADD[ 0 ] ):
      # With addition, overflow occurred if the two input signs are
      # the same and those signs differ from the result's sign.
      m.d.comb += self.v.eq( ( xa[ 31 ] == xb[ 31 ] ) &
                             ( xa[ 31 ] != self.y[ 31 ] ) )
    with m.Elif( fn == C_SUB[ 0 ] ):
      # With subtraction, overflow occurred if A and -B have the same
      # sign, and that sign differs from the result's sign.
      m.d.comb += self.v.eq( ( xa[ 31 ] != xb[ 31 ] ) &
                             ( xa[ 31 ] != self.y[ 31 ] ) )
    with m.Else():
      # For non-arithmetic operations, set 'V' to 0.
      # TODO: Should multiplication also set the overflow flag?
      m.d.comb += self.v.eq( 0 )

    # Perform ALU computations based on the 'function' bits.
    # Boolean unit (F = [...]):
    #  - 0b101000: Y = A AND B
    with m.If( fn == C_AND[ 0 ] ):
      m.d.comb += self.y.eq( xa & xb )
    #  - 0b101001: Y = A  OR B
    with m.Elif( fn == C_OR[ 0 ] ):
      m.d.comb += self.y.eq( xa | xb )
    #  - 0b101010: Y = A XOR B
    with m.Elif( fn == C_XOR[ 0 ] ):
      m.d.comb += self.y.eq( xa ^ xb )
    #  - 0b101011: Y = A XNOR B = NOT( A XOR B )
    with m.Elif( fn == C_XNOR[ 0 ] ):
      m.d.comb += self.y.eq( ~( xa ^ xb ) )
    # Arithmetic unit (F = [...]):
    #  - 0b100000: Y = A + B
    with m.Elif( fn == C_ADD[ 0 ] ):
      m.d.comb += self.y.eq( xa + xb )
    #  - 0b100001: Y = A - B
    with m.Elif( fn == C_SUB[ 0 ] ):
      m.d.comb += self.y.eq( xa - xb )
    #  - 0b100011: Y = A / B (wow, this is way easier than VHDL!)
    with m.Elif( fn == C_DIV[ 0 ] ):
      # (Return 0 if division by 0 occurs)
      with m.If( xb == 0 ):
        m.d.comb += self.y.eq( 0 )
      with m.Elif( divs ):
        m.d.comb += self.y.eq( ( diva // divb ) * -1 )
      with m.Else():
        m.d.comb += self.y.eq( diva // divb )
    #  - 0b100010: Y = A * B
    with m.Elif( fn == C_MUL[ 0 ] ):
      m.d.comb += self.y.eq( xa * xb )
    # Comparison unit (F = [...]):
    #  - 0b100100: Y = ( A == B )
    with m.Elif( fn == C_CEQ[ 0 ] ):
      m.d.comb += self.y.eq( xa == xb )
    #  - 0b100101: Y = ( A <  B )
    with m.Elif( fn == C_CLT[ 0 ] ):
      m.d.comb += self.y.eq( xa < xb )
    #  - 0b100110: Y = ( A <= B )
    with m.Elif( fn == C_CLE[ 0 ] ):
      m.d.comb += self.y.eq( xa <= xb )
    # Shift unit (F = [...]):
    #  - 0b101100: Y = A << B
    with m.Elif( fn == C_SHL[ 0 ] ):
      m.d.comb += self.y.eq( xa << ub )
    #  - 0b101101: Y = A >> B (no sign extend)
    with m.Elif( fn == C_SHR[ 0 ] ):
      m.d.comb += self.y.eq( ua >> ub )
    #  - 0b101110: Y = A >> B (with sign extend)
    with m.Elif( fn == C_SRA[ 0 ] ):
      m.d.comb += self.y.eq( xa >> ub )
    # Return 0 after one clock cycle for unrecognized commands.
    with m.Else():
      m.d.comb += self.y.eq( 0x00000000 )

    # End of ALU module definition.
    return m

##################
# ALU testbench: #
##################
# Keep track of test pass / fail rates.
p = 0
f = 0

# Perform an individual ALU unit test.
def alu_ut( alu, a, b, fn, expected ):
  global p, f
  # Set A, B, F.
  yield alu.a.eq( a )
  yield alu.b.eq( b )
  yield alu.f.eq( fn[ 0 ] )
  # Pulse 'start' with one intervening clock tick.
  yield alu.start.eq( 1 )
  yield Tick()
  yield alu.start.eq( 0 )
  # Done. Check the result after combinatorial logic settles.
  yield Settle()
  actual = yield alu.y
  if hexs( expected ) != hexs( actual ):
    f += 1
    print( "\033[31mFAIL:\033[0m %s %s %s = %s (got: %s)"
           %( hexs( a ), fn[ 1 ], hexs( b ),
              hexs( expected ), hexs( actual ) ) )
  else:
    p += 1
    print( "\033[32mPASS:\033[0m %s %s %s = %s"
           %( hexs( a ), fn[ 1 ], hexs( b ), hexs( expected ) ) )

# Helper method to verify that 'N', 'Z', 'V' flags are set correctly.
def check_nzv( alu, n, z, v ):
  global p, f
  an = yield alu.n
  az = yield alu.z
  av = yield alu.v
  if ( an == n ) and ( az == z ) and ( av == v ):
    p += 1
    print( "\033[32m  PASS:\033[0m N, Z, V flags: %d, %d, %d"
           %( n, z, v ) )
  else:
    f += 1
    print( "\033[31m  FAIL:\033[0m N, Z, V flags: " \
           "%d, %d, %d (got: %d, %d, %d)"
           %( n, z, v, an, az, av ) )

# Top-level ALU test method.
def alu_test( alu ):
  # Let signals settle after reset.
  yield Settle()

  # Print a test header.
  print( "--- ALU Tests ---" )

  # Test the bitwise 'AND' operation.
  print( "AND (&) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, C_AND, 0xCCCC0000 )
  yield from alu_ut( alu, 0x00000000, 0x00000000, C_AND, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_AND, 0xFFFFFFFF )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, C_AND, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, C_AND, 0x00000000 )

  # Test the bitwise 'OR' operation.
  print( "OR  (|) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, C_OR, 0xCCCCCCCC )
  yield from alu_ut( alu, 0x00000000, 0x00000000, C_OR, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_OR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, C_OR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, C_OR, 0xFFFFFFFF )

  # Test the bitwise 'XOR' operation.
  print( "XOR (^) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, C_XOR, 0x0000CCCC )
  yield from alu_ut( alu, 0x00000000, 0x00000000, C_XOR, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_XOR, 0x00000000 )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, C_XOR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, C_XOR, 0xFFFFFFFF )

  # Test the bitwise 'XNOR' operation.
  print( "XNOR (!^) tests:" )
  yield from alu_ut( alu, 0xCCCCCCCC, 0xCCCC0000, C_XNOR, 0xFFFF3333 )
  yield from alu_ut( alu, 0xCCCCCCCC, 0x33330000, C_XNOR, 0x00003333 )
  yield from alu_ut( alu, 0x00000000, 0x00000000, C_XNOR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_XNOR, 0xFFFFFFFF )
  yield from alu_ut( alu, 0x00000000, 0xFFFFFFFF, C_XNOR, 0x00000000 )
  yield from alu_ut( alu, 0xFFFFFFFF, 0x00000000, C_XNOR, 0x00000000 )

  # Test the addition operation.
  print( "ADD (+) tests:" )
  yield from alu_ut( alu, 0, 0, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ut( alu, 0, 1, C_ADD, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ut( alu, 1, 0, C_ADD, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ut( alu, 0xFFFFFFFF, 1, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ut( alu, 29, 71, C_ADD, 100 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ut( alu, 0x80000000, 0x80000000, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 1 )
  yield from alu_ut( alu, 0x7FFFFFFF, 0x7FFFFFFF, C_ADD, 0xFFFFFFFE )
  yield from check_nzv( alu, 1, 0, 1 )

  # Test the subtraction operation.
  print( "SUB (-) tests:" )
  yield from alu_ut( alu, 0, 0, C_SUB, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ut( alu, 0, 1, C_SUB, 0xFFFFFFFF )
  yield from check_nzv( alu, 1, 0, 0 )
  yield from alu_ut( alu, 1, 0, C_SUB, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ut( alu, -1, 1, C_SUB, 0xFFFFFFFE )
  yield from check_nzv( alu, 1, 0, 0 )
  yield from alu_ut( alu, -1, -1, C_SUB, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ut( alu, 0x7FFFFFFF, 0x80000000, C_SUB, 0xFFFFFFFF )
  yield from check_nzv( alu, 1, 0, 1 )
  yield from alu_ut( alu, 0x80000000, 0x7FFFFFFF, C_SUB, 1 )
  yield from check_nzv( alu, 0, 0, 1 )

  # Test the division operation.
  print( "DIV (/) tests:" )
  yield from alu_ut( alu, 0, 0, C_DIV, 0 )
  yield from alu_ut( alu, 1, 0, C_DIV, 0 )
  yield from alu_ut( alu, 1, 1, C_DIV, 1 )
  yield from alu_ut( alu, 0, 1, C_DIV, 0 )
  yield from alu_ut( alu, 0, 2, C_DIV, 0 )
  yield from alu_ut( alu, 1, 2, C_DIV, 0 )
  yield from alu_ut( alu, 2, 2, C_DIV, 1 )
  yield from alu_ut( alu, 3, 2, C_DIV, 1 )
  yield from alu_ut( alu, 4, 2, C_DIV, 2 )
  yield from alu_ut( alu, 8, 2, C_DIV, 4 )
  yield from alu_ut( alu, 8, 3, C_DIV, 2 )
  yield from alu_ut( alu, 42, 2, C_DIV, 21 )
  yield from alu_ut( alu, -42, 2, C_DIV, -21 )
  yield from alu_ut( alu, 42, -2, C_DIV, -21 )
  yield from alu_ut( alu, -42, -2, C_DIV, 21 )

  # Test the multiplication operation.
  print( "MUL (*) tests:" )
  yield from alu_ut( alu, 0, 0, C_MUL, 0 )
  yield from alu_ut( alu, 1, 0, C_MUL, 0 )
  yield from alu_ut( alu, 0, 1, C_MUL, 0 )
  yield from alu_ut( alu, 1, 1, C_MUL, 1 )
  yield from alu_ut( alu, 2, 1, C_MUL, 2 )
  yield from alu_ut( alu, 2, 2, C_MUL, 4 )
  yield from alu_ut( alu, 3, 3, C_MUL, 9 )
  yield from alu_ut( alu, 6, 7, C_MUL, 42 )
  yield from alu_ut( alu, 6, -7, C_MUL, -42 )
  yield from alu_ut( alu, -6, 7, C_MUL, -42 )
  yield from alu_ut( alu, -6, -7, C_MUL, 42 )

  # Test the '==' comparison operation.
  print( "CMP (==) tests:" )
  yield from alu_ut( alu, 0, 0, C_CEQ, 1 )
  yield from alu_ut( alu, 1, 0, C_CEQ, 0 )
  yield from alu_ut( alu, 0, 1, C_CEQ, 0 )
  yield from alu_ut( alu, 42, 42, C_CEQ, 1 )
  yield from alu_ut( alu, -42, -42, C_CEQ, 1 )
  yield from alu_ut( alu, 42, -42, C_CEQ, 0 )

  # Test the '<' comparison operation.
  print( "CMP (<) tests:" )
  yield from alu_ut( alu, 0, 0, C_CLT, 0 )
  yield from alu_ut( alu, 1, 0, C_CLT, 0 )
  yield from alu_ut( alu, 0, 1, C_CLT, 1 )
  yield from alu_ut( alu, -1, 0, C_CLT, 1 )
  yield from alu_ut( alu, -42, -10, C_CLT, 1 )
  yield from alu_ut( alu, -10, -42, C_CLT, 0 )

  # Test the '<=' comparison operation.
  print( "CMP (<=) tests:" )
  yield from alu_ut( alu, 0, 0, C_CLE, 1 )
  yield from alu_ut( alu, 1, 0, C_CLE, 0 )
  yield from alu_ut( alu, 0, 1, C_CLE, 1 )
  yield from alu_ut( alu, -1, 0, C_CLE, 1 )
  yield from alu_ut( alu, -42, -10, C_CLE, 1 )
  yield from alu_ut( alu, -10, -42, C_CLE, 0 )
  yield from alu_ut( alu, -42, -42, C_CLE, 1 )

  # Test the shift left operation.
  print ( "SHL (<<) tests:" )
  yield from alu_ut( alu, 0x00000001, 0, C_SHL, 0x00000001 )
  yield from alu_ut( alu, 0x00000001, 1, C_SHL, 0x00000002 )
  yield from alu_ut( alu, 0x00000001, 4, C_SHL, 0x00000010 )
  yield from alu_ut( alu, 0x00000010, 4, C_SHL, 0x00000100 )
  yield from alu_ut( alu, 0x80000000, 1, C_SHL, 0x00000000 )

  # Test the shift right operation.
  print ( "SHR (>>) tests:" )
  yield from alu_ut( alu, 0x00000001, 0, C_SHR, 0x00000001 )
  yield from alu_ut( alu, 0x00000001, 1, C_SHR, 0x00000000 )
  yield from alu_ut( alu, 0x00000011, 1, C_SHR, 0x00000008 )
  yield from alu_ut( alu, 0x00000010, 1, C_SHR, 0x00000008 )
  yield from alu_ut( alu, 0x80000000, 1, C_SHR, 0x40000000 )
  yield from alu_ut( alu, 0x80000000, 4, C_SHR, 0x08000000 )

  # Test the shift right with sign extension operation.
  print ( "SRA (>> + sign extend) tests:" )
  yield from alu_ut( alu, 0x00000001, 0, C_SRA, 0x00000001 )
  yield from alu_ut( alu, 0x00000001, 1, C_SRA, 0x00000000 )
  yield from alu_ut( alu, 0x00000011, 1, C_SRA, 0x00000008 )
  yield from alu_ut( alu, 0x00000010, 1, C_SRA, 0x00000008 )
  yield from alu_ut( alu, 0x80000000, 1, C_SRA, 0xC0000000 )
  yield from alu_ut( alu, 0x80000000, 4, C_SRA, 0xF8000000 )

  # Done.
  yield Tick()
  print( "ALU Tests: %d Passed, %d Failed"%( p, f ) )

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate an ALU module.
  dut = ALU()

  # Run the tests.
  with Simulator( dut, vcd_file = open( 'alu.vcd', 'w' ) ) as sim:
    def proc():
      yield from alu_test( dut )
    sim.add_clock( 24e-6 )
    sim.add_clock( 24e-6, domain = "nsync" )
    sim.add_sync_process( proc )
    sim.run()
