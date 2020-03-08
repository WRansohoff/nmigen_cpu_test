from nmigen import *
from nmigen.back.pysim import *

###############
# ALU module: #
###############

# ALU definitions: [ bitcode, string ]
C_AND = [ 0b101000, "&" ]
C_OR  = [ 0b101001, "|" ]
C_XOR = [ 0b101010, "^" ]
C_ADD = [ 0b100000, "+" ]
C_SUB = [ 0b100001, "-" ]
C_CEQ = [ 0b100100, "==" ]
C_CLT = [ 0b100101, "<" ]
C_CLE = [ 0b100110, "<=" ]
C_SHL = [ 0b101100, "<<" ]
C_SHR = [ 0b101101, ">>" ]
C_SRA = [ 0b101110, ">>" ]

class ALU( Elaboratable ):
  def __init__( self ):
    # 'A' and 'B' data inputs.
    self.a = Signal( 32, reset = 0x00000000 )
    self.b = Signal( 32, reset = 0x00000000 )
    # 'F' function select input.
    self.f = Signal( 6,  reset = 0b000000 )
    # 'Y' data output.
    self.y = Signal( 32, reset = 0x00000000 )
    # 'N' arithmetic flag (last result was negative)
    self.n = Signal()
    # 'Z' arithmetic flag (last result == zero)
    self.z = Signal()
    # 'V' arithmetic flag (last result overflowed)
    self.v = Signal()
    # 'Start' and 'Done' signalling bits.
    self.start = Signal()
    self.done  = Signal()

  def elaborate( self, platform ):
    # Core ALU module.
    m = Module()

    # Internal 'busy' signal.
    busy = Signal()
    # Latched input values.
    xa  = Signal( 32 )
    xb  = Signal( 32 )
    fn  = Signal( 6 )
    # Extra register to hold sign-extension bits.
    signex = Signal( 32 )

    # Combinational N, Z, and V arithmetic flags.
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
      m.d.comb += self.v.eq( 0 )

    # Main ALU FSM; two states, "IDLE" and "BUSY".
    with m.FSM() as fsm:
      # Latch input / function values when start = 1 in "IDLE" state.
      with m.State( "ALU_IDLE" ):
        m.d.comb += busy.eq( self.start )
        with m.If( busy ):
          m.d.sync += [
            xa.eq( self.a ),
            xb.eq( self.b ),
            fn.eq( self.f ),
            self.done.eq( 0 )
          ]
          m.next = "ALU_BUSY"
        with m.Else():
          m.d.sync += self.done.eq( 1 )
      with m.State( "ALU_BUSY" ):
        # Set shared 'end of operation' values for the next cycle.
        m.next = "ALU_IDLE"
        m.d.comb += busy.eq( 0 )
        m.d.sync += self.done.eq( 1 )
        # Perform ALU computations based on the 'function' bits.
        # Boolean unit (F = [...]):
        #  - 0b101000: Y = A AND B
        with m.If( fn == C_AND[ 0 ] ):
          m.d.sync += self.y.eq( xa & xb )
        #  - 0b101001: Y = A  OR B
        with m.Elif( fn == C_OR[ 0 ] ):
          m.d.sync += self.y.eq( xa | xb )
        #  - 0b101010: Y = A XOR B
        with m.Elif( fn == C_XOR[ 0 ] ):
          m.d.sync += self.y.eq( xa ^ xb )
        # Arithmetic unit (F = [...]):
        #  - 0b100000: Y = A + B
        with m.Elif( fn == C_ADD[ 0 ] ):
          m.d.sync += self.y.eq( xa + xb )
        #  - 0b100001: Y = A - B
        with m.Elif( fn == C_SUB[ 0 ] ):
          m.d.sync += self.y.eq( xa - xb )
        # Comparison unit (F = [...]):
        #  - 0b100100: Y = ( A == B )
        with m.Elif( fn == C_CEQ[ 0 ] ):
          m.d.sync += self.y.eq( xa == xb )
        #  - 0b100101: Y = ( A <  B )
        with m.Elif( fn == C_CLT[ 0 ] ):
          # Can't use 'xa < xb' because HW logic doesn't account
          # for negative numbers, i.e. 0xFFFFFFFF > 0x00000000.
          m.d.sync += self.y.eq( ( ( xb - xa ) > 0 ) &
                                 ( ( xb - xa )[ 31 ] == 0 ) )
        #  - 0b100110: Y = ( A <= B )
        with m.Elif( fn == C_CLE[ 0 ] ):
          # Same as above; 'xa <= xb' hardware description
          # does not account for negative numbers.
          m.d.sync += self.y.eq( ( ( xb - xa ) >= 0 ) &
                                 ( ( xb - xa )[ 31 ] == 0 ) )
        # Shift unit (F = [...]):
        #  - 0b101100: Y = A << B
        with m.Elif( fn == C_SHL[ 0 ] ):
          m.d.sync += self.y.eq( xa << xb )
        #  - 0b101101: Y = A >> B (no sign extend)
        with m.Elif( fn == C_SHR[ 0 ] ):
          m.d.sync += self.y.eq( xa >> xb )
        #  - 0b101110: Y = A >> B (with sign extend)
        with m.Elif( fn == C_SRA[ 0 ] ):
          m.d.sync += self.y.eq( ( xa >> xb ) | signex )
          # Calculate sign extension bit with combinational logic.
          with m.If( xa[ 31 ] == 0 ):
            m.d.comb += signex.eq( 0x00000000 )
          with m.Else():
            m.d.comb += signex.eq(
              ( ( xb > 0 )  << 31 ) | ( ( xb > 1 )  << 30 ) |
              ( ( xb > 2 )  << 29 ) | ( ( xb > 3 )  << 28 ) |
              ( ( xb > 4 )  << 27 ) | ( ( xb > 5 )  << 26 ) |
              ( ( xb > 6 )  << 25 ) | ( ( xb > 7 )  << 24 ) |
              ( ( xb > 8 )  << 23 ) | ( ( xb > 9 )  << 22 ) |
              ( ( xb > 10 ) << 21 ) | ( ( xb > 11 ) << 20 ) |
              ( ( xb > 12 ) << 19 ) | ( ( xb > 13 ) << 18 ) |
              ( ( xb > 14 ) << 17 ) | ( ( xb > 15 ) << 16 ) |
              ( ( xb > 16 ) << 15 ) | ( ( xb > 17 ) << 14 ) |
              ( ( xb > 18 ) << 13 ) | ( ( xb > 19 ) << 12 ) |
              ( ( xb > 20 ) << 11 ) | ( ( xb > 21 ) << 10 ) |
              ( ( xb > 22 ) << 9  ) | ( ( xb > 23 ) << 8  ) |
              ( ( xb > 24 ) << 7  ) | ( ( xb > 25 ) << 6  ) |
              ( ( xb > 26 ) << 5  ) | ( ( xb > 27 ) << 4  ) |
              ( ( xb > 28 ) << 3  ) | ( ( xb > 30 ) << 2  ) |
              ( ( xb > 31 ) << 1  )
            )
        # Return 0 after one clock cycle for unrecognized commands.
        with m.Else():
          m.d.sync += self.y.eq( 0x00000000 )

    # End of ALU module definition.
    return m

##################
# ALU testbench: #
##################
# Keep track of test pass / fail rates.
p = 0
f = 0

# Helper method to pretty-print a 2s-complement 32-bit hex string.
def hexs( h ):
  if h >= 0:
    return "0x%08X"%( h )
  else:
    return "0x%08X"%( ( h + ( 1 << 32 ) ) % ( 1 << 32 ) )

# Perform an individual ALU unit test.
def alu_ut( alu, a, b, fn, expected ):
  global p, f
  # Set A, B, F.
  yield alu.a.eq( a )
  yield alu.b.eq( b )
  yield alu.f.eq( fn[ 0 ] )
  # Set 'start'
  yield alu.start.eq( 1 )
  # Wait two ticks, clearing 'start' after one tick.
  yield Tick()
  yield alu.start.eq( 0 )
  yield Tick()
  # Done. Check the result after combinational logic settles.
  yield Settle()
  actual = yield alu.y
  if expected != actual:
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
    sim.add_sync_process( proc )
    sim.run()
