from nmigen import *
from nmigen.back.pysim import *

###############
# ALU module: #
###############

# ALU definitions: [ bitcode, string ]
C_AND = [ 0b101000, "&" ]
C_OR  = [ 0b101110, "|" ]
C_XOR = [ 0b100110, "^" ]
C_A   = [ 0b101010, "=" ]
C_ADD = [ 0b010000, "+" ]
C_SUB = [ 0b010001, "-" ]
C_CEQ = [ 0b000011, "==" ]
C_CLT = [ 0b000101, "<" ]
C_CLE = [ 0b000111, "<=" ]
C_SHL = [ 0b110000, "<<" ]
C_SHR = [ 0b110001, ">>" ]
C_SRA = [ 0b110011, ">>" ]

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

    # 'Counter' which determines how many cycles have elapsed in
    # the current operation.
    cnt = Signal( 2 )
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
    with m.If( fn.matches( '01---0' ) ):
      # With addition, overflow occurred if the two input signs are
      # the same and those signs differ from the result's sign.
      m.d.comb += self.v.eq( ( xa[ 31 ] == xb[ 31 ] ) &
                             ( xa[ 31 ] != self.y[ 31 ] ) )
    with m.Elif( fn.matches( '01---1' ) ):
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
        with m.If( self.start ):
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
        m.d.sync += [
          self.start.eq( 0 ),
          self.done.eq( 1 )
        ]
        # Perform ALU computations based on the 'function' bits.
        # Boolean unit (F = [...]):
        #  - 0b101000: Y = A AND B
        with m.If( fn == C_AND[ 0 ] ):
          m.d.sync += self.y.eq( xa & xb )
        #  - 0b101110: Y = A  OR B
        with m.Elif( fn == C_OR[ 0 ] ):
          m.d.sync += self.y.eq( xa | xb )
        #  - 0b100110: Y = A XOR B
        with m.Elif( fn == C_XOR[ 0 ] ):
          m.d.sync += self.y.eq( xa ^ xb )
        #  - 0b101010: Y = A
        with m.Elif( fn == C_A[ 0 ] ):
          m.d.sync += self.y.eq( xa )
        # Arithmetic unit (F = [...]):
        #  - 0b01xxx0: Y = A + B
        with m.Elif( fn.matches( '01---0' ) ):
          m.d.sync += self.y.eq( xa + xb )
        #  - 0b01xxx1: Y = A - B
        with m.Elif( fn.matches( '01---1' ) ):
          m.d.sync += self.y.eq( xa - xb )
        # Comparison unit (F = [...]):
        #  - 0b00x011: Y = ( A == B )
        with m.Elif( fn.matches( '00-011' ) ):
          m.d.sync += self.y.eq( xa == xb )
        #  - 0b00x101: Y = ( A <  B )
        with m.Elif( fn.matches( '00-101' ) ):
          # Can't use 'xa < xb' because HW logic doesn't account
          # for negative numbers, i.e. 0xFFFFFFFF > 0x00000000.
          m.d.sync += self.y.eq( ( ( xb - xa ) > 0 ) &
                                 ( ( xb - xa )[ 31 ] == 0 ) )
        #  - 0b00x111: Y = ( A <= B )
        with m.Elif( fn.matches( '00-111' ) ):
          # Same as above; 'xa <= xb' hardware description
          # does not account for negative numbers.
          m.d.sync += self.y.eq( ( ( xb - xa ) >= 0 ) &
                                 ( ( xb - xa )[ 31 ] == 0 ) )
        # Shift unit (F = [...]):
        #  - 0b11xx00: Y = A << B
        with m.Elif( fn.matches( '11--00' ) ):
          m.d.sync += self.y.eq( xa << xb )
        #  - 0b11xx01: Y = A >> B (no sign extend)
        with m.Elif( fn.matches( '11--01' ) ):
          m.d.sync += self.y.eq( xa >> xb )
        #  - 0b11xx11: Y = A >> B (with sign extend)
        with m.Elif( fn.matches( '11--11' ) ):
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

# Helper method to pretty-print a 2s-complement 32-bit hex string.
def hexs( h ):
  if h >= 0:
    return "0x%08X"%( h )
  else:
    return "0x%08X"%( ( h + ( 1 << 32 ) ) % ( 1 << 32 ) )

# Perform an individual ALU functional test.
def alu_ft( alu, a, b, fn, expected ):
  # Set A, B, F.
  yield alu.a.eq( a )
  yield alu.b.eq( b )
  yield alu.f.eq( fn[ 0 ] )
  # Set 'start'
  yield alu.start.eq( 1 )
  # Wait two ticks.
  yield Tick()
  yield Tick()
  # Done. Check the result after combinational logic settles.
  yield Settle()
  act = yield alu.y
  if expected != act:
    print( "\033[31mFAIL:\033[0m %s %s %s = %s (got: %s)"
           %( hexs( a ), fn[ 1 ], hexs( b ),
              hexs( expected ), hexs( act ) ) )
  else:
    print( "\033[32mPASS:\033[0m %s %s %s = %s"
           %( hexs( a ), fn[ 1 ], hexs( b ), hexs( expected ) ) )

# Helper method to verify that 'N', 'Z', 'V' flags are set correctly.
def check_nzv( alu, n, z, v ):
  an = yield alu.n
  az = yield alu.z
  av = yield alu.v
  if ( an == n ) and ( az == z ) and ( av == v ):
    print( "\033[32m  PASS:\033[0m N, Z, V flags: %d, %d, %d"
           %( n, z, v ) )
  else:
    print( "\033[31m  FAIL:\033[0m N, Z, V flags: " \
           "%d, %d, %d (got: %d, %d, %d)"
           %( n, z, v, an, az, av ) )

# Top-level ALU test method.
def alu_test( alu ):
  # Let signals settle after reset.
  yield Settle()

  # Test the bitwise 'AND' operation.
  print( "AND (&) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_AND, 0xCCCC0000 )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_AND, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_AND, 0xFFFFFFFF )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_AND, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_AND, 0x00000000 )

  # Test the bitwise 'OR' operation.
  print( "OR  (|) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_OR, 0xCCCCCCCC )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_OR, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_OR, 0xFFFFFFFF )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_OR, 0xFFFFFFFF )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_OR, 0xFFFFFFFF )

  # Test the bitwise 'XOR' operation.
  print( "XOR (^) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_XOR, 0x0000CCCC )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_XOR, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_XOR, 0x00000000 )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_XOR, 0xFFFFFFFF )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_XOR, 0xFFFFFFFF )

  # Test the 'Y = A' operation.
  print( "A   (=) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_A, 0xCCCCCCCC )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_A, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_A, 0xFFFFFFFF )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_A, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_A, 0xFFFFFFFF )

  # Test the addition operation.
  print( "ADD (+) tests:" )
  yield from alu_ft( alu, 0, 0, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 0, 1, C_ADD, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 1, 0, C_ADD, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 0xFFFFFFFF, 1, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 29, 71, C_ADD, 100 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 0x80000000, 0x80000000, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 1 )
  yield from alu_ft( alu, 0x7FFFFFFF, 0x7FFFFFFF, C_ADD, 0xFFFFFFFE )
  yield from check_nzv( alu, 1, 0, 1 )

  # Test the subtraction operation.
  print( "SUB (-) tests:" )
  yield from alu_ft( alu, 0, 0, C_SUB, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 0, 1, C_SUB, 0xFFFFFFFF )
  yield from check_nzv( alu, 1, 0, 0 )
  yield from alu_ft( alu, 1, 0, C_SUB, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, -1, 1, C_SUB, 0xFFFFFFFE )
  yield from check_nzv( alu, 1, 0, 0 )
  yield from alu_ft( alu, -1, -1, C_SUB, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 0x7FFFFFFF, 0x80000000, C_SUB, 0xFFFFFFFF )
  yield from check_nzv( alu, 1, 0, 1 )
  yield from alu_ft( alu, 0x80000000, 0x7FFFFFFF, C_SUB, 1 )
  yield from check_nzv( alu, 0, 0, 1 )

  # Test the '==' comparison operation.
  print( "CMP (==) tests:" )
  yield from alu_ft( alu, 0, 0, C_CEQ, 1 )
  yield from alu_ft( alu, 1, 0, C_CEQ, 0 )
  yield from alu_ft( alu, 0, 1, C_CEQ, 0 )
  yield from alu_ft( alu, 42, 42, C_CEQ, 1 )
  yield from alu_ft( alu, -42, -42, C_CEQ, 1 )
  yield from alu_ft( alu, 42, -42, C_CEQ, 0 )

  # Test the '<' comparison operation.
  print( "CMP (<) tests:" )
  yield from alu_ft( alu, 0, 0, C_CLT, 0 )
  yield from alu_ft( alu, 1, 0, C_CLT, 0 )
  yield from alu_ft( alu, 0, 1, C_CLT, 1 )
  yield from alu_ft( alu, -1, 0, C_CLT, 1 )
  yield from alu_ft( alu, -42, -10, C_CLT, 1 )
  yield from alu_ft( alu, -10, -42, C_CLT, 0 )

  # Test the '<=' comparison operation.
  print( "CMP (<=) tests:" )
  yield from alu_ft( alu, 0, 0, C_CLE, 1 )
  yield from alu_ft( alu, 1, 0, C_CLE, 0 )
  yield from alu_ft( alu, 0, 1, C_CLE, 1 )
  yield from alu_ft( alu, -1, 0, C_CLE, 1 )
  yield from alu_ft( alu, -42, -10, C_CLE, 1 )
  yield from alu_ft( alu, -10, -42, C_CLE, 0 )
  yield from alu_ft( alu, -42, -42, C_CLE, 1 )

  # Test the shift left operation.
  print ( "SHL (<<) tests:" )
  yield from alu_ft( alu, 0x00000001, 0, C_SHL, 0x00000001 )
  yield from alu_ft( alu, 0x00000001, 1, C_SHL, 0x00000002 )
  yield from alu_ft( alu, 0x00000001, 4, C_SHL, 0x00000010 )
  yield from alu_ft( alu, 0x00000010, 4, C_SHL, 0x00000100 )
  yield from alu_ft( alu, 0x80000000, 1, C_SHL, 0x00000000 )

  # Test the shift right operation.
  print ( "SHR (>>) tests:" )
  yield from alu_ft( alu, 0x00000001, 0, C_SHR, 0x00000001 )
  yield from alu_ft( alu, 0x00000001, 1, C_SHR, 0x00000000 )
  yield from alu_ft( alu, 0x00000011, 1, C_SHR, 0x00000008 )
  yield from alu_ft( alu, 0x00000010, 1, C_SHR, 0x00000008 )
  yield from alu_ft( alu, 0x80000000, 1, C_SHR, 0x40000000 )
  yield from alu_ft( alu, 0x80000000, 4, C_SHR, 0x08000000 )

  # Test the shift right with sign extension operation.
  print ( "SRA (>> + sign extend) tests:" )
  yield from alu_ft( alu, 0x00000001, 0, C_SRA, 0x00000001 )
  yield from alu_ft( alu, 0x00000001, 1, C_SRA, 0x00000000 )
  yield from alu_ft( alu, 0x00000011, 1, C_SRA, 0x00000008 )
  yield from alu_ft( alu, 0x00000010, 1, C_SRA, 0x00000008 )
  yield from alu_ft( alu, 0x80000000, 1, C_SRA, 0xC0000000 )
  yield from alu_ft( alu, 0x80000000, 4, C_SRA, 0xF8000000 )

  # Done.
  yield Tick()

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
