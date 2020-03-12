# Simple nMigen CPU

This is a basic nMigen implementation of the educational RISC computer which is used to teach MIT's edX 6004.2x free online course.

I've been brushing up on digital logic since it's been a few years since college and I want to design a minimal implementation of the RISC-V ISA without going crawling back to Verilog or VHDL. So I figured I'd follow along with some online classes using nMigen alongside the provided simulators as a learning exercise.

The 6004.1X and 6004.2X courses are archived, but you can still watch the lectures and see the example problems here:

[6004.1x](https://courses.edx.org/courses/course-v1:MITx+6.004.1x_3+3T2016/course/)

[6004.2x](https://courses.edx.org/courses/course-v1:MITx+6.004.2x_2+3T2016/course/)


And you can do the lab assignments here, even though they are no longer available on the edX website:

["Computation Structures" course website](https://computationstructures.org/)

The .1x course mostly covers the background of how digital circuit design works at a high level, but the last few modules cover basic pipelining and describes a [simple ALU.](https://github.com/WRansohoff/nmigen_alu_test)

The .2x course covers the design and implementation of a more complete SoC, which I'm trying to implement here. It uses an educational RISC ISA, and the edX course has named it the "Beta processor".

# Why use the "Beta" ISA?

There are no good reasons, only legacy ones.

I'm planning to use this as a stepping stone towards implementing a RISC-V CPU, and it actually looks like the real MIT 6.004 classes are now teaching with RISC-V instead of their in-house ISA. But they haven't updated the free online edX courses, so most of us can't follow the newer material online. C'est la vie.

Fortunately, the 32-bit "Beta" ISA is very similar to RISC-V, so it should be fairly easy to make the switch. To implement the base `RV32I` spec, I'll probably only need to change the opcodes and instruction formats. (I hope...)

# Usage

You'll need to install the [nMigen library](https://github.com/m-labs/nmigen/) with Python 3.x to run the testbenches. You can either install it from the repository's source code, or use pip:

    pip3 install nmigen nmigen-boards

You can run test suites for individual components by running the corresponding Python file. For example, to test the ALU module:

    python3 alu.py

The result waveforms are saved in a `.vcd` file with the same name as the `.py` file.

# CPU Tests

The CPU module is a bit more complicated than its submodules, so running the `cpu.py` file will run a series of test applications on the simulated CPU, each with its own `cpu_*.vcd` result file.

There are some basic tests covering every instruction except for division and multiplication, which I skipped since those operations are not part of the base `RV32I` instruction set. I guess I'll 

I haven't implemented a 'reset' signal for this CPU, and I don't have a good way to swap out the ROM modules once the simulated device object is instantiated. So for now, each test application creates a new CPU device and simulation. But it looks like nMigen automatically implements a 'reset' signal for each module which can bring every `Signal`s to its `reset` value (unless their `reset_less` attribute is set), so it's probably possible to simplify things.
