# Simple nMigen CPU

This is a work-in-progress nMigen implementation of the educational CPU which is used in MIT's edX 6004.2x free online course.

I've been brushing up on digital logic since it's been a few years since college and I want to design a minimal implementation of the RISC-V ISA without going crawling back to Verilog or VHDL. So I figured I'd follow along with some online classes using nMigen alongside the provided simulators as a learning exercise.

The 6004.1X and 6004.2X courses are archived, but you can still watch the lectures and see the example problems here:

[6004.1x](https://courses.edx.org/courses/course-v1:MITx+6.004.1x_3+3T2016/course/)

[6004.2x](https://courses.edx.org/courses/course-v1:MITx+6.004.2x_2+3T2016/course/)


And you can do the lab assignments here, even though they are no longer available on the edX website:

["Computation Structures" course website](https://computationstructures.org/)

The .1x course mostly covers the background of how digital circuit design works at a high level, but the last few modules cover basic pipelining and describes a [simple ALU.](https://github.com/WRansohoff/nmigen_alu_test)

The .2x course covers the design of a more complete SoC, which I'm trying to implement here. It uses an educational RISC ISA, and the edX course has named it the "Beta processor".

# Usage

You'll need to install the [nMigen library](https://github.com/m-labs/nmigen/) with Python 3.x to run the testbenches. You can either install it from the repository's source code, or use pip:

    pip3 install nmigen nmigen-boards

You can run test suites for individual components by running the corresponding Python file. For example, to test the ALU module:

    python3 alu.py

The result waveforms are saved in a `.vcd` file with the same name as the `.py` file.

I haven't written tests for the actual CPU module yet; instead, the `cpu.py` file simulates the CPU's execution for a period of time given the ROM module's initial values. Only the `ADD` and `ADDC` operations work right now, so I haven't put much thought into how to encode program data for execution.
