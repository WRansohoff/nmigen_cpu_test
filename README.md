# Simple nMigen ALU

This is an nMigen implementation of the educational CPU which is used in MIT's edX 6004.2x free online course.

I wanted to brush up on digital logic since it's been a few years since college and I want to design a minimal implementation of the RISC-V ISA in nMigen. So I figured I'd follow along with some online classes using nMigen alongside the provided simulators as a learning exercise.

The 6004.1X and 6004.2X courses are archived, but you can still watch the lectures and see the example problems here:

[6004.1x](https://courses.edx.org/courses/course-v1:MITx+6.004.1x_3+3T2016/course/)

[6004.2x](https://courses.edx.org/courses/course-v1:MITx+6.004.2x_2+3T2016/course/)


And you can do the lab assignments here, even though they are no longer available on the edX website:

["Computation Structures" course website](https://computationstructures.org/)

The .1x course mostly covers the background of how digital circuit design works at a high level, but the last few modules cover basic pipelining and describes a simple ALU.

The .2x course looks like it covers the design of a more complete SoC (which is implemented here). It uses an educational RISC ISA which the course calls the "Beta ISA".

# Usage

You'll need to install the [nMigen library](https://github.com/m-labs/nmigen/) with Python 3.x to run the ALU tests. You can either install it from the repository's source code, or use pip:

    pip3 install nmigen nmigen-boards

You can run test suites for individual components by running the corresponding Python file. For example, to test the ALU module:

    python3 alu.py

The result waveforms are saved in a `.vcd` file with the same name as the `.py` file.
