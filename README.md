FV1 lisp-based assembler
------------------------

This (very much work-in-progress) project aims to replace the SpinASM program
from spinsemi:

It should allow one to write assembly programs using s-expressions (possibly calling out to common lisp operators, e.g. arithmetic) for the FV-1 chip.

This project is mainly intended for me to learn lisp a bit more, and its
ultimate goal is to be able to hot-reload code running on the FV-1 wirelessly,
using a SoC that emulates the program RAM.

For more info, see assembler.lisp.

Remaining work
--------------

- GUI editor
- Allow function/macro `blocks` in the assembly
- looping test using SKP instructions
- "project builder" like spinasm
  - build a program image based on multiple asm sources
- ihex disassembler
- reader/converter for .spn syntax

