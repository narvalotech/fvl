((sof #b01101 (("C" 16 16 s1.14 nil)
               ("D" 5 11 s.10 nil))
       "C * ACC + D"
       "SOF will multiply the current value in ACC with C and will then add the
  constant D to the result.")

 (and #b01110 (("M" 8 24 uint nil))
       "ACC & MASK"
       "AND will perform a bitwise 'and' of the current ACC and the specified 24b
  MASK." )

 (or #b01111 (("M" 8 24 uint nil))
      "ACC | MASK"
      "OR will perform a bitwise 'or' of the current ACC and the specified 24b
  MASK.")

 (xor #b10000 (("M" 8 24 uint nil))
       "ACC ^ MASK"
       "XOR will perfxorm a bitwise 'xor' of the current ACC and the specified
  24b MASK.")

 (log #b01011 (("C" 16 16 s1.14 nil)
               ("D" 5 11 s4.6 nil))
       "C * LOG(|ACC|) + D"
       "LOG will multiply the Base2 LOG of the current absolute value in ACC with
C and add the constant D to the result.

It is important to note that the LOG function returns a fixed point number in
S4.19 format instead of the standard S.23 format, which in turn means that the
most negative Base2 LOG value is –16.

The LOG instruction can handle absolute linear accumulator values from
0.99999988 to 0.00001526 which translates to a dynamic range of apx. 96dB.

D an offset to be added to the logarithmic value in the range of –16 to +
15.999998.")

 (exp #b01100 (("C" 16 16 s1.14 nil)
               ("D" 5 11 s.10 nil))
       "C * EXP(ACC) + D"
       "EXP will multiply 2^ACC with C and add the constant D to the result.

Since ACC (in it’s role as the destination for the EXP instruction) is limited
to linear values from 0 to +0.99999988, the EXP instruction is limited to
logarithmic ACC values (in it’s role as the source operand for the EXP
instruction) from –16 to 0.

Like the LOG instruction, EXP will treat the ACC content as a S4.19 number.

Positive logarithmic ACC values will be clipped to +0.99999988 which is the most
positive linear value that can be represented within the accumulator.

D is intended to allow the linear ACC to be offset by a constant in the range
from –1 to +0.9990234375")

 (skp #b10001 (("C" 27 5 uint nil)
               ("N" 21 6 uint nil))
       "CMASK N"
       "The SKP instruction allows conditional program execution.

The FV­1 features five condition flags that can be used to conditionally skip
the next N instructions.

The selection of which condition flag(s) must be asserted in order to skip the
next N instructions is made by the five bit condition mask “CMASK”.

Only if all condition flags that correspond to a logic '1' within CMASK are
asserted are the following N instructions skipped.

The individual bits within CMASK correspond to the FV­1 condition flags as
follows:

| CMASK | Flag | Description                        |
|-------+------+------------------------------------|
| b4    | RUN  | Cleared after the first execution. |
| b3    | ZRC  | Zero Crossing                      |
| b2    | ZRO  | ACC = 0                            |
| b1    | GEZ  | ACC >= 0                           |
| b0    | NEG  | ACC < 0                            |
")
)
