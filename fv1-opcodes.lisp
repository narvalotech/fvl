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

 ;; Register instructions
 ;; ---------------------
 (rdax #b00100 (("A" 5 6 uint nil)
                ("C" 16 16 s1.14 nil))
       "C * REG[ADDR] + ACC"
       "RDAX will fetch the value contained in [ADDR] from the register file,
multiply it with C and add the result to the previous content of ACC.

This multiply accumulate is probably the most popular operation found in DSP
algorithms.")

 (wrax #b00110 (("A" 5 6 uint nil)
                ("C" 16 16 s1.14 nil))
       "ACC->REG[ADDR], C * ACC"
       "WRAX will save the current value in ACC to [ADDR] and then multiply ACC
by C.

This instruction can be used to write ACC to one DAC channel while clearing ACC
for processing the next audio channel.

In order to simplify the WRAX syntax, see the list of predefined symbols for all
registers within the FV­1.")

 (maxx #b01001 (("A" 5 6 uint nil)
                ("C" 16 16 s1.14 nil))
       "MAX(|REG[ADDR] * C|, |ACC|)"
       "MAXX will compare the absolute value of ACC versus C times the absolute
value of the register pointed to by ADDR. If the absolute value of ACC is larger
ACC will be loaded with |ACC|, otherwise the accumulator becomes overwritten by
|REG[ADDR] * C|.")

 (mulx #b01010 (("A" 5 6 uint nil))
       "ACC * REG[ADDR]"
       "MULX will multiply ACC by the value of the register pointed to by ADDR.

An important application of the MULX instruction is squaring the content of ACC,
which combined with a single order LP is especially useful in calculating the
RMS value of an arbitrary waveform.")

 (rdfx #b00101 (("A" 5 6 uint nil)
                ("C" 16 16 s1.14 nil))
       "C * (ACC->REG[ADDR]) + REG[ADDR]"
       "RDFX will subtract the value of the register pointed to by ADDR from
ACC, multiply the result by C and then add the value of the register pointed to
by ADDR.

RDFX is an extremely powerful instruction in that it represents the major
portion of a single order low pass filter.")

 (wrlx #b01000 (("A" 5 6 uint nil)
                ("C" 16 16 s1.14 nil))
       "ACC->REG[ADDR], C * (PACC - ACC) + PACC"
       "First the current ACC value is stored into the register pointed to by
ADDR, then ACC is subtracted from the previous content of ACC (PACC). The
difference is then multiplied by C and finally PACC is added to the result.

WRLX is an extremely powerful instruction in that when combined with RDFX, it
forms a single order low pass shelving filter")

 (wrhx #b00111 (("A" 5 6 uint nil)
                ("C" 16 16 s1.14 nil))
       "ACC->REG[ADDR], C * ACC + PACC"
       "The current ACC value is stored in the register pointed to by ADDR, then
ACC is multiplied by C. Finally the previous content of ACC (PACC) is added to
the product.

WRHX is an extremely powerful instruction in that when combined with RDFX, it
forms a single order high pass shelving filter.")

 ;; Delay RAM instructions
 ;; ----------------------
 (rda #b00000 (("A" 5 16 uint nil)
               ("C" 21 11 s1.9 nil))
      "C * SRAM[ADDR] + ACC"
      "RDA will fetch the sample [ADDR] from the delay ram, multiply it by C and
add the result to the previous content of ACC.

This multiply accumulate is probably the most popular operation found in DSP
algorithms.")

 (rmpa #b1100000001 (("C" 21 11 s1.9 nil))
       "C * SRAM[PNTR|N|] + ACC"
       "RMPA provides indirect delay line addressing in that the delay line
address of the sample to be multiplied by C is not explicitly given in the
instruction itself but contained within the pointer register ADDR_PTR
(absolute address 24 within the internal register file).

RMPA will fetch the indirectly addressed sample from the delay ram, multiply it
by C and add the result to the previous content of ACC.")

 (wra #b00010 (("A" 5 16 uint nil)
               ("C" 21 11 s1.9 nil))
      "ACC->SRAM[ADDR], ACC * C"
      "WRA will store ACC to the delay ram location addressed by ADDR and then
      multiply ACC by C.")

 (wrap #b00011 (("A" 5 16 uint nil)
                ("C" 21 11 s1.9 nil))
       "ACC->SRAM[ADDR], ACC * C + LR"
       "WRAP will store ACC to the delay ram location addressed by ADDR then
multiply ACC by C and finally add the content of the LR register to the product.

Please note that the LR register contains the last sample value read from the
delay ram memory. This instruction is typically used for all­pass filters in a
reverb program.")

 ;; LFO instructions
 ;; ----------------
 (wlds #b10010 (("N" 29 1 uint nil)
                ("F" 20 9 uint nil)
                ("A" 5 15 uint nil))
       "See description"
       "WLDS will load frequency and amplitude control values into the selected
SIN LFO (N = 0 or 1).

This instruction is intended to setup the selected SIN LFO which is typically
done within the first sample iteration after a new program is loaded. As a
result WLDS will in most cases be used in combination with a SKP RUN
instruction. For a more detailed description regarding the frequency and
amplitude control values see application note AN­0001.")

 ;; Same opcode as wlds except bit 30
 (wldr #b01000000000000000000000000010010
       (("N" 29 1 uint nil)
        ("F" 13 16 uint nil)
        ("A" 5 2 uint nil))
       "See description"
       "WLDR will load frequency and amplitude control values into the selected
RAMP LFO (N = 0 or 1).

This instruction is intended to setup the selected RAMP LFO which is typically
done within the first sample iteration after a new program became loaded. As a
result WLDR will in most cases be used in combination with a SKP RUN
instruction. For a more detailed description regarding the frequency and
amplitude control values see application note AN­0001.")

 (jam #b10010011 (("N" 6 1 uint nil))
      "0->RAMP LFO N"
      "JAM will reset the selected RAMP LFO to its starting point.")

 (cho-rda #b10100 (("N" 21 2 uint nil)
                   ("C" 24 6 uint nil)
                   ("A" 5 16 uint nil))
          "See description"
          "Like the RDA instruction, CHO RDA will read a sample from the delay
ram, multiply it by a coefficient and add the product to the previous content of
ACC.

However, in contrast to RDA the coefficient is not explicitly embedded within
the instruction and the effective delay ram address is not solely determined by
the address parameter.

Instead, both values are modulated by the selected LFO at run time, for an in
depth explanation please consult the FV­1 datasheet alongside with application
note AN­0001.

CHO RDA is a very flexible and powerful instruction, especially useful for delay
line modulation effects such as chorus or pitch shifting.

The coefficient field of the 'CHO' instructions are used as control bits to
select various aspects of the LFO. These bits can be set using predefined flags
that are ORed together to create the required bit field.

N: LFO select: SIN0, SIN1, RMP0, RMP1
C: Bit flags

For a sine wave LFO (SIN0 or SIN1), valid flags are: SIN COS REG COMPC COMPA
While for a ramp LFO (RMP0 and RMP1), valid flags are: REG COMPC COMPA RPTR2
NA.

| Flag  | Hex | Description                                 |
|-------+-----+---------------------------------------------|
| SIN   |   0 | Select SIN output (default)                 |
| COS   |   1 | Select COS output                           |
| REG   |   2 | Save LFO out into LFO register              |
| COMPC |   4 | Complement the coef (1-coeff)               |
| COMPA |   8 | Complement the addr offset from LFO         |
| RPTR2 |  10 | Select (ramp+1/2) pointer (ramp only)       |
| NA    |  20 | Select x-fade coeff, do not add addr offset |
")

 (cho-sof #b10000000000000000000000000010100
          (("N" 21 2 uint nil)
           ("C" 24 6 uint nil)
           ("D" 5 16 s.15 nil))
          "See description"
          "Like the SOF instruction, CHO SOF will multiply ACC by a coefficient
and add the constant D to the result.

However, in contrast to SOF the coefficient is not explicitly embedded within
the instruction. Instead, based on the selected LFO and the 6 bit vector C, the
coefficient is picked from a list of possible coefficients available within the
LFO block of the FV­1.

For an in depth explanation please consult the FV­ 1 datasheet alongside with
application note AN­0001.

CHO SOF is a very flexible and powerful instruction, especially useful for the
cross fading portion of pitch shift algorithms.

N: LFO select: SIN0, SIN1, RMP0, RMP1
C: Bit flags

For a sine wave LFO (SIN0 or SIN1), valid flags are: SIN COS REG COMPC COMPA
While for a ramp LFO (RMP0 and RMP1), valid flags are: REG COMPC COMPA RPTR2
NA.

| Flag  | Hex | Description                                 |
|-------+-----+---------------------------------------------|
| SIN   |   0 | Select SIN output (default)                 |
| COS   |   1 | Select COS output                           |
| REG   |   2 | Save LFO out into LFO register              |
| COMPC |   4 | Complement the coef (1-coeff)               |
| COMPA |   8 | Complement the addr offset from LFO         |
| RPTR2 |  10 | Select (ramp+1/2) pointer (ramp only)       |
| NA    |  20 | Select x-fade coeff, do not add addr offset |
 ")

 (cho-rdal #b11000010000000000000000000010100
           (("N" 21 2 uint nil))
           "LFO * 1 -> ACC"
           "CHO RDAL will read the current value of the selected LFO into ACC.

N: LFO select: SIN0, COS0, SIN1, COS1, RMP0, RMP1.")

 ;; Pseudo opcodes
 ;; --------------

 (nop #b10001 ()
      "No-op"
      "Fill empty program space with this. Equivalent to SKP with unset
  params.")

 (clr #b01110 ()
      "0 -> ACC"
      "CLR will clear the accumulator.")

 (not #b11111111111111111111111100010000 ()
      "~ACC -> ACC"
      "NOT will negate all bit positions whithin accumulator this performing a
  1's complement.")

 (absa #b01001 ()
       "|ACC| -> ACC"
       "Loads the accumulator with the absolute value of the accumulator.")

 (ldax #b00101 (("A" 5 6 uint nil))
       "REG[ADDR] -> ACC"
       "Loads the accumulator with the contents of the addressed register.
There seems to be a typo in the manual, the position of ADDR is unknown.
TODO: assemble this instruction to find out where ADDR is.")
 )
