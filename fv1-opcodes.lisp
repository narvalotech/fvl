(vector-push
 (make-opcode 'sof #b01101
               "C * ACC + D"
               "SOF will multiply the current value in ACC with C and will then add the
  constant D to the result."
               '(("C" 16 16 s1.14 nil)
                 ("D" 5 11 s.10 nil))) *fv1-opcodes*)

(vector-push
 (make-opcode 'and #b01110
               "ACC & MASK"
               "AND will perform a bitwise 'and' of the current ACC and the
  specified 24b MASK."
              '(("M" 8 24 uint nil))) *fv1-opcodes*)
