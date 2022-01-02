;; FV-1 assembler
;;
;; Goal:
;; - Express instructions as s-expressions
;; - Write to binary & intel hex
;; - Disassemble from binary & intel hex
;; - Read and write from classic .spn assembly files
;; - Perform same checks as spinasm

;; Instruction format
;; ------------------
;; [label:] opcode(,subopcode),operand1(,operand2) [;comment]

;; Operand data types
;; ------------------
;; - signed fixed point values
;; - unsigned integers
;; - bit vectors
;;
;; Signed fixed point values
;; - typically used as coefficients (operand2).
;; - may be in different formats: S1.14, S1.9 and S.10
;;
;; S1.14:
;; - 16bit coeff
;; - 1 sign bit (MSB)
;; - 1 int bit
;; - 14 frac bits
;;
;; S1.9 and S.10 have the same logic, with 11 bits instead.
;;
;; Range & resolution
;; |       | Bits | Range               | Resolution (LSB) |
;; |-------+------+---------------------+------------------|
;; | S1.14 |   16 | -2 to 1.99993896484 |    0.00006103516 |
;; | S1.9  |   11 | -2 to 1.998046875   |      0.001953125 |
;; | S.10  |   11 | -1 to 0.9990234375  |     0.0009765625 |
;;
;; Entry formats
;; - can be entered as real numbers or hex
;; - real numbers:
;;   - 1 digit int portion
;;   - decimal point
;;   - multiple fractional digits
;;   - can be prefixed with + or -
;;   - will be rounded to nearest LSB
;; - hex:
;;   - prefixed with $
;;   - right-justified
;;   - zeroes optional
;;   - S.10 cannot be represented
;;
;; Examples
;; |-------+-------+----------------+-------+---------------+---------------|
;; | S1.14 |       |                |       |               |               |
;; | Real  |    -2 | ­0.00006103516 |     0 | 0.00006103516 | 1.99993896484 |
;; | Hex   | $8000 | $FFFF          | $0000 |         $0001 |         $7FFF |
;; |       |       |                |       |               |               |
;; | S1.9  |       |                |       |               |               |
;; | Real  |    -2 | ­0.001953125   |     0 |   0.001953125 |   1.998046875 |
;; | Hex   |  $400 | $7FF           |  $000 |          $001 |          $3FF |
;; |       |       |                |       |               |               |
;; | S.10  |       |                |       |               |               |
;; | Real  |    -1 | ­0.0009765625  |     0 |  0.0009765625 |  0.9990234375 |
;; |-------+-------+----------------+-------+---------------+---------------|
;;
;; Unsigned and signed integers
;; - primarily used to specify an adress (operand1).
;; - (address) range of uint is dependent on the opcode (dram vs regs)
;; - uint also used to specify the num of instruction to be skipped with SKP.
;;   It must be enteredd in decimal in that case.
;;
;; Entry formats
;; - decimal or hex ($ prefix)
;;
;; Bit vectors
;; - combination of 0 and 1 prefixed with %.
;;   - must specify all bits
;;   - can use _ char to separate 8bits
;; - hex format: $13 -> %010011 if applied to 6 bit vector
;; - or-ing values to set particular bits
;;   - 4|1 -> %000101
;;
;;
;; Assembler statements
;; --------------------
;;
;; EQU Statement
;;
;; Allows to define symbolic operands to increase readability.
;; (i.e. aliases/defines)
;;
;; Name EQU Value [;comment]
;; -> replace any occurrence of the literal "Name" by the literal "Value".
;;
;; - "Name" must be an unique string
;; - <32 chars
;; - first char is a letter excl. +, - and !.
;;
;; Predefined symbols:
;; using EQU statement with them will redefine them
;;
;; MEM Statement
;;
;; Allows user to partition the delay ram memory into blocks.
;;
;; Name MEM Size [;comment]
;;
;; memory block "Name" can be referenced from within instruction line.
;; "Name" complies w/ same rules as for EQU.
;; "Size" is uint in range 1-32768, in decimal or hex.
;;
;; assembler also defines:
;; "Name" : first mem location within block
;; "Name#": last mem location
;; "Name^": middle of mem block
;;
;; special cases:
;; - if size=1, name = name^ = name#
;; - if size=2, name = name^
;; - if size even (can't be halved), name^ = size MOD 2
;;
;; All 3 identifiers can be offset by a positive or negative int, entered in decimal.
;; The user can't control where memory is placed.
;;
;; FV-1 instruction set
;; --------------------
;;
;; Groups of instructions:
;; - Accumulator
;; - Register
;; - Delay ram
;; - LFO
;; - Pseudo opcodes
;;
;; 32-bit wide
;; 5 bit opcode, coeff, address specifier
;; (except for LFOs and bool acc instructions)
;;
;; Register instructions
;; - 6 bits for addressing the internal registers
;; - 16 bits coefficient
;; - 5 bits reserved (set to 0)
;;
;; Delay ram instructions
;; - 16 bits address
;; - 11 bits coeff
;;
;; Pseudo opcodes are convenience instructions: they can be replaced by the
;; generic combination of instructions they are based upon.

;; Parsing assembly s-exps:
;; (opcode [subopcode] operand1 [operand2] "comment")
;;
;; opcode should be an interned string
;; we need to know/store:
;; - opcode string
;; - opcode binary rep
;; - doc shortstring
;; - parameters
;;   - name
;;   - bit position
;;   - width
;;   - entry format (s1.14/10 etc)
;;   - range
;;
;; example
;; sof  0.25,  -0.125   ; POT0 in ACC, -0.125 to +0.125
;; (sof 0.25 -0.125 "POT0 in ACC, -0.125 to +0.125")
;;
(defclass fv1-op ()
  ((mnemonic :initarg :mnemonic :accessor mnemonic)
   (opcode   :initarg :opcode   :accessor opcode)
   (desc-op  :initarg :desc-op  :accessor desc-op)
   (desc     :initarg :desc     :accessor desc)
   (params   :initarg :params   :accessor params :initform nil)))

(defclass fv1-param ()
  ((name   :initarg :name   :accessor name)
   (pos    :initarg :pos    :accessor pos)
   (width  :initarg :width  :accessor width)
   (form   :initarg :form   :accessor form)
   (range  :initarg :range  :accessor range :initform nil)))

(defun make-param (name pos width form &optional (range nil))
  (make-instance 'fv1-param
                 :name   name
                 :pos    pos
                 :width  width
                 :form   form
                 :range  range))

;; TODO: need fns for coding parameters into binary, depending on their properties.

(defun make-params (paramlist)
  (loop for param in paramlist collect
        (apply #'make-param param)))

(defun make-opcode (mne op desc-op desc params)
  (make-instance 'fv1-op
                 :mnemonic mne
                 :opcode   op
                 :desc-op  desc-op
                 :desc     desc
                 :params   (make-params params)))

(defun show-coding (opcode)
  "Return a representation of the instruction coding, in the style of the
   SpinASM instruction manual."
  (let ((repr-str "00000000000000000000000000000000"))
    (loop for param in (params opcode) do
      (loop for i below (width param)
            do (setf (schar repr-str (- 31 i (pos param)))
                     (schar (name param) 0))))
    (loop for i below (integer-length (opcode opcode))
          do (setf (schar repr-str (- 31 i))
                   (if (logbitp i (opcode opcode)) #\1 #\0)))
    repr-str))

;; FIXME: temp location for the opcode list
(defparameter *fv1-opcodes* (make-array 1000 :adjustable t :fill-pointer 0))

;; Testing
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

;; (print (mnemonic (aref *fv1-opcodes* 0)))

;; Read test ASM file into a list
(defparameter *asm-sexp*
  (with-open-file
      (stream "/home/john/Seafile/Projects/Hardware/FV1clip/software/assembler/testasm.fvl")
    (loop for line = (read-line stream nil)
          until (eq line nil) collect (read-from-string line))))

;; (print *asm-sexp*)
;; (eql 'sof (car (car *asm-sexp*)))

;; Process a single opcode list
;; loop across all possible opcodes until eql
(defun process-instruction (inst)
  (let* ((op
          (loop for opcode across *fv1-opcodes*
                until (eql (mnemonic opcode) (car inst))
                finally (return opcode)))
        (inst-word (logand #x0000001F (opcode op)))) ; 5-bit opcode
    (loop for param in (params op)
          counting T into i
          do (setf inst-word
                   (logior inst-word
                           (encode-param (nth i inst) param)))
          finally (return (values inst-word op)))))

;; (format nil "~B" (process-instruction (nth 0 *asm-sexp*)))

;; Encode param depending on:
;; - type/form
;; - width
;; - position

(defun encode-param (value param)
  ;; FIXME: properly encode raw hex
  (ash
   (logand (1- (ash 1 (width param)))
           ;; FIXME: hack because I can't eql on a slot
           ;; maybe generics are too much, should just use cond instead
           (encode-param-m value param (form param)))
   (pos param)))

(defgeneric encode-param-m (value param param-type)
  (:documentation "Encode an instruction/opcode parameter in binary at its place
  in the instruction word."))

(defmethod encode-param-m (value param (param-type (eql 'uint)))
  value)

(defun saturate (int-bits frac-bits value)
  ;; TODO: emit warning/condition on saturation
  (cond
    ((>= value (ash 1 (+ int-bits frac-bits)))
     (1- (ash 1 (+ int-bits frac-bits))))
    (t value)))

(defun encode-fixed-point (int-bits frac-bits value)
  (let ((fixed (truncate (* (abs value) (ash 1 frac-bits)))))
    (logior
     ;; Sign bit
     (ash (if (< value 0) 1 0) (+ int-bits frac-bits))
     ;; Fixed point conversion
     (logand
      (if (>= value 0)
          (saturate int-bits frac-bits fixed)
          (logand (1+ (lognot fixed))))
      (1- (ash 1 (+ int-bits frac-bits)))))))

;; (encode-fixed-point 0 10 .5)
;; (encode-fixed-point 0 10 0.9991)
;; (encode-fixed-point 0 10 0.000977)
;; (encode-fixed-point 0 10 -0.000977)
;; (encode-fixed-point 1 9 1.998046875)
;; (encode-fixed-point 1 9 -1.998046875)
;; (encode-fixed-point 1 9 -1.5)
;; (encode-fixed-point 1 9 -2)
;; (encode-fixed-point 1 9 -1)
;; (encode-fixed-point 1 9 1)
;; (encode-fixed-point 1 9 .5)
;; (encode-fixed-point 1 9 2)       ; saturation test
;; (encode-fixed-point 1 9 .5)

(defun decode-fixed-point (int-bits frac-bits value)
  (if (logbitp (+ int-bits frac-bits) value)
      (/
       ;; Mask includes the sign bit
       (logand (1- (ash 1 (+ int-bits frac-bits 1)))
               (1+ (lognot value)))
       (ash 1 frac-bits)
       -1)
      (/ value (ash 1 frac-bits))))

;; (decode-fixed-point 0 10 #x1)
;; (decode-fixed-point 0 10 #x3ff)
;; (decode-fixed-point 0 10 #x7ff)
;; (decode-fixed-point 1 9 #x1)
;; (decode-fixed-point 1 9 #x500)
;; (decode-fixed-point 1 9 #x7ff)
;; (decode-fixed-point 1 9 #x3ff)
;; (decode-fixed-point 1 9 511)

(defmethod encode-param-m (value param (param-type (eql 's.10)))
  (encode-fixed-point 0 10 value))

(defmethod encode-param-m (value param (param-type (eql 's1.9)))
  (encode-fixed-point 1 9 value))

(defmethod encode-param-m (value param (param-type (eql 's1.14)))
  (encode-fixed-point 1 14 value))

(defun show-binary-instruction (inst)
  "Same as get-instruction-coding, but with the actual parameter values."
  (multiple-value-bind (word op)
      (process-instruction inst)
    (format nil "~%~a~%~32,'0B~%~:*~8,'0X"
            (show-coding op)
            word)))

(loop for ins in *asm-sexp* do
  (print (show-binary-instruction ins)))
