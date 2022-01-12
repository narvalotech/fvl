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

(defun make-params (paramlist)
  (loop for param in paramlist collect
        (apply #'make-param param)))

(defun pprint-param (param)
  (with-slots (name width form) param
    (format nil "~a : ~a (~a bits)" name form width)))

(defun make-opcode (op-list)
  (destructuring-bind (mne op params desc-op desc) op-list
    (make-instance 'fv1-op
                   :mnemonic mne
                   :opcode   op
                   :desc-op  desc-op
                   :desc     desc
                   :params   (make-params params))))

(defun show-coding (opcode)
  "Return a representation of the instruction coding, in the style of the
   SpinASM instruction manual."
  (let ((repr-str (copy-seq "00000000000000000000000000000000")))
    (loop for i below (integer-length (opcode opcode))
          do (setf (schar repr-str (- 31 i))
                   (if (logbitp i (opcode opcode)) #\1 #\0)))
    (loop for param in (params opcode) do
      (loop for i below (width param)
            do (setf (schar repr-str (- 31 i (pos param)))
                     (schar (name param) 0))))
    repr-str))

;; FIXME: temp location for the opcode list
;; Load the opcode definitions inside *fv1-opcodes*
(defparameter *fv1-opcodes*
  (loop for op in
                   (with-open-file
                       (stream (uiop:parse-unix-namestring "./fv1-opcodes.lisp"))
                     (read stream))
        collect (make-opcode op)))

(defmacro generate-registers (name addr num)
  `(list ,@(loop for i from 0 to (1- num)
                 collecting
                 `'(,(read-from-string (format nil "~A~A" name i))
                    ,(+ addr i)))))

(defparameter *fv1-registers*
  (append
   (list
    '(SIN0_RATE  #x00) ; Sine LFO 0 rate
    '(SIN0_RANGE #x01) ; Sine LFO 0 range
    '(SIN1_RATE  #x02) ; Sine LFO 1 rate
    '(SIN1_RANGE #x03) ; Sine LFO 1 range
    '(RMP0_RATE  #x04) ; Ramp LFO 0 rate
    '(RMP0_RANGE #x05) ; Ramp LFO 0 range
    '(RMP1_RATE  #x06) ; Ramp LFO 1 rate
    '(RMP1_RANGE #x07) ; Ramp LFO 1 range
    '(POT0       #x10) ; Potentiometer 0 value
    '(POT1       #x11) ; Potentiometer 1 value
    '(POT2       #x12) ; Potentiometer 2 value
    '(ADCL       #x14) ; ADC value (left)
    '(ADCR       #x15) ; ADC value (right)
    '(DACL       #x16) ; DAC value (left)
    '(DACR       #x17) ; DAC value (right)
    '(ADDR_PTR   #x18) ; Used with RMPA inst for indirect read
    )
   ;; Generate REG0 to REG31
   (generate-registers "REG" #x20 32)))

(defparameter *fv1-symbols*
  (list
   ;; Used with CHO instructions
   '(SIN0 #x00) ; SINE LFO 0
   '(SIN1 #x01) ; SINE LFO 1
   '(COS0 #x08) ; COSE LFO 0
   '(COS1 #x09) ; COSE LFO 1
   '(RMP0 #x02) ; RAMP LFO 0
   '(RMP1 #x03) ; RAMP LFO 1

   '(RDA   #x00) ; ACC += (DRAM * COEFF)
   '(SOF   #x02) ; ACC = (ACC * LFO COEFF) + Constant
   '(RDAL  #x03) ; Reads  value of selected LFO into the ACC
   '(SIN   #x00) ; SIN/COS from SINE LFO
   '(COS   #x01) ; SIN/COS from SINE LFO
   '(REG   #x02) ; Save LFO temp reg in LFO block
   '(COMPC #x04) ; 2's comp : Generate 1-x for interpolate
   '(COMPA #x08) ; 1's comp address offset (Generate SIN or COS)
   '(RPTR2 #x10) ; Add 1/2 to ramp to generate 2nd ramp for pitch shift
   '(NA    #x20) ; Do NOT add LFO to address and select cross-face coefficient

   ;; Used with SKP instruction
   '(RUN #x80000000) ; Skip if NOT FIRST time through program
   '(ZRC #x40000000) ; Skip On Zero Crossing
   '(ZRO #x20000000) ; Skip if ACC = 0
   '(GEZ #x10000000) ; Skip if ACC is' >= 0'
   '(NEG #x8000000)  ; Skip if ACC is Negative
   ))

(defun find-opcode (mne)
  (loop for opcode in *fv1-opcodes*
        until (eql (mnemonic opcode) mne)
        finally (return opcode)))

;; Dump all opcodes + params to stdout
(format t "Available opcodes ~%")
(loop for opcode in *fv1-opcodes* do
  (print (mnemonic opcode))
  (print (show-coding opcode))
  (loop for param in (params opcode) do
        (print (pprint-param param))))

;; Process a single opcode list
;; loop across all possible opcodes until eql
(defun process-instruction (inst)
  (let* ((op (find-opcode (car inst)))
        (inst-word (logand #xFFFFFFFF (opcode op)))) ; opcode can be up to word-len
    (loop for param in (params op)
          counting T into i
          do (setf inst-word
                   (logior inst-word
                           (encode-param (nth i inst) param)))
          finally (return (values inst-word op)))))

;; Encode param depending on:
;; - type/form
;; - width
;; - position

(defun get-keyword-value (val list)
  (loop for sym in list
        do (if (eql (car sym) val)
               (return (cadr sym))
               nil)))

(defun decode-reg-sym (val)
  "Return the value of the FV-1 register or reserved symbol."
  (if (typep val 'symbol)
      (cond ((get-keyword-value val *fv1-registers*))
            ((get-keyword-value val *fv1-symbols*))
            (t nil))
      val))

(defun bor (&rest param-list)
  "Bitwise-OR, accepts FV-1 symbols and registers."
  (reduce 'logior
   (loop for sym in param-list collecting (decode-reg-sym sym))))

(defgeneric encode-param-m (value param param-type)
  (:documentation "Encode an instruction/opcode parameter in binary at its place
  in the instruction word."))

(defun encode-param (value param)
  (ash
   (logand (1- (ash 1 (width param)))
           (cond ((typep value 'symbol)
                  (cond  ((eql (schar (string value) 0) #\$)
                          ;; Use hex value directly if specified with $
                          (read-from-string
                           (format nil "#x~A" (subseq (string value) 1))))
                         (; Parse if register or reserved symbol
                          (decode-reg-sym value))
                         (t (format t "Unrecognized symbol"))))
                 ((typep value 'list) (eval value))
                 ;; TODO: replace "form" with less ambiguous name
                 ;; TODO: remove generics maybe ?
                 (t (encode-param-m value param (form param)))))
   (pos param)))

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

(defun decode-fixed-point (int-bits frac-bits value)
  (if (logbitp (+ int-bits frac-bits) value)
      (/
       ;; Mask includes the sign bit
       (logand (1- (ash 1 (+ int-bits frac-bits 1)))
               (1+ (lognot value)))
       (ash 1 frac-bits)
       -1)
      (/ value (ash 1 frac-bits))))

(defmethod encode-param-m (value param (param-type (eql 's.10)))
  (encode-fixed-point 0 10 value))

(defmethod encode-param-m (value param (param-type (eql 's1.9)))
  (encode-fixed-point 1 9 value))

(defmethod encode-param-m (value param (param-type (eql 's4.6)))
  (encode-fixed-point 4 6 value))

(defmethod encode-param-m (value param (param-type (eql 's.15)))
  (encode-fixed-point 0 15 value))

(defmethod encode-param-m (value param (param-type (eql 's1.14)))
  (encode-fixed-point 1 14 value))

(defun show-binary-instruction (inst)
  "Same as get-instruction-coding, but with the actual parameter values."
  (multiple-value-bind (word op)
      (process-instruction inst)
    (format nil "~%~a~%~32,'0B~%~:*~8,'0X"
            (show-coding op)
            word)))

;; Assemble test file
;; Read test ASM file into a list
;; Note: need to set directory to where this file is first.
(defparameter *asm-sexp*
  (with-open-file
      (stream (uiop:parse-unix-namestring "./testasm.fvl"))
    (loop for line = (read-line stream nil)
          until (eq line nil) collect (read-from-string line))))

(loop for ins in *asm-sexp* do
  (print (show-binary-instruction ins)))
