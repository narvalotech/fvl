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
   (range  :initarg :range  :accessor range :initform nil)
   (op-mne :initarg :op-mne :accessor op-mne)))

(defun make-param (mne name pos width form &optional (range nil))
  (make-instance 'fv1-param
                 :name   name
                 :pos    pos
                 :width  width
                 :form   form
                 :range  range
                 :op-mne mne))

(defun make-params (paramlist mne)
  (loop for param in paramlist collect
        (apply #'make-param (append (list mne) param))))

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
                   :params   (make-params params mne))))

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
   '(RUN #x10) ; Skip if NOT FIRST time through program
   '(ZRC #x8) ; Skip On Zero Crossing
   '(ZRO #x4) ; Skip if ACC = 0
   '(GEZ #x2) ; Skip if ACC is' >= 0'
   '(NEG #x1)  ; Skip if ACC is Negative
   ))

(defparameter *equ-list*
  (list '(nil nil)))

(defun find-opcode (mne)
  (loop for opcode in *fv1-opcodes*
        if (eql (mnemonic opcode) mne) do (return opcode)
        finally (return nil)))

;; Dump all opcodes + params to stdout
(format t "Available opcodes ~%")
(loop for opcode in *fv1-opcodes* do
  (format t "~%")
  (print (mnemonic opcode))
  (print (desc-op opcode))
  (print (show-coding opcode))
  (loop for param in (params opcode) do
        (print (pprint-param param))))

(defun add-to-kv (kv name val)
  "Add/update element in key-value table."
  (let ((pos
          (loop for el in kv
                counting t into pos
                when (eql (car el) name)
                  return (1- pos))))
    (if pos
        (setf (nth pos kv) (list name val))
        (setf kv (append kv (list (list name val)))))
    kv))

(defparameter *memory-max-bytes* 32768)
(defparameter *memory-blocks* nil) ; format: (name (addr size))

(defun get-next-free-addr ()
  "Gets address after last byte of allocated memory."
  (if (not *memory-blocks*) 0
      (loop for el in *memory-blocks*
            finally
               (return
                 (+ 1 ; spinasm adds 1 after each region
                    (nth 0 (nth 1 el))
                    (nth 1 (nth 1 el)))))))

(defun create-mem-region (name size)
  "Allocate a new memory region in order. Not smart."
  (let ((addr (get-next-free-addr)))
    (if (< (+ addr size) *memory-max-bytes*)
        (setf *memory-blocks*
              (append *memory-blocks*
                      (list (list name (list addr size)))))
        (error "Memory capacity exceeded."))))

(defun process-statement (inst)
  "Process statements that are not FV-1 opcodes (e.g. EQU, LABEL, etc)."
  (cond
    ((eql (car inst) 'EQU)
     (setf *equ-list* (add-to-kv *equ-list* (nth 1 inst) (nth 2 inst))) t)
    ((eql (car inst) 'LABEL) t) ; Ignore LABELs
    ((eql (car inst) 'MEM) (create-mem-region (nth 1 inst) (nth 2 inst)) t)
    (t (error (format nil "Unable to parse opcode: ~A" (car inst))))))

(defun process-instruction (inst &key generated)
  "Process a single instruction form (s-expression)."
  (let ((op (find-opcode (car inst))))
    ;; Increment only if instruction originates from source file
    (if (not generated) (setf *inst-curr* (subseq *inst-curr* 1)))
    (if (eql op nil)
        (values (process-statement inst) op)
        (let ((inst-word (logand #xFFFFFFFF (opcode op))))
          (loop for param in (params op)
                counting T into i
                do (setf inst-word
                         (logior
                          inst-word
                          (ash
                           (logand (1- (ash 1 (width param)))
                                   (encode-param (nth i inst) param))
                           (pos param))))
                finally (return (values inst-word op)))))))

(defun nop-padding (amount)
  (loop repeat amount collect (process-instruction '(nop) :generated t)))

(defun process-instructions (inst-list &key padding)
  (append
   (loop for ins in inst-list collect (process-instruction ins))
   (if padding
       (nop-padding (- 512 (length inst-list))))))

(defun resolve-skip-label (label param)
  "Return the number of instructions to skip (until the label is found)."
  (if (eql (op-mne param) 'skp)
      (loop for ins in *inst-curr*
            counting (find-opcode (car ins)) into n
            do
               (if (eql (nth 0 ins) 'label)
                (if (eql (nth 1 ins) label)
                  (return n))))
      nil))

(defun get-keyword-value (val list)
  "Return the value of a symbol if it exists in the given table."
  (loop for sym in list
        do (if (eql (car sym) val)
               (return (cadr sym))
               nil)))

(defun remove-from-symbol (val char)
  (read-from-string (remove char (string val))))

(defun resolve-addr (val &key pos)
  "Return the address of the named memory region."
  (cond ((search "^" (string val))
         (setf pos 'middle)
         (setf val (remove-from-symbol val #\^)))

        ((search "#" (string val))
         (setf pos 'end)
         (setf val (remove-from-symbol val #\#))))

  (let ((mem-region (get-keyword-value val *memory-blocks*)))
    (if (and mem-region (nth 1 mem-region))
        (let* ((addr (coerce (nth 1 mem-region) 'integer))
               (offset
                 (cond
                   ((eql pos nil) 0)
                   ((eql pos 'start) 0)
                   ((eql pos 'end) (1- addr))
                   ((eql pos 'middle) (1- (ceiling (/ addr 2))))
                   (t 0))))
          (+ (car mem-region) offset))
        nil)))

(defun mem-middle (name)
  "Return the address of the midpoint of the named memory region."
  (resolve-addr name :pos 'middle))

(defun mem-end (name)
  "Return the address of the last byte of the named memory region."
  (resolve-addr name :pos 'end))

(defun resolve-equ (val param)
  "Return the value of the EQU variable."
  (let ((equ-value (get-keyword-value val *equ-list*)))
    (cond (equ-value (encode-param equ-value param))
          (t nil))))

(defun resolve-reg-sym (val)
  "Return the value of the FV-1 register or reserved symbol."
  (if (typep val 'symbol)
      (cond ((get-keyword-value val *fv1-registers*))
            ((get-keyword-value val *fv1-symbols*))
            (t nil))
      val))

(defun resolve-in-list (val param)
  "Return a lisp form, resolving each element to its real value."
  (append
   (list (car val))
   (loop for el in (subseq val 1) collecting (encode-param el param))))

(defun bor (&rest param-list)
  "Bitwise-OR on a list."
  (reduce 'logior param-list))

(defun read-spin-hex (val)
  "Return binary value of hex value specified with SpinASM syntax."
  (if (eql (schar (string val) 0) #\$)
      (read-from-string
       (format nil "#x~A" (subseq (string val) 1)))
      nil))

(defun read-spin-bin (val)
  "Return binary value of bit vector value specified with SpinASM syntax."
  (if (eql (schar (string val) 0) #\%)
      (read-from-string
       (format nil "#b~A" (subseq (remove #\_ (string val)) 1)))
      nil))

(defgeneric encode-param-m (value param param-type)
  (:documentation "Encode an instruction/opcode parameter in binary at its place
  in the instruction word."))

(defun encode-param (value param)
  (cond ((typep value 'symbol)
         (cond  (; Use hex value directly if specified with $
                 (read-spin-hex value))
                (; Use bin value directly if specified with %
                 (read-spin-bin value))
                (; Parse if register or reserved symbol
                 (resolve-reg-sym value))
                (; Parse if EQU entry exists
                 (resolve-equ value param))
                (; Parse if pointing to LABEL
                 (resolve-skip-label value param))
                (; Return address if pointing to mem region
                 (resolve-addr value))
                (t (error
                    (format nil "Unable to parse param: ~A." value)))))
        ((typep value 'list)
         (cond
           ;; Don't resolve memory region names for memory helpers
           ((or (eql (car value) 'mem-middle)
                (eql (car value) 'mem-end))
           (funcall (car value) (cadr value)))
           ;; Attempt to evaluate s-exp, resolving symbols recursively.
           (t (eval (resolve-in-list value param)))))
        ((eql value nil) (error "Encoded value is NIL."))
        ;; TODO: replace "form" with less ambiguous name
        ;; TODO: remove generics maybe ?
        (t (encode-param-m value param (form param)))))

(defmethod encode-param-m (value param (param-type (eql 'uint)))
  value)

(defun saturate (int-bits frac-bits value)
  (let ((limit (1- (ash 1 (+ int-bits frac-bits)))))
    (cond
      ((> value limit)
       (progn
         (warn "Saturation: #x~x -> #x~x" value limit)
         limit))
      (t value))))

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

; Poor man's intel hex formatting
; 512 instructions per-program
(defun bytes-from-string (str)
  "Converts a string of hex bytes to a list of numbers."
  (loop for c across str
        when (eql 0 (mod i 2))
          collect (read-from-string
                   (format nil "#x~A" (subseq str i (+ 2 i))))
        counting t into i))

(defun calc-ihex-checksum (str)
  "Calculate intel hex checksum."
  (logand #xFF (1+ (lognot (reduce '+ (bytes-from-string str))))))

(defparameter *curr-addr* 0)
(defun encode-ihex (word)
  "Encodes a single instruction word in intel hex format."
  (let ((repr (format nil "04~4,'0X00~8,'0X" *curr-addr* word)))
    (setf *curr-addr* (+ 4 *curr-addr*))
    (format nil ":~A~2,'0X"
            repr
            (calc-ihex-checksum repr))))

(defun encode-ihex-eof ()
  (format nil ":00000001FF"))

(defun get-byte (word pos)
  (ldb (byte 8 (* pos 8)) word))

(defun word-to-c-array (word)
  "Formats a 32-bit word to a 8-bit c array format"
  (format nil "0x~2,'0X, 0x~2,'0X, 0x~2,'0X, 0x~2,'0X,~%"
          (get-byte word 3)
          (get-byte word 2)
          (get-byte word 1)
          (get-byte word 0)))

(defun inst-words-to-c-array (words)
  (loop for word in words
        if (typep word 'integer)
          collecting (word-to-c-array word)))

(defun encode-c-header (inst-list)
  "Convert an instruction list to a C header, padded with NOPs"
  (concatenate 'string
               (format nil "uint8_t program[] = {~%")
               (format nil "~{~a~}" (inst-words-to-c-array (process-instructions inst-list :padding t)))
               (format nil "};~%")))

(defun show-binary-instruction (inst)
  "Same as get-instruction-coding, but with the actual parameter values."
  (multiple-value-bind (word op)
      (process-instruction inst)
    (if (eql op nil) nil ; don't show anything if not real opcode
        (format nil "~%~a~%~32,'0B~%~:*~8,'0X"
                (show-coding op)
                word))))

;; Assemble test file
;; Read test ASM file into a list
;; Note: need to set (repl) directory to where this file is before calling.
(defun read-file (filename)
  (with-open-file
      (stream (uiop:parse-unix-namestring filename))
    (loop for line = (read-line stream nil)
          until (eq line nil)
          unless (string-equal line "")
            unless (eql (schar line 0) #\;)
              collect (read-from-string line))))

(defparameter *inst-list* '())
(defparameter *inst-curr* '())

(defun show-ihex (inst &key generated)
  (let ((word (process-instruction inst :generated generated)))
    (cond ((typep word 'integer)
           (print (encode-ihex word)))
          (t nil))))

(let* ((*inst-list*  (read-file "./programs/rom_pitch.fvl"))
       (*inst-curr* *inst-list*)
       (*curr-addr* 0))
  (setf *memory-blocks* nil)
  (setf *equ-list*
        (list '(nil nil)))
  (loop for ins in *inst-list*
        do (show-ihex ins))
  (loop until (eql 0 (mod *curr-addr* #x200))
        do (show-ihex '(nop) :generated t)))

(let* ((*inst-list*  (read-file "./programs/rom_pitch.fvl"))
       (*inst-curr* *inst-list*))
  (setf *memory-blocks* nil)
  (setf *equ-list*
        (list '(nil nil)))
  (format t "~a" (encode-c-header *inst-list*)))

(let* ((*inst-list*  (read-file "./programs/testasm.fvl"))
       (*inst-curr* *inst-list*))
  (setf *memory-blocks* nil)
  (setf *equ-list*
        (list '(nil nil)))
  (loop for ins in *inst-list*
        do (print (show-binary-instruction ins))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make serial packet
;; we pad with NOPs on the target side by default
(defun words->bytes-be (words)
  "List of 32-bit words -> list of big-endian 8-bit bytes."
  (loop for word in words
        if (typep word 'integer)
        nconc
        (loop for pos in '(3 2 1 0)
              collect (ldb (byte 8 (* pos 8)) word))))

(defun string->bytes (string)
  "Assumes we only uses chars <127."
  (loop for char across string
        collect (char-code char)))

(defun u16->bytes-be (number)
  (loop for pos in '(1 0)
        collect (ldb (byte 8 (* pos 8)) number)))

(defun crc (data)
  "Calculate basic CRC-8 for the given data. Not implemented yet."
  (declare (ignore data))
  '(0))

(defun make-packet (op data)
  (append
   (string->bytes "FV1")
   (list op)
   (crc data)
   (u16->bytes-be (+ 1 (length data)))
   '(7)                                 ; destination program slot
   data))

;; Data to be encapsulated in serial packet:
(defun assemble (asm-file-path)
  (let* ((*inst-list*  (read-file asm-file-path))
         (*inst-curr* *inst-list*))
    (setf *memory-blocks* nil)
    (setf *equ-list*
          (list '(nil nil)))

     (words->bytes-be
      (process-instructions *inst-list*))))

(defparameter *packet*
  (make-packet #x00 (assemble "./programs/rom_pitch.fvl")))

(defun format-pot-values (vals)
  (loop for val in vals
        nconc (u16->bytes-be val)))

;; Output for usage with xxd
;; (format t "~{~2,'0X~}~%" *packet*)
;; (format t "~{~2,'0X ~}~%" *packet*)

;; Send over UART
(ql:quickload "cserial-port")

(defun send-serial (buffer)
  (cserial-port:with-serial (rs "/dev/ttyACM0" :baud-rate 115200 :data-bits 8 :stop-bits 1 :parity :none)
    (loop for byte in buffer
          do (cserial-port:write-serial-byte byte rs))))

(send-serial (make-packet #x00 (assemble "./programs/soft-dist.fvl")))
(send-serial (make-packet #x00 (assemble "./programs/rom_pitch.fvl")))

(send-serial (make-packet #x01 (format-pot-values '(100 500 1000))))
(send-serial (make-packet #x01 (format-pot-values '(0 0 0))))

