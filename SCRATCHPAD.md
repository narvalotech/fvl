# Scratchpad

A place for sketching up ideas.

Function blocks
---------------
block definition:
- name/class
- shortdoc
- long documentation
- number of inputs
- number of outputs
- paramlist
  - name
  - description
  - range
- program
  - s-exp of procedure

block instance: 
- name/class
- patch id
- plist of inputs
- plist of params
  - value (if const)
  - connection id if variable
  
patch is just a list of block instances
static blocks (lfo, lin/rin etc) get IDs 9xx

FV-1 backend will only support 2 inputs + 2 outputs, and only linear patches (ie no sidechain).

representation of connections:
on each block input:
(fx-instance out-channel-id)
e.g. `:inputs (:0 (block-000 1) :1 (block-002 0)`

to build compute chain order:
- scan all blocks
- build list of dependents per-block
  e.g. block C and D need block A

patch example:
in -> gain [000] -> filter [001] -> out
``` common-lisp
(
  ('adc #900
   :inputs nil
   :params nil)
  ('gain #000
   :inputs (:0 (*adc* 0) :1 (*adc* 1))
   :params (:amount 100))
  ('filter #001
   :inputs (:0 (#000 0) :1 (#000 1))
   :params (:freq (*pot* 0) :q 20 :gain 10))
  ('dac #999 
   :inputs (:0 (#001 0) :1 (#001 1))
   :params nil)
)
```

block example:
``` common-lisp
(defblock filter 2 2 
    ((freq uint (0 20000))
    (q uint (1 40))
    (gain int (-10 10)))

    "Short docstring"

    "Long docstring"

    '(
    (mem ldel 4096)
    (mem rdel 4096)
    (mem dtemp 1)

    (equ potfil reg0)

    (skp run loop)
    (wldr 0 0 4096)

    (label loop)

    (rdax adcl (block-param 'q))
    (wra ldel (block-param 'gain))
    (rdax adcr 1.0)
    (wra rdel 0.0)
    )
)
```

where to store (and load) stuff in-between blocks:
-> reg15 - reg31
- R15 - LIN
- R16 - RIN
- R17 - LOUT
- R18 - ROUT

POTs can be read from any block.
We could imagine having 'virtual pots', where the value is read by the host MCU and the program is altered in real-time with its value. 
Since the host MCU will have the program sitting in RAM, it should be pretty straightforward.

e.g.
- assembly is always compiled with extra instructions that load a value in R18+ at program start
- 4th pot moves
- value is read and stored by host MCU
- MCU rewrites 1st extra instruction with read value
- FV1 program loops back to start
- next users of 'virtual pot' registers (R18+) see updated value

LFOs not abstracted for now (due to special opcodes).
-> only one block can use them due to init sequence.

The `defblock` macro does some rewriting:
`equ`, `mem` and `label` names are appended the block id
e.g. `(mem ldel 4096)` -> `(mem ldel:001 4096)`

param format:
`(name type (min max))`
only type allowed for now is (u)int
