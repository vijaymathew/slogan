;;;============================================================================

;;; File: "help.scm"

;;; Copyright (c) 2011-2014 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(##namespace ("help#"))

(##include "~~lib/gambit#.scm")

(##namespace ("gr#" show-help-document))

(declare
  (standard-bindings)
  (extended-bindings)
  (block)
  (fixnum)
  (not safe)
)

;;;============================================================================

;; Access to online help.

(define main-help-document     (path-expand "~~/help.html"))
(define r5rs-help-document     (path-expand "~~/r5rs.html"))
(define gambit-help-document (path-expand "~~/gambit.html"))

(define help-names-r5rs '(
*
+
-
/
<
<=
=
>
>=
abs
acos
and
angle
append
apply
asin
assoc
assq
assv
atan
begin
boolean?
caar
cadr
call-with-current-continuation
call-with-input-file
call-with-output-file
call-with-values
car
case
cdddar
cddddr
cdr
ceiling
char->integer
char-alphabetic?
char-ci<=?
char-ci<?
char-ci=?
char-ci>=?
char-ci>?
char-downcase
char-lower-case?
char-numeric?
char-ready?
char-upcase
char-upper-case?
char-whitespace?
char<=?
char<?
char=?
char>=?
char>?
char?
close-input-port
close-output-port
complex?
cond
cons
cos
current-input-port
current-output-port
delay
denominator
display
do
dynamic-wind
eof-object?
eq?
equal?
eqv?
eval
even?
exact->inexact
exact?
exp
expt
floor
for-each
force
gcd
if
imag-part
inexact->exact
inexact?
input-port?
integer->char
integer?
interaction-environment
lambda
lcm
length
let
let*
let-syntax
letrec
letrec-syntax
list
list->string
list->vector
list-ref
list-tail
list?
load
log
magnitude
make-polar
make-rectangular
make-string
make-vector
map
max
member
memq
memv
min
modulo
negative?
newline
not
null-environment
null?
number->string
number?
numerator
odd?
open-input-file
open-output-file
or
output-port?
pair?
peek-char
positive?
procedure?
quasiquote
quote
quotient
rational?
rationalize
read
read-char
real-part
real?
remainder
reverse
round
scheme-report-environment
set!
set-car!
set-cdr!
sin
sqrt
string
string->list
string->number
string->symbol
string-append
string-ci<=?
string-ci<?
string-ci=?
string-ci>=?
string-ci>?
string-copy
string-fill!
string-length
string-ref
string-set!
string<=?
string<?
string=?
string>=?
string>?
string?
substring
symbol->string
symbol?
syntax-rules
tan
transcript-off
transcript-on
truncate
values
vector
vector->list
vector-fill!
vector-length
vector-ref
vector-set!
vector?
with-input-from-file
with-output-to-file
write
write-char
zero?
))

(define help-names-gambit '(
<
<=
=
>
>=
abandoned-mutex-exception?
abort
address-info-family
address-info-protocol
address-info-socket-info
address-info-socket-type
address-info?
address-infos
all-bits-set?
any-bits-set?
append-f32vectors
append-f64vectors
append-s16vectors
append-s32vectors
append-s64vectors
append-s8vectors
append-strings
append-u16vectors
append-u32vectors
append-u64vectors
append-u8vectors
append-vectors
arithmetic-shift
bit-count
bit-set?
bitwise-and
bitwise-ior
bitwise-merge
bitwise-not
bitwise-xor
box
box?
break
c-declare
c-define
c-define-type
c-initialize
c-lambda
call-with-current-continuation
call-with-input-file
call-with-input-process
call-with-input-string
call-with-input-u8vector
call-with-input-vector
call-with-output-file
call-with-output-process
call-with-output-string
call-with-output-u8vector
call-with-output-vector
call/cc
cfun-conversion-exception-arguments
cfun-conversion-exception-code
cfun-conversion-exception-message
cfun-conversion-exception-procedure
cfun-conversion-exception?
char->integer
char-ci<=?
char-ci<?
char-ci=?
char-ci>=?
char-ci>?
char<=?
char<?
char=?
char>=?
char>?
clear-bit-field
close-input-port
close-output-port
close-port
command-line
compile-file
compile-file-to-c
cond-expand
condition-variable-broadcast!
condition-variable-name
condition-variable-signal!
condition-variable-specific
condition-variable-specific-set!
condition-variable?
configure-command-string
console-port
continuation-capture
continuation-graft
continuation-return
continuation?
copy-bit-field
copy-file
cpu-time
create-directory
create-fifo
create-link
create-symbolic-link
current-directory
current-error-port
current-exception-handler
current-input-port
current-output-port
current-readtable
current-thread
current-time
current-user-interrupt-handler
datum-parsing-exception-kind
datum-parsing-exception-parameters
datum-parsing-exception-readenv
datum-parsing-exception?
deadlock-exception?
declare
default-random-source
defer-user-interrupts
define
define-cond-expand-feature
define-macro
define-record-type
define-structure
define-syntax
define-type
define-type-of-thread
delete-directory
delete-file
directory-files
display-continuation-backtrace
display-continuation-dynamic-environment
display-continuation-environment
display-dynamic-environment?
display-environment-set!
display-exception
display-exception-in-context
display-procedure-environment
divide-by-zero-exception-arguments
divide-by-zero-exception-procedure
divide-by-zero-exception?
eq?-hash
equal?-hash
eqv?-hash
err-code->string
error
error-exception-message
error-exception-parameters
error-exception?
eval
exit
expression-parsing-exception-kind
expression-parsing-exception-parameters
expression-parsing-exception-source
expression-parsing-exception?
extract-bit-field
f32vector
f32vector->list
f32vector-append
f32vector-copy
f32vector-fill!
f32vector-length
f32vector-ref
f32vector-set!
f32vector-shrink!
f32vector?
f64vector
f64vector->list
f64vector-append
f64vector-copy
f64vector-fill!
f64vector-length
f64vector-ref
f64vector-set!
f64vector-shrink!
f64vector?
file-attributes
file-creation-time
file-device
file-exists?
file-group
file-info
file-info-attributes
file-info-creation-time
file-info-device
file-info-group
file-info-inode
file-info-last-access-time
file-info-last-change-time
file-info-last-modification-time
file-info-mode
file-info-number-of-links
file-info-owner
file-info-size
file-info-type
file-info?
file-inode
file-last-access-time
file-last-change-time
file-last-modification-time
file-mode
file-number-of-links
file-owner
file-size
file-type
finite?
first-bit-set
fixnum->flonum
fixnum-overflow-exception-arguments
fixnum-overflow-exception-procedure
fixnum-overflow-exception?
fixnum?
fl*
fl+
fl-
fl/
fl<
fl<=
fl=
fl>
fl>=
flabs
flacos
flasin
flatan
flceiling
flcos
fldenominator
fleven?
flexp
flexpt
flfinite?
flfloor
flinfinite?
flinteger?
fllog
flmax
flmin
flnan?
flnegative?
flnumerator
flodd?
flonum?
flpositive?
flround
flsin
flsqrt
fltan
fltruncate
flzero?
force-output
foreign-address
foreign-release!
foreign-released?
foreign-tags
foreign?
future
fx*
fx+
fx-
fx<
fx<=
fx=
fx>
fx>=
fxabs
fxand
fxarithmetic-shift
fxarithmetic-shift-left
fxarithmetic-shift-right
fxbit-count
fxbit-set?
fxeven?
fxfirst-bit-set
fxif
fxior
fxlength
fxmax
fxmin
fxmodulo
fxnegative?
fxnot
fxodd?
fxpositive?
fxquotient
fxremainder
fxwrap*
fxwrap+
fxwrap-
fxwrapabs
fxwraparithmetic-shift
fxwraparithmetic-shift-left
fxwraplogical-shift-right
fxwrapquotient
fxxor
fxzero?
gc-report-set!
generate-proper-tail-calls
gensym
get-output-string
get-output-u8vector
get-output-vector
getenv
group-info
group-info-gid
group-info-members
group-info-name
group-info?
heap-overflow-exception?
help
help-browser
host-info
host-info-addresses
host-info-aliases
host-info-name
host-info?
host-name
improper-length-list-exception-arg-num
improper-length-list-exception-arguments
improper-length-list-exception-procedure
improper-length-list-exception?
inactive-thread-exception-arguments
inactive-thread-exception-procedure
inactive-thread-exception?
include
infinite?
initialized-thread-exception-arguments
initialized-thread-exception-procedure
initialized-thread-exception?
input-port-byte-position
input-port-bytes-buffered
input-port-char-position
input-port-characters-buffered
input-port-column
input-port-line
input-port-readtable
input-port-readtable-set!
input-port-timeout-set!
input-port?
integer->char
integer-length
integer-nth-root
integer-sqrt
invalid-hash-number-exception-arguments
invalid-hash-number-exception-procedure
invalid-hash-number-exception?
join-timeout-exception-arguments
join-timeout-exception-procedure
join-timeout-exception?
keyword->string
keyword-expected-exception-arguments
keyword-expected-exception-procedure
keyword-expected-exception?
keyword-hash
keyword?
lambda
link-flat
link-incremental
list->f32vector
list->f64vector
list->s16vector
list->s32vector
list->s64vector
list->s8vector
list->table
list->u16vector
list->u32vector
list->u64vector
list->u8vector
mailbox-receive-timeout-exception-arguments
mailbox-receive-timeout-exception-procedure
mailbox-receive-timeout-exception?
main
make-condition-variable
make-f32vector
make-f64vector
make-mutex
make-parameter
make-random-source
make-root-thread
make-s16vector
make-s32vector
make-s64vector
make-s8vector
make-table
make-thread
make-thread-group
make-u16vector
make-u32vector
make-u64vector
make-u8vector
make-uninterned-keyword
make-uninterned-symbol
make-will
multiple-c-return-exception?
mutex-lock!
mutex-name
mutex-specific
mutex-specific-set!
mutex-state
mutex-unlock!
mutex?
namespace
nan?
network-info
network-info-aliases
network-info-name
network-info-number
network-info?
newline
no-such-file-or-directory-exception-arguments
no-such-file-or-directory-exception-procedure
no-such-file-or-directory-exception?
noncontinuable-exception-reason
noncontinuable-exception?
nonempty-input-port-character-buffer-exception-arguments
nonempty-input-port-character-buffer-exception-procedure
nonempty-input-port-character-buffer-exception?
nonprocedure-operator-exception-arguments
nonprocedure-operator-exception-code
nonprocedure-operator-exception-operator
nonprocedure-operator-exception-rte
nonprocedure-operator-exception?
number-of-arguments-limit-exception-arguments
number-of-arguments-limit-exception-procedure
number-of-arguments-limit-exception?
object->serial-number
object->string
object->u8vector
open-directory
open-dummy
open-event-queue
open-file
open-input-file
open-input-process
open-input-string
open-input-u8vector
open-input-vector
open-output-file
open-output-process
open-output-string
open-output-u8vector
open-output-vector
open-process
open-string
open-string-pipe
open-tcp-client
open-tcp-server
open-u8vector
open-u8vector-pipe
open-vector
open-vector-pipe
os-exception-arguments
os-exception-code
os-exception-message
os-exception-procedure
os-exception?
output-port-byte-position
output-port-char-position
output-port-column
output-port-line
output-port-readtable
output-port-readtable-set!
output-port-timeout-set!
output-port-width
output-port?
parameterize
path-directory
path-expand
path-extension
path-normalize
path-strip-directory
path-strip-extension
path-strip-trailing-directory-separator
path-strip-volume
path-volume
peek-char
port-settings-set!
port?
pp
pretty-print
primordial-exception-handler
print
println
process-pid
process-status
process-times
protocol-info
protocol-info-aliases
protocol-info-name
protocol-info-number
protocol-info?
raise
random-f64vector
random-integer
random-real
random-source-make-f64vectors
random-source-make-integers
random-source-make-reals
random-source-make-u8vectors
random-source-pseudo-randomize!
random-source-randomize!
random-source-state-ref
random-source-state-set!
random-source?
random-u8vector
range-exception-arg-num
range-exception-arguments
range-exception-procedure
range-exception?
read
read-all
read-char
read-line
read-substring
read-subu8vector
read-u8
readtable-case-conversion?
readtable-case-conversion?-set
readtable-eval-allowed?
readtable-eval-allowed?-set
readtable-keywords-allowed?
readtable-keywords-allowed?-set
readtable-max-unescaped-char
readtable-max-unescaped-char-set
readtable-max-write-length
readtable-max-write-length-set
readtable-max-write-level
readtable-max-write-level-set
readtable-sharing-allowed?
readtable-sharing-allowed?-set
readtable-start-syntax
readtable-start-syntax-set
readtable-write-cdr-read-macros?
readtable-write-cdr-read-macros?-set
readtable-write-extended-read-macros?
readtable-write-extended-read-macros?-set
readtable?
real-time
receive
rename-file
repl-display-environment?
repl-input-port
repl-output-port
repl-result-history-max-length-set!
repl-result-history-ref
replace-bit-field
rpc-remote-error-exception-arguments
rpc-remote-error-exception-message
rpc-remote-error-exception-procedure
rpc-remote-error-exception?
s16vector
s16vector->list
s16vector-append
s16vector-copy
s16vector-fill!
s16vector-length
s16vector-ref
s16vector-set!
s16vector-shrink!
s16vector?
s32vector
s32vector->list
s32vector-append
s32vector-copy
s32vector-fill!
s32vector-length
s32vector-ref
s32vector-set!
s32vector-shrink!
s32vector?
s64vector
s64vector->list
s64vector-append
s64vector-copy
s64vector-fill!
s64vector-length
s64vector-ref
s64vector-set!
s64vector-shrink!
s64vector?
s8vector
s8vector->list
s8vector-append
s8vector-copy
s8vector-fill!
s8vector-length
s8vector-ref
s8vector-set!
s8vector-shrink!
s8vector?
scheduler-exception-reason
scheduler-exception?
seconds->time
serial-number->object
service-info
service-info-aliases
service-info-name
service-info-port-number
service-info-protocol
service-info?
set-box!
setenv
sfun-conversion-exception-arguments
sfun-conversion-exception-code
sfun-conversion-exception-message
sfun-conversion-exception-procedure
sfun-conversion-exception?
shell-command
six.!
six.!x
six.&x
six.*x
six.++x
six.+x
six.--x
six.-x
six.arrow
six.break
six.call
six.case
six.clause
six.compound
six.cons
six.continue
six.define-procedure
six.define-variable
six.do-while
six.dot
six.for
six.goto
six.identifier
six.if
six.index
six.label
six.list
six.literal
six.make-array
six.new
six.null
six.prefix
six.procedure
six.procedure-body
six.return
six.switch
six.while
six.x!=y
six.x%=y
six.x%y
six.x&&y
six.x&=y
six.x&y
six.x*=y
six.x*y
six.x++
six.x+=y
six.x+y
six.x--
six.x-=y
six.x-y
six.x/=y
six.x/y
six.x:-y
six.x:=y
six.x:y
six.x<<=y
six.x<<y
six.x<=y
six.x<y
six.x==y
six.x=y
six.x>=y
six.x>>=y
six.x>>y
six.x>y
six.x?y:z
six.x^=y
six.x^y
six.~x
socket-info-address
socket-info-family
socket-info-port-number
socket-info?
stack-overflow-exception?
started-thread-exception-arguments
started-thread-exception-procedure
started-thread-exception?
step
step-level-set!
string->keyword
string-ci<=?
string-ci<?
string-ci=?
string-ci=?-hash
string-ci>=?
string-ci>?
string-shrink!
string<=?
string<?
string=?
string=?-hash
string>=?
string>?
subf32vector
subf32vector-fill!
subf32vector-move!
subf64vector
subf64vector-fill!
subf64vector-move!
subs16vector
subs16vector-fill!
subs16vector-move!
subs32vector
subs32vector-fill!
subs32vector-move!
subs64vector
subs64vector-fill!
subs64vector-move!
subs8vector
subs8vector-fill!
subs8vector-move!
substring-fill!
substring-move!
subu16vector
subu16vector-fill!
subu16vector-move!
subu32vector
subu32vector-fill!
subu32vector-move!
subu64vector
subu64vector-fill!
subu64vector-move!
subu8vector
subu8vector-fill!
subu8vector-move!
subvector
subvector-fill!
subvector-move!
symbol-hash
system-stamp
system-type
system-type-string
system-version
system-version-string
table->list
table-copy
table-for-each
table-length
table-merge
table-merge!
table-ref
table-search
table-set!
table?
tcp-client-peer-socket-info
tcp-client-self-socket-info
tcp-server-socket-info
tcp-service-register!
tcp-service-unregister!
terminated-thread-exception-arguments
terminated-thread-exception-procedure
terminated-thread-exception?
test-bit-field?
this-source-file
thread-base-priority
thread-base-priority-set!
thread-group->thread-group-list
thread-group->thread-group-vector
thread-group->thread-list
thread-group->thread-vector
thread-group-name
thread-group-parent
thread-group-resume!
thread-group-suspend!
thread-group-terminate!
thread-group?
thread-init!
thread-interrupt!
thread-join!
thread-mailbox-extract-and-rewind
thread-mailbox-next
thread-mailbox-rewind
thread-name
thread-priority-boost
thread-priority-boost-set!
thread-quantum
thread-quantum-set!
thread-receive
thread-resume!
thread-send
thread-sleep!
thread-specific
thread-specific-set!
thread-start!
thread-state
thread-state-abnormally-terminated-reason
thread-state-abnormally-terminated?
thread-state-active-timeout
thread-state-active-waiting-for
thread-state-active?
thread-state-initialized?
thread-state-normally-terminated-result
thread-state-normally-terminated?
thread-state-uninitialized?
thread-suspend!
thread-terminate!
thread-thread-group
thread-yield!
thread?
time
time->seconds
time?
timeout->time
top
touch
trace
transcript-off
transcript-on
tty-history
tty-history-max-length-set!
tty-history-set!
tty-mode-set!
tty-paren-balance-duration-set!
tty-text-attributes-set!
tty-type-set!
tty?
type-exception-arg-num
type-exception-arguments
type-exception-procedure
type-exception-type-id
type-exception?
u16vector
u16vector->list
u16vector-append
u16vector-copy
u16vector-fill!
u16vector-length
u16vector-ref
u16vector-set!
u16vector-shrink!
u16vector?
u32vector
u32vector->list
u32vector-append
u32vector-copy
u32vector-fill!
u32vector-length
u32vector-ref
u32vector-set!
u32vector-shrink!
u32vector?
u64vector
u64vector->list
u64vector-append
u64vector-copy
u64vector-fill!
u64vector-length
u64vector-ref
u64vector-set!
u64vector-shrink!
u64vector?
u8vector
u8vector->list
u8vector->object
u8vector-append
u8vector-copy
u8vector-fill!
u8vector-length
u8vector-ref
u8vector-set!
u8vector-shrink!
u8vector?
unbound-global-exception-code
unbound-global-exception-rte
unbound-global-exception-variable
unbound-global-exception?
unbound-os-environment-variable-exception-arguments
unbound-os-environment-variable-exception-procedure
unbound-os-environment-variable-exception?
unbound-serial-number-exception-arguments
unbound-serial-number-exception-procedure
unbound-serial-number-exception?
unbound-table-key-exception-arguments
unbound-table-key-exception-procedure
unbound-table-key-exception?
unbox
unbreak
uncaught-exception-arguments
uncaught-exception-procedure
uncaught-exception-reason
uncaught-exception?
uninitialized-thread-exception-arguments
uninitialized-thread-exception-procedure
uninitialized-thread-exception?
uninterned-keyword?
uninterned-symbol?
unknown-keyword-argument-exception-arguments
unknown-keyword-argument-exception-procedure
unknown-keyword-argument-exception?
unterminated-process-exception-arguments
unterminated-process-exception-procedure
unterminated-process-exception?
untrace
user-info
user-info-gid
user-info-home
user-info-name
user-info-shell
user-info-uid
user-info?
user-name
vector-append
vector-copy
vector-shrink!
void
will-execute!
will-testator
will?
with-exception-catcher
with-exception-handler
with-input-from-file
with-input-from-port
with-input-from-process
with-input-from-string
with-input-from-u8vector
with-input-from-vector
with-output-to-file
with-output-to-port
with-output-to-process
with-output-to-string
with-output-to-u8vector
with-output-to-vector
write
write-char
write-substring
write-subu8vector
write-u8
wrong-number-of-arguments-exception-arguments
wrong-number-of-arguments-exception-procedure
wrong-number-of-arguments-exception?
|six.x,y|
|six.x\|=y|
|six.x\|\|y|
|six.x\|y|
))

(define (gambit-repl-help subject)
  (let* ((name
          (cond ((procedure? subject)
                 (##procedure-name subject))
                (else
                 subject)))
         (docu
          (cond ((memq name help-names-gambit)
                 gambit-help-document)
                ((memq name help-names-r5rs)
                 r5rs-help-document)
                (else
                 #f))))
    (if (not docu)
        (error "No documentation for" name)
        (let* ((prefix
                "Definition of ")
               (anchor
                (##escape-link (string-append prefix (##object->string name)))))
          (show-help-document docu anchor)))))

(set! ##help-hook gambit-repl-help)

;;;============================================================================
