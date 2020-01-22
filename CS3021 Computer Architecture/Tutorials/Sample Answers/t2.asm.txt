option casemap:none                         ; case sensitive

;
; t2.asm
;
; Copyright (C) 2012 - 2017 jones@scss.tcd.ie
;

;
; VC++ uses the following code generation strategy to pass parameters to
; functions
;
; the first 4 parameters are passed in ecx, edx, r8 and r9
; additional parameters are pushed on the stack right to left
;
; non leaf functions must allocate space on the stack so that any function it
; calls has the option of spilling its 4 register parameters onto the stack
; this space is call home or shadow space
; 
; as a further optimisation, the compiler determines, within a function, the
; maximum number of parameters passed to any function it calls this amount
; of space is allocated on the stack at function entry and deallocated at exit
; the first 4 parameters are passed in the registers, but any additional
; parameters are stored directly in the correct location in this home space
;

includelib legacy_stdio_definitions.lib

extrn  printf:near

;
; global variables
;
.data

;
;   _int64 g = 4;
;
public  g                   ; make sure function name is exported

g       QWORD   4           ; global variable g initialised to 4

;
; code
; 
.code

;   _int64 min(int a, int b, int c) {
;       _int64 v = a;
;       if (b < v)
;           v = b;
;       if (c < v)
;           v = c;
;       return v;
;   }

; parameter a   rcx
; parameter b   rdx
; parameter c   r8

public  min                     ; export function name

min:    mov     rax, rcx        ; v = a
        cmp     rdx, rax        ; if (b < v)
        jge     min0            ;
        mov     rax, rdx        ; v = b
min0:   cmp     r8, rax         ; if (c < v)
        jge     min1            ;
        mov     rax, r8         ; v = c
min1:   ret                     ; return v

;   int p(_int64 i, _int64 j, _int64 k, _int64 l) {
;       return min(min(g, i, j), k, l);
;   }

; parameter i   rcx
; parameter j   rdx
; parameter k   r8
; parameter l   r9
;
; assume min needs shadow space
; assume that min may change any of the volatile registers
; means that r8 and r9 may not survive first call to min
; save r8 and r9 in shadow space (shows why shadow space is useful)

public  p                       ; export function name

p:      sub     rsp, 32         ; allocate shadow space once for class to min
        mov     [rsp+64], r9    ; save r9 in our shadow space
        mov     [rsp+56], r8    ; save r8 in our shadow space
        mov     r8, rdx         ; j
        mov     rdx, rcx        ; i
        mov     rcx, [g]        ; g
        call    min             ; min(g, i, j)
        mov     rcx, rax        ; min(g, i, j)
        mov     rdx, [rsp+56]   ; k
        mov     r8, [rsp+64]    ; l
        call    min             ; min(min(g, i, j), k, l)
        add     rsp, 32         ; deallocate shadow space
        ret                     ; return

;   _int64 gcd(_int64 a, _int64 b) {
;       if (b == 0) {
;           return a;
;       } else {
;           return gcd(b, a % b);
;       }
;   }
;
;
;   parameter a rcx
;   parameter b rdx
; 
;   treat as a leaf function - no shadow space allocated

public      gcd                     ; export function name

gcd:        test    rdx, rdx        ; if (b == 0)
            jne     gcd0            ;
            mov     eax, ecx        ; return a
            jmp     gcd1            ;
gcd0:       mov     rax, rcx        ; rax = a
            mov     rcx, rdx        ; rcx = b
            cqo                     ; sign extend a
            idiv    rcx             ; rdx = a % b
            call    gcd             ; gcd(b, a % b)
gcd1:       ret                     ; return        

;   _int64 q(_int64 a, _int64 b, _int64 c, _int64 d, _int64 e) {
;       int sum = a + b + c + d + e;
;       printf("a = %I64d b = %I64d c = %I64d d = %I64d e = %I64d sum = %I64d\n", a, b, c, d, e, sum);
;       return sum;
;   }

; parameter a   rcx
; parameter b   rdx
; parameter c   r8
; parameter d   r9
; parameter e   [rsp+8]
;
; must assume printf needs shadow space
; must assume that printf may change any of the volatile registers and overwrite any of its parameters passed on the stack
; non volatile registers: ebp, rbx, r12, r13, r14 and r15 

fs0     db      "a = %I64d b = %I64d c = %I64d d = %I64d e = %I64d sum = %I64d", 0AH, 00H

public  q                       ; make sure function name is exported

q:      sub     rsp, 64         ; allocate shadow space for 7 parameters + pushed sum
        lea     rax, [rcx+rdx]  ; rax = a + b {rbx across call to printf}
        add     rax, r8         ; rax = a + b + c
        add     rax, r9         ; rax = a + b + c + d
        mov     r10, [rsp+104]  ; r10 = e {NB: offset for e is 104}
        add     rax, r10        ; rax = a + b + c + d + e 
        mov     [rsp+56], rax   ; save sum
        mov     [rsp+48], rax   ; store printf param 7 in [rsp+48] {sum}
        mov     [rsp+40], r10   ; store printf param 6 in [rsp+40] {e}
        mov     [rsp+32], r9    ; store printf param 5 in [rsp+32] {d}
        mov     r9, r8          ; store printf param 4 in r9 {c}
        mov     r8, rdx         ; store printf param 3 in r8 {b}
        mov     rdx, rcx        ; store printf param 2 in rdx {a}
        lea     rcx, fs0        ; store printf param 1 in rcx {&fs0}
        call    printf          ; call printf
        add     rsp, 56         ; deallocate shadow space
        pop     rax             ; pop sum {a+b+c+d+e}
        ret                     ; return

;
;   show that shadow space needs to be allocated
;
;   if statements to allocate/deallocate shadow space are commente out a runtime exception is geneerated
;
;   void qns() {
;       printf("qns");
;   }
;

fs1     db      "qns", 0AH, 00H

public  qns                     ; export function name

qns:    lea     rcx, fs1        ;   
;       sub     rsp, 32         ; allocate shadow space
        call    printf          ; call printf
;       add     rsp, 32         ; deallocate shadow space
        xor     rax, rax        ; rteurn 0
        ret                     ; return

        end
