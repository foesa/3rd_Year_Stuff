.686                                        ; create 32 bit code
.model flat, C                              ; 32 bit memory model
option casemap:none                         ; case sensitive

 ;
 ; t1.asm
 ;
 ; Copyright (C) 2017 jones@scss.tcd.ie
 ;

.data                                       ; global variables

;
;   int g = 4;
;
public g                                    ; export variable name

g           DWORD   4                       ; global variable g initialised to 4

.code                                       ; code

;
;   int min(int a, int b, int c) {
;       int v = a;
;       if (b < v)
;           v = b;
;       if (c < v)
;           v = c;
;       return v;
;   }
;

public      min                             ; export function name

;
; "debug" version
;
;min:       push    ebp                     ; push frame pointer
;           mov     ebp, esp                ; update ebp
;           sub     esp, 4                  ; allocate space for v

;           mov     eax, [ebp+8]            ; v = a
;           mov     [ebp-4], eax

;           mov     eax, [ebp+12]           ; if (b < v)
;           cmp     eax, [ebp-4]
;           jge     min0
;           mov     eax, [ebp+12]           ; v = b
;           mov     [ebp-4], eax

;min0:      mov     eax, [ebp+16]           ; if (c < v)
;           cmp     eax, [ebp-4]
;           jge     min1
;           mov     eax, [ebp+16]           ; v = c
;           mov     [ebp-4], eax

;min1:      mov     eax, [ebp-4]            ; return v

;           mov     esp, ebp                ; restore esp
;           pop     ebp                     ; pop old frame pointer
;           ret                             ; return

;
; "release" version
;
min:        push    ebp                     ; push frame pointer
            mov     ebp, esp                ; update ebp
            mov     eax, [ebp+8]            ; v = a
            cmp     [ebp+12], eax           ; if (b < v)
            jge     min0
            mov     eax, [ebp+12]           ; v = b
min0:       cmp     [ebp+16], eax           ; if (c < v)
            jge     min1
            mov     eax, [ebp+16]           ; v = c
min1:       mov     esp, ebp                ; restore esp
            pop     ebp                     ; pop old frame pointer
            ret                             ; return
;
;   int p(int i, int j, int k, int l) {
;       return min(min(g, i, j), k, l);
;   }
;

public      p                               ; export function name

p:          push    ebp                     ; push old frame pointer
            mov     ebp, esp                ; new frame pointer
            sub     esp, 4                  ; allocate sapce for v
            push    [ebp+20]                ; push l
            push    [ebp+16]                ; push k
            push    [ebp+12]                ; v = min(g, i, j)
            push    [ebp+8]
            push    [g]
            call    min
            add     esp, 12                 ; pop parameters
            push    eax                     ; min(min(g, i, j), k, l)               
            call    min
            add     esp, 12                 ; pop parameters
            mov     esp, ebp                ; restore esp
            pop     ebp                     ; pop old frame pointer
            ret                             ; return

;
;   int gcd(int a, int b) {
;       if (b == 0) {
;           return a;
;       } else {
;           return gcd(b, a % b);
;       }
;   }
;

public      gcd                             ; make sure function name is exported

gcd:        push    ebp                     ; push old frame pointer
            mov     ebp, esp                ; new frame pointer

            cmp     DWORD PTR [ebp+12], 0   ; if (b == 0)
            jne     gcd0
            mov     eax, [ebp+8]            ; return a
            jmp     gcd1

gcd0:       mov     eax, [ebp+8]            ; return gcd(b, a % b)
            cdq                             ; sign extend
            idiv    DWORD PTR [ebp+12]      ; edx:eax / [ebp+12]
            push    edx                     ; modulus
            push    [ebp+12]
            call    gcd
            add     esp, 8                  ; pop parameters

gcd1:       mov     esp, ebp                ; restore esp
            pop     ebp                     ; pop old frame pointer
            ret                             ; return

end
