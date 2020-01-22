### What level of detail on the register sets?

Know what registers you have (rax-rdx, rsi, rdi, pc, rsp etc) in the various registers. Know which are volatile and have to save or not.

### In the delay slot, what registers are used, this window or the next?

Params passed in 10-15 and they appear in 26-31 in the next window. Similarly, the registers used in the delay slot are from the current register window.

### But the current window pointer is the first thing to be changed?

The call essentially works on a secondary clock period independent of the normal instruction FDE clock.

### How would you do the code conversion questions (2015 Q1)?

The first part asks you to compare RISC-I to IA32, so you'll have to describe the registers needed in each and the registers used for each, describing the procedure (push registers, save return address, save frame pointer, save registers you're going to use, then unwind after). You also need to know which registers are volatile so you don't waste time saving them.

You then do the same for the RISC-I

This gives you some sort of a picture of what to implement for the next part, retrieving the two parameters from wherever they've been stored and executing the while loop.

### How RISC-I where is the return address stored?

Not too fussed. `R25` agreed on in the tutorial. `R15` is convention. Has to use a register that is allocated on a precedure call.

#### Agreed

- Return address: R25
- Return results: Global registers

### What's the different between capacity and conflict misses?

- Work out compulsory misses in a fully *associative* cache
- Additional misses are *capacity* misses
- In a non-fully associative cache, all additional misses are *conflict* misses

### Do you need to allocate shadow space?

- If its the code that doesn't specify it's windows, explain and can leave it out.
