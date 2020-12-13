# How to use

Simply run the application as a dotnet 3.0 app. Takes one argument: `<path to input>`

Calculating the two star example output would be
```
$ dotnet run ./exampleinput.txt
Day 8 - Handheld Halting (**)
Using path: ./exampleinput.txt
Halting on instruction acc +1 @ Instruction Pointer 1.
Reason: Already visited this instruction before on cycle: [2]
--- Handheld Console ---
Instruction Pointer: 1
Accumulator: 5
Instructions: 9
Cycles Simulated: 8
Final value of accumulator before infinite loop (*): 5


Halting! Reached end of memory map
Halting on instruction acc +1 @ Instruction Pointer 6.
Reason: Already visited this instruction before on cycle: [4]
Halting on instruction acc +1 @ Instruction Pointer 1.
Reason: Already visited this instruction before on cycle: [2]
Halting on instruction nop +0 @ Instruction Pointer 0.
Reason: Already visited this instruction before on cycle: [1]
Correct console after permuting over possible programs
--- Handheld Console ---
Instruction Pointer: 9
Accumulator: 8
Instructions: 9
Cycles Simulated: 7
Final value of accumulator for adjusted program (**): 8
```

