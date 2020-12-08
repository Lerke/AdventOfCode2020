# How to use

Simply run the application as a dotnet 3.0 app. Takes one argument: `<path to input>`

Calculating the one star example output would be
```
$ dotnet run ./exampleinput.txt
Day 8 - Handheld Halting (*)
Using path: ./exampleinput.txt
Halting on instruction acc +1 @ Instruction Pointer 1.
Reason: Already visited this instruction before on cycle: [2]
Handheld Console ---
Instruction Pointer: 1
Accumulator: 5
Instructions: 9
Cycles Simulated: 7
Final value of accumulator before infinite loop (*): 5
```

