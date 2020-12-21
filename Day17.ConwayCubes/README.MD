# How to use

Simply run the application as a .NET 5.0 app. Takes one argument: `<path to input> <number of cycles>`

Calculating the two star example output would be
```powershell
$ dotnet run ./exampleinput.txt 6
Day 17 - Conway Cubes
Path: ./exampleinput.txt
Cycles: 6
Running simulation 6
Running simulation 5
Running simulation 4
Running simulation 3
Running simulation 2
Running simulation 1
After 0 cycles:
z=0 w=0
.#.
..#
###

...

[ ** ] Number of cubes left in active state after 6 cycles: 848
Simulation took 20 seconds!
```