# How to use

Simply run the application as a .NET 5.0 app. Takes one argument: `<path to input> <variant>`

Calculating the one star example output would be
```
$ dotnet run ./exampleinput.txt 1
Day 11 - Seating System (*)
Variant: OneStar
Path: ./exampleinput.txt
Last stable grid
#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##
Number of occupied seats: 37
```

Calculating the two star example output would be
```powershell
$ dotnet run ./exampleinput.txt 2
Path: ./exampleinput.txt
Variant: TwoStar
Last stable grid
#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#
Number of occupied seats: 26
```