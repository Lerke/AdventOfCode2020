# How to use

Simply run the application as a dotnet 3.0 app. Takes three arguments: `<path to input> <initial x direction> <initial y direction> [<initial x direction> <initial y direction>]*`

Calculating the two star example output would be
```
$ dotnet run ./exampleinput.txt 1 1 3 1 5 1 7 1 1 2
Run 0 (1, 1) - Trees hit: 2
Run 1 (3, 1) - Trees hit: 7
Run 2 (5, 1) - Trees hit: 3
Run 3 (7, 1) - Trees hit: 4
Run 4 (1, 2) - Trees hit: 2
Multiplied together, that's 336 trees! (Two star output)
```

