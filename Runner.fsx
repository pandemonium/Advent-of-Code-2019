#load "Day-1.fs"
#load "Day-2.fs"

open AdventOfCode.Day2.Intcode

"day-5.txt" 
|> Loader.loadProgram 
|> Interpreter.start Executive.parameters


let loadText = Loader.parseContent >> Loader.load

loadText "1101,100,-1,4,0"
loadText "1002,4,3,4,33" 
|> Interpreter.start Executive.parameters
