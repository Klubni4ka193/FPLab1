open System

let precision = 1e-10

// Метод деления интервала (дихотомия)
let rec findRootByBisection func lowerBound upperBound =
    let midPoint = (lowerBound + upperBound) / 2.0
    let funcAtLower = func lowerBound
    let funcAtMid = func midPoint

    if abs funcAtMid < precision then midPoint
    else if funcAtMid * funcAtLower > 0.0 then findRootByBisection func midPoint upperBound
    else findRootByBisection func lowerBound midPoint

// Метод итераций
let rec iterateUntilConvergence iterFunc initialGuess =
    let newGuess = iterFunc initialGuess
    if abs (newGuess - initialGuess) < precision then newGuess
    else iterateUntilConvergence iterFunc newGuess

// Метод Ньютона
let newtonMethod func derivFunc startGuess =
    let iterationStep x = x - (func x) / (derivFunc x)
    iterateUntilConvergence iterationStep startGuess

// Примеры функций и их производных
let complexFunction1 x = 2.0 * x * sin x - cos x
let complexFunction2 x = exp x + sqrt (1.0 + exp (2.0 * x)) - 2.0
let complexFunction3 x = log x - x + 1.8

let derivativeFunc1 x = 2.0 * sin x + 2.0 * x * cos x + sin x
let derivativeFunc2 x = exp x + exp (2.0 * x) / sqrt (1.0 + exp (2.0 * x))
let derivativeFunc3 x = 1.0 / x - 1.0

// Метод итераций для функций с использованием преобразований
let phiMap1 x = x - (2.0 * x * sin x - cos x) / (2.0 * sin x + x * cos x)
let phiMap2 x = x - (exp x + sqrt (1.0 + exp (2.0 * x)) - 2.0) / (exp x + exp (2.0 * x) / 2.0)
let phiMap3 x = x - (log x - x + 1.8) / (1.0 / x - 1.0)

// Вывод результатов
printfn "Function |   Bisection   |  Iterative   |     Newton     "
printfn "======================================================="
printfn "f1       |  %3.10f |  %3.10f |  %3.10f " (findRootByBisection complexFunction1 0.4 1.0) (iterateUntilConvergence phiMap1 0.6) (newtonMethod complexFunction1 derivativeFunc1 0.6)
printfn "f2       | %3.10f | %3.10f | %3.10f " (findRootByBisection complexFunction2 -1.0 0.0) (iterateUntilConvergence phiMap2 -0.5) (newtonMethod complexFunction2 derivativeFunc2 -0.5)
printfn "f3       |  %3.10f |  %3.10f |  %3.10f " (findRootByBisection complexFunction3 2.0 3.0) (iterateUntilConvergence phiMap3 2.5) (newtonMethod complexFunction3 derivativeFunc3 2.5)
