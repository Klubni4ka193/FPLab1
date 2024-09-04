// Определение начальных и конечных значений, а также параметров
let initialX = 0.0
let finalX = 1.0
let epsilon = 0.00001
let stepSize = 0.1

// Факториал числа с использованием вспомогательной функции
let rec computeFactorial n =
    let rec factorialAux result num =
        if num <= 1 then result
        else factorialAux (result * num) (num - 1)
    factorialAux 1 n

// подход к разложению в ряд Тейлора
let naiveTaylorExpansion x epsilon =
    let rec taylorTermCalculation n accumulator =
        let coefficient = if n % 2 = 0 then -1.0 else 1.0
        let factor = 2.0 ** (float(2 * n - 1))
        let denom = float (computeFactorial (2 * n))
        let termValue = coefficient * factor * (x ** (float (2 * n))) / denom
        let updatedAcc = accumulator + termValue
        if abs (updatedAcc - accumulator) <= epsilon then (updatedAcc, n)
        else taylorTermCalculation (n + 1) updatedAcc
    taylorTermCalculation 1 0.0

// Оптимизированный подход к разложению в ряд Тейлора
let optimizedTaylorExpansion x epsilon =
    let rec smartTermCalc n prevTerm accum =
        let currentTerm = -1.0 * prevTerm * 2.0 ** 2.0 * x ** 2.0 / ((float (2 * n) - 1.0) * float (2 * n))
        let newAccum = accum + currentTerm
        if abs (newAccum - accum) <= epsilon then (newAccum, n)
        else smartTermCalc (n + 1) currentTerm newAccum

    let initialTerm = 2.0 * x ** 2.0 / float (computeFactorial 2)
    smartTermCalc 2 initialTerm initialTerm

// Вывод таблицы с результатами
let displayResults start end epsilon =
    printfn " x |     f(x)     | Naive Series | Terms # | Optimized Series | Terms # "
    printfn "===|==============|==============|=========|==================|========="

    let rec outputRow currentX =
        if currentX >= end then ()
        else
            let fx = sin currentX ** 2.0
            let (naiveResult, naiveTermsCount) = naiveTaylorExpansion currentX epsilon
            let (optimizedResult, optimizedTermsCount) = optimizedTaylorExpansion currentX epsilon
            printfn "%2.1f| %1.10f | %1.10f | %6d | %1.10f | %6d " currentX fx naiveResult naiveTermsCount optimizedResult optimizedTermsCount
            outputRow (currentX + stepSize)

    outputRow start

    printfn "-----------------------------------------------------------------------"

displayResults initialX finalX epsilon
