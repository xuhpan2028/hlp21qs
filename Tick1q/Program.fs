open System

//------------------------write your answer function(s) here---------------------//

// top-level subfunctions of polarToCartesianApprox (if any)

/// answer to Tick1
// the header given here is correct.


// let polarToCartesianApprox (r,theta) n = 
//     failwithf "Tick1 not yet implemented" // replace this line with your top-level implementation




let factorial n =
    [1 .. n]
    |> List.fold (fun acc x -> acc * float x) 1.0


let sineTaylorSeries x n =
    if n = 0 then 0.0 else
    let terms = 
        [0 .. (n - 1) / 2]
        |> List.map (fun k -> Math.Pow(-1.0, float k) * Math.Pow(x, float (2 * k + 1)) / float (factorial (2 * k + 1)))
    List.sum terms


let cosineTaylorSeries x n =
    if n = 0 then 1.0 else
    let terms = 
        [0 .. n / 2]
        |> List.map (fun k -> Math.Pow(-1.0, float k) * Math.Pow(x, float (2 * k)) / float (factorial (2 * k)))
    List.sum terms

let polarToCartesianApprox (r, theta) n =
    let x = r * cosineTaylorSeries theta n
    let y = r * sineTaylorSeries theta n
    (x, y)



//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [1.;2.] [1.;2.]
    List.allPairs testPolarCoords [0;1;2;3;10]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [
        ((1.0, 1.0), 0, (1.0, 0.0))       
        ((1.0, 2.0), 0, (1.0, 0.0))        
        ((2.0, 1.0), 0, (2.0, 0.0))        
        ((2.0, 2.0), 0, (2.0, 0.0))        
        ((1.0, 1.0), 1, (1.0, 1.0))        
        ((1.0, 2.0), 1, (1.0, 2.0))        
        ((2.0, 1.0), 1, (2.0, 2.0))        
        ((2.0, 2.0), 1, (2.0, 4.0))        
        ((1.0, 1.0), 2, (0.5, 1.0))        
        ((1.0, 2.0), 2, (-1.0, 2.0))        
        ((2.0, 1.0), 2, (1.0, 2.0))        
        ((2.0, 2.0), 2, (-2.0, 4.0))        
        ((1.0, 1.0), 3, (0.5, 0.8333333333))        
        ((1.0, 2.0), 3, (-1.0, 0.6666666667))        
        ((2.0, 1.0), 3, (1.0, 1.666666667))        
        ((2.0, 2.0), 3, (-2.0, 1.333333333))        
        ((1.0, 1.0), 10, (0.5403023038, 0.8414710097))        
        ((1.0, 2.0), 10, (-0.4161552028, 0.9093474427))        
        ((2.0, 1.0), 10, (1.080604608, 1.682942019))        
        ((2.0, 2.0), 10, (-0.8323104056, 1.818694885))
    ]
/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001
    let testItem fn (coords, n, (expectedX,expectedY) as expected) =
        let actualX,actualY as actual = testFun coords n
        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A"coords n expected actual
            1
        else
            0
    printfn "Starting tests..."
    let numErrors = List.sumBy (testItem testFun) testData
    printfn "%d tests Passed %d tests failed." (testData.Length - numErrors) numErrors

[<EntryPoint>]
let main argv =
    testBench testBenchData polarToCartesianApprox
    0 // return an integer exit code
