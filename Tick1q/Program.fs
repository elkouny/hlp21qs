﻿open System

//------------------------write your answer function(s) here---------------------//

// top-level subfunctions of polarToCartesianApprox (if any)

/// answer to Tick1
// the header given here is correct.
let odd lst =
    List.collect (fun x -> if x % 2 = 0 then [ 0 ] else [ 1 ]) lst

let even lst =
    List.collect (fun x -> if x % 2 = 0 then [ 1 ] else [ 0 ]) lst

let fact n =
    if n = 0 then 1.0 else [ 1.0 .. float n ] |> List.reduce (*)

let clean lst =
    List.collect (fun x -> if x = 0.0 then [] else [ x ]) lst

let coeff = (fun index value -> if index % 2 = 0 then value else value * -1.0)

let polarToCartesianApprox (r, theta) n =
    let lst = [ 0..n ]
    let sin_index = odd lst
    let cos_index = even lst
    let term i = (theta) ** float i / float (fact i)

    let tmp_sin_terms =
        List.mapi (fun index value -> (term index) * float value) sin_index

    let tmp_cos_terms =
        List.mapi (fun index value -> term index * float value) cos_index

    let sin_terms_cleaned = clean tmp_sin_terms
    let cos_terms_cleaned = clean tmp_cos_terms
    let sin_terms = List.mapi coeff sin_terms_cleaned
    let cos_terms = List.mapi coeff cos_terms_cleaned
    let sin = if sin_terms = [] then 0.0 else List.reduce (+) sin_terms
    let cos = if cos_terms = [] then 0.0 else List.reduce (+) cos_terms
    (r * cos, r * sin)







//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [ 1.; 2. ] [ 1.; 2. ]
    List.allPairs testPolarCoords [ 0; 1; 2; 3; 10 ]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [ ((1.0, 1.0), 0, (1.0, 0.0))
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
      ((2.0, 2.0), 10, (-0.8323104056, 1.818694885)) ]

/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001

    let testItem fn (coords, n, (expectedX, expectedY) as expected) =
        let actualX, actualY as actual = testFun coords n

        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A" coords n expected actual
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
