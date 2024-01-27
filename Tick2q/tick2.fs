module Tick2

//---------------------------Tick2 PartA skeleton code-------------------------------//


module PartACase1 =

    type MScBoundaries = { Distinction: int; Merit: int; Pass: int; Fail: int }
    type MEngBoundaries = { First: int; UpperSecond: int; LowerSecond: int; Fail: int }
    type BEngBoundaries = { First: int; UpperSecond: int; LowerSecond: int; Third: int; Fail: int }

    let mScBoundaries = { Distinction = 70; Merit = 60; Pass = 50; Fail = 0 }
    let mEngBoundaries = { First = 70; UpperSecond = 60; LowerSecond = 50; Fail = 0 }
    let bEngBoundaries = { First = 70; UpperSecond = 60; LowerSecond = 50; Third = 40; Fail = 0 }

    

module PartACase2 =

    type Boundaries = { Bound70: string option; Bound60: string option; Bound50: string option; Bound40: string option; Bound0: string option }

    let mScBoundaries = { Bound70 = Some "Distinction"; Bound60 = Some "Merit"; Bound50 = Some "Pass"; Bound40 = None; Bound0 = Some "Fail" }
    let mEngBoundaries = { Bound70 = Some "First"; Bound60 = Some "UpperSecond"; Bound50 = Some "LowerSecond"; Bound40 = None; Bound0 = Some "Fail" }
    let bEngBoundaries = { Bound70 = Some "First"; Bound60 = Some "UpperSecond"; Bound50 = Some "LowerSecond"; Bound40 = Some "Third"; Bound0 = Some "Fail" }


module PartACase3 =

    let mScBoundaries = [("Distinction", 70); ("Merit", 60); ("Pass", 50); ("Fail", 0)]
    let mEngBoundaries = [("First", 70); ("UpperSecond", 60); ("LowerSecond", 50); ("Fail", 0)]
    let bEngBoundaries = [("First", 70); ("UpperSecond", 60); ("LowerSecond", 50); ("Third", 40); ("Fail", 0)]


//---------------------------Tick2 PartB case 2 skeleton code-------------------------------//

module PartBCase2 =

    open PartACase2

    let classify (course: string) (mark: float) : Result<string,string> =
        if mark < 0.0 || mark > 100.0 then 
            Error "Marks must be in the range 0 - 100."
        else
            let boundaries = 
                match course with
                | "MSc" -> Some mScBoundaries
                | "MEng" -> Some mEngBoundaries
                | "BEng" -> Some bEngBoundaries
                | _ -> None
            
            match boundaries with
            | Some b when mark >= 70.0 -> Ok (b.Bound70.Value)
            | Some b when mark >= 60.0 -> Ok (b.Bound60.Value)
            | Some b when mark >= 50.0 -> Ok (b.Bound50.Value)
            | Some b when mark >= 40.0 && b.Bound40.IsSome -> Ok (b.Bound40.Value)
            | Some b when mark >= 0.0 -> Ok (b.Bound0.Value)
            | Some _ -> Error "Invalid mark"
            | None -> Error "Course not recognized."


//---------------------------Tick2 PartB case 3 skeleton code-------------------------------//

module PartBCase3 =

    open PartACase3

    let classify (course: string) (mark: float) : Result<string,string> =
        if mark < 0.0 || mark > 100.0 then 
            Error "Marks must be in the range 0 - 100."
        else
            let boundaries = 
                match course with
                | "MSc" -> Some mScBoundaries
                | "MEng" -> Some mEngBoundaries
                | "BEng" -> Some bEngBoundaries
                | _ -> None

            match boundaries with
            | Some bs ->
                match List.tryFind (fun (_, boundary) -> mark >= float boundary) bs with
                | Some (classification, _) -> Ok classification
                | None -> Error "Invalid mark"
            | None -> Error "Course not recognized."

//------------------------------------Tick2 PartC skeleton code-----------------------------------//

module PartC =
    open PartACase3 // get unqualified access to Case 3 types and values
    open PartBCase3 // get unqualified access to classify function

    type Marks = {Mark1: float} // simplified set of marks (just one mark) used for compilation of code

    // Existing markTotal and upliftFunc definitions are assumed to be correct.

    let classifyAndUplift 
        (boundaries: string list)
        (course: string) 
        (marks: Marks)
            : Result<string,string> =

        // First, check if the total mark is valid.
        match markTotal marks course with
        | None -> Error "Invalid course or mark."
        | Some total ->
            // Attempt to find a boundary that the student's mark is close to and could potentially be uplifted.
            let upliftAttempt = 
                boundaries 
                |> List.tryPick (fun boundary -> 
                    match upliftFunc marks total boundary course with
                    | Ok {IsAboveBoundary = true; Uplift = _} -> Some (classify course total)
                    | Ok {IsAboveBoundary = false; Uplift = Some uplift} -> Some (classify course (total + uplift))
                    | Ok {IsAboveBoundary = false; Uplift = None} -> None
                    | Error err -> Some (Error err))

            // Process the result of the uplift attempt.
            match upliftAttempt with
            | None -> classify course total // If no uplift applies, classify normally.
            | Some result -> result // Return the result of the uplift attempt.


//------------------------------Simple test data and functions---------------------------------//
module TestClassify =
    /// test data comaptible with the Tick 2 problem
    let classifyUnitTests = [
        "MEng",75.0, Ok "First"
        "MSc", 75.0,Ok "Distinction"
        "BEng", 75.0, Ok "First"
        "MEng",65.0, Ok "UpperSecond"
        "MSc", 65.0, Ok "Merit"
        "BEng", 65.0, Ok "UpperSecond"        
        "MEng",55.0, Ok "LowerSecond"
        "MSc", 55.0, Ok "Pass"
        "BEng", 55.0, Ok "LowerSecond"        
        "MEng",45.0, Ok "Fail"
        "MSc", 45.0, Ok "Fail"
        "BEng", 45.0, Ok "Third"
        "BEng", 35.0, Ok "Fail"        
    ]

    let runClassifyTests unitTests classify testName =
        unitTests
        |> List.map (fun (data as (course,mark,_)) -> classify course mark, data)
        |> List.filter (fun (actualClass, (_,_,className)) -> actualClass <> className)
        |> function 
            | [] -> printfn $"all '{testName}' tests passed."
            | fails -> 
                fails 
                |> List.iter (fun (actual, (course,mark,className)) 
                                -> printfn $"Test Failed: {course}, {mark}, expected className={className}, \
                                          actual className={actual}")


//-------------------------------------------------------------------------------------------//
//---------------------------------Run Part B tests------------------------------------------//
//-------------------------------------------------------------------------------------------//

open TestClassify
let runTests() =
    runClassifyTests classifyUnitTests PartBCase2.classify "Case2"
    runClassifyTests classifyUnitTests PartBCase3.classify "Case3"


//-------------------------------------------------------------------------------------------//
//---------------------------------Tick2 Part X Skeleton code--------------------------------//
//-------------------------------------------------------------------------------------------//
module PartX =
    type Lens<'A,'B> = ('A -> 'B) * ('B -> 'A -> 'A)

    let lensMap (lens: Lens<'A,'B>) (f: 'B -> 'B) (a: 'A) =       
        (fst lens a |> f |> snd lens) a

    let mapCAndB (lensC: Lens<'A,'C>) (lensB: Lens<'A,'B>) (fc:'C->'C) (fb: 'B->'B) =
        lensMap lensC fc >> lensMap lensB fb

    let combineLens (l1: Lens<'A,'B>) (l2: Lens<'B,'C>) : Lens<'A,'C> =
        let getL1, setL1 = l1
        let getL2, setL2 = l2

        let getCombined a = 
            let b = getL1 a
            getL2 b

        let setCombined c a =
            let b = getL1 a
            let newB = setL2 c b
            setL1 newB a

        (getCombined, setCombined)