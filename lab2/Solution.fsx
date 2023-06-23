// Solution
// N = 12
// Лисин Роман, М8О-306Б-20

// In this sample, the goal is to insert zeros before negative numbers


// Method 1: Library Function
let insert_zero_before_negative list =
  list  |> List.map (fun el -> if el < 0 then [0; el] else [el])
        |> List.concat

// Method 2: Recursion
let rec insert_zero_before_negative2 list = 
    match list with
    | [] -> []
    | h::t -> if h < 0 then
                 [0; h] @ (insert_zero_before_negative2 t)
              else
                 [h] @ (insert_zero_before_negative2 t)

// Method 3: Tail Rec
let insert_zero_before_negative3 list =
    let rec insert_zero_before_negative3 list list' =
        match list with
        | []   -> list'
        | h::t -> if h < 0 then
                      insert_zero_before_negative3 t (list' @ [0; h])
                  else
                      insert_zero_before_negative3 t (list' @ [h])
    insert_zero_before_negative3 list []

  
let list = [-5; 0; -4; 10; -8]
printfn "%A" (insert_zero_before_negative list)
printfn "%A" (insert_zero_before_negative2 list)
printfn "%A" (insert_zero_before_negative3 list)
