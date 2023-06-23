// N = 12
// Лисин Роман, М8О-306Б-20 
// Print a table of a given function f, computed by taylor series

// function to compute
let f x = 3. ** x

let a = 0.
let b = 1.
let n = 10
let eps = 0.000001

let rec for_ f i a b =
    if a > b then i
    else f a (for_ f i (a + 1.) b)
    
let fact = for_ (*) 1. 1.

let rec iter start f acc =
  let cur_res = f start acc
  if (abs (acc - cur_res)) <= eps then 
    (start, cur_res)
  else 
    iter (start + 1.) f cur_res


// Define a function to compute f using naive taylor series method
let taylor_n x n = ((log 3.) ** n) * (x ** n) / float(fact n)

let taylor_naive x = 
  iter 1. (fun it acc -> acc + (taylor_n x it)) 1.


// Define a function to do the same in a more efficient way
let clever_taylor_n x n = (log 3.) * x / n

let rec clever_iter start x acc res =
  let cur_item = acc * (clever_taylor_n x start)
  let cur_res = res + cur_item
  if (abs ((f x) - cur_res)) <= eps then 
    (start, cur_res)
  else 
    clever_iter (start + 1.) x cur_item cur_res

let taylor x = clever_iter 1. x 1. 1.
  

let main =
  for i=0 to n do
    let x = float(a+(float i)/(float n)*(b-a))
    let (naive_iterations, naive_res) = taylor_naive x
    let (iterations, res) = taylor x 
    printfn "%5.2f  %10.6f %10.6f,%10.6f %10.6f,%10.6f" x (f x) naive_res naive_iterations res iterations
// make sure to improve this table to include the required number of iterations
// for each of the methods

main
