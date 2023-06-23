// N = 12
// Лисин Роман, М8О-306Б-20 
// Define functions to solve algebraic equations

let eps = 0.000001

let rec dichotomy f a b = 
  if (b - a) <= eps then  
    ((a + b) / 2.)
  else
    let c = (a + b) / 2.
    if ((f b) * (f c)) < 0. then 
      dichotomy f c b
    else
      dichotomy f a c

let rec iter f acc =
  let cur_res = f acc
  if (abs (cur_res - acc)) <= eps then 
    cur_res
  else 
    iter f cur_res

let iterations phi x0 = iter (fun acc -> (phi acc)) x0

let newthon f f' x0 = iter (fun acc -> acc - (f acc) / (f' acc)) x0

// Solve 3 equations using three methods defined above
let f1 x = (log x) - x + 1.8
let f2 x = x * (tan x) - (1. / 3.) 
let f3 x = (tan (x / 2.)) - (1. / (tan (x / 2.))) + x

let f1' x = (1. / x) - 1.
let f2' x = (((cos x) * (sin x)) + x) / float ((cos x) * (cos x)) 
let f3' x = (1. / 2. * ((cos (x / 2.)) * (cos (x / 2.)))) + (1. / 2. * ((sin (x / 2.)) * (sin (x / 2.)))) + 1.

let phi1 x = (log x) + 1.8
let phi2 x = 1. / (3. * (tan x))
let phi3 x = (- (tan (x / 2.)) * (tan (x / 2.)) + 1.) / (tan (x / 2.))

let main = 
    printfn "%10.5f  %10.5f %10.5f" (dichotomy f1 2. 3.) (iterations phi1 2.) (newthon f1 f1' 2.)
    printfn "%10.5f  %10.5f %10.5f" (dichotomy f2 0.2 1.) (iterations phi2 0.2) (newthon f2 f2' 0.2)
    printfn "%10.5f  %10.5f %10.5f" (dichotomy f3 1. 2.) (iterations phi3 1.) (newthon f3 f3' 2.)
