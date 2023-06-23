(
  (let closure_id 8 ) 
  (makefun id_1 {arg_1 arg_2} (+ arg_1 arg_2 closure_id)) 
  (let id_for_func 7)
  (id_1 id_for_func (if 0 then 15 else 8))
)
# evaluated to 23.0
