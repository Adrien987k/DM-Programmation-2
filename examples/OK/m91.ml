let rec m91 x =
  if x > 100 then
    x - 10
  else
    m91 (m91 (x + 11))

let rec fact x =
  if x = 0 then 1
  else x * (fact (x - 1)) 

let _ = m91 1

let _ = fact 5
