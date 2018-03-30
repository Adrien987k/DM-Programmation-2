let rec m91 x =
  if x > 100 then
    x - 10
  else
    m91 (m91 (x + 11))

let _ = m91 1
