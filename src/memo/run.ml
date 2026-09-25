open Stdune

type t = int

let compare = Int.compare
let to_dyn = Dyn.int
let current = ref 0
let is_current t = Int.equal !current t
let restart () = incr current
let current () = !current
let invalid = -1

module For_testing = struct
  let of_int t = t
  let to_int t = t
end
