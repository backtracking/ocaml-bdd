open Prop
open Bench_prop

type bench = De_bruijn_p | De_bruijn_n | Pigeon_p | Equiv_p

let bench = ref De_bruijn_p
let n = ref 10

let _ =

  Arg.parse [
    "-de-bruijn-p", Arg.Unit (fun () -> bench := De_bruijn_p), "";
    "-de-bruijn-n", Arg.Unit (fun () -> bench := De_bruijn_n), "";
    "-pigeon-p", Arg.Unit (fun () -> bench := Pigeon_p), "";
    "-equiv-p", Arg.Unit (fun () -> bench := Equiv_p), ""
  ] (fun x -> n := int_of_string x) "";

  match !bench with
    | De_bruijn_p ->
	Format.printf "# de_bruijn_p n=%d@\n%a@." !n print (de_bruijn_p !n)
    | De_bruijn_n ->
	Format.printf "# de_bruijn_n n=%d@\n%a@." !n print (de_bruijn_n !n)
    | Pigeon_p ->
	Format.printf "# pigeon_p n=%d@\n%a@." !n print (pigeon_p !n)
    | Equiv_p ->
	Format.printf "# equiv_p n=%d@\n%a@." !n print (equiv_p !n)
