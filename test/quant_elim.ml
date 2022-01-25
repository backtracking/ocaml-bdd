
let x = 1
let y = 2
let z = 3

let print_var fmt x =
  match x with
  | 1 -> Format.fprintf fmt "x"
  | 2 -> Format.fprintf fmt "y"
  | 3 -> Format.fprintf fmt "z"
  | _ -> Format.fprintf fmt "b%d" x

module B = Bdd.Make(struct
               let print_var = print_var
               let size = 7001
               let max_var = 100
             end)

open Format

let () =
  (* x /\ y *)
  let andxy = B.mk_and (B.mk_var x) (B.mk_var y) in
  (* x \/ y *)
  let orxy = B.mk_or (B.mk_var x) (B.mk_var y) in
  (* y /\ z *)
  let andyz = B.mk_and (B.mk_var y) (B.mk_var z) in
  (* y \/ z *)
  let oryz = B.mk_or (B.mk_var y) (B.mk_var z) in
  let b = B.mk_exist ((==) y) andxy in
  printf "exists y. x /\\ y ===> @[%a@]@." B.print b;
  assert (b == B.mk_var x);
  let b = B.mk_exist ((==) y) orxy in
  printf "exists y. x \\/ y ===> @[%a@]@." B.print b;
  assert (b == B.one);
  let b = B.mk_exist ((==) y) andyz in
  printf "exists y. y /\\ z ===> @[%a@]@." B.print b;
  assert (b == B.mk_var z);
  let b = B.mk_exist ((==) y) oryz in
  printf "exists y. y \\/ z ===> @[%a@]@." B.print b;
  assert (b == B.one);
  let b = B.mk_forall ((==) y) andxy in
  printf "forall y. x /\\ y ===> @[%a@]@." B.print b;
  assert (b == B.zero);
  let b = B.mk_forall ((==) y) orxy in
  printf "forall y. x \\/ y ===> @[%a@]@." B.print b;
  assert (b == B.mk_var x);
  let b = B.mk_forall ((==) y) andyz in
  printf "forall y. y /\\ z ===> @[%a@]@." B.print b;
  assert (b == B.zero);
  let b = B.mk_forall ((==) y) oryz in
  printf "forall y. y \\/ z ===> @[%a@]@." B.print b;
  assert (b == B.mk_var z);
  ()
