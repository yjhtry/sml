datatype Exp = Constant of int
  | Negate of Exp
  | Add of Exp * Exp
  | Multiply of Exp * Exp


fun eval(e: Exp) =
  case e of
     Constant v => v
   | Negate(a) => ~(eval(a))
   | Add(a, b) => (eval(a) + eval(b))
   | Multiply(a, b) => (eval(a) * eval(b))


val value = eval(Multiply(Add(Constant(1), Constant(2)), Constant(2)))

datatype my_int_lsit = Empty
  | Cons of int * my_int_lsit

val my_list1 = Cons((1, Empty))

fun append_my_list(xs: my_int_lsit, xy: my_int_lsit) =
  case xs of
     Empty => xy
   | Cons(x, y') => Cons(x, append_my_list(y', xy))

val my_list2 =  append_my_list(my_list1, Cons(2, Empty))

fun my_is_some(dd: 'a option) =
  case dd of
     NONE => false
   | SOME(_) => true

val test1 = my_is_some(SOME(1))

fun add_one(x: int) = x + 1

val add_two = add_one o add_one

val test2 = d(1)


fun sum_list xs =
  case xs of
     [] => 0
   | x::xs' => x + sum_list(xs')

val test3 = sum_list([1,2,3])


fun append_list(xs, ys) =
  case xs of
     [] => ys
   | x::xs' => x::append_list(xs', ys)

val test4 = append_list([], [1])


datatype ('a, 'b) my_tree =
    Leaf of 'b
  | Node of 'a * ('a, 'b) my_tree * ('a, 'b) my_tree

fun sum_my_tree t =
  case t of
     Leaf(v) => v
   | Node(v, lft, rgt) => v + sum_my_tree(lft) + sum_my_tree(rgt)

val node = Node(2, Leaf(1), Leaf(2))

val test5 = sum_my_tree(node)

val tripe = (1, 2, 3)

fun add_trip t = 
  case t of
    (x, y, z) => x + y + z

val test6_1 = add_trip(tripe)
val test6_2 = add_trip(1,2,3)


val people = {name = "john", friend ="lei"}

fun print_people p = 
  case p of
    {name = n, friend = f} => n ^ " " ^ f

val test7 = print_people people

fun compare(a, b) =
  a = b

val test8_1 = compare(5, 6)

(* will error *)
(* val test8_2 = compare(5.1, 6.2) *) 

exception SomeError

fun throw_err() =
  raise SomeError

val test9 = throw_err()

exception LengthNoMatch

fun zip3 lst =
  case lst of
    ([],[],[]) => []
  | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
  | _ => raise LengthNoMatch

val test_10_value = ([1,2,3], [4,5,6], [7,8,9])
val test10 = zip3 test_10_value

fun un_zip3 lst =
  case lst of
    [] => ([], [], [])
  | (hd1, hd2, hd3)::tl => let val (tl1, tl2, tl3) = un_zip3(tl)
    in
      (hd1::tl1, hd2::tl2, hd3::tl3)
    end

val test11 = un_zip3(test10)



val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f::(!cbs) (* The only "public" binding *)

fun onEvent i =
  let fun loop fs =
    case fs of
    [] => ()
    | f::fs => (f i; loop fs)
  in loop (!cbs) end
