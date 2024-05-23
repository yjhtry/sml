val only_capitals = let
  val isTitle = Char.isUpper o String.sub
in
  List.filter (fn s => isTitle (s, 0))
end

val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["A","dd","B","C"] = ["A","B","C"]

fun longest_string1 list = List.foldl (fn (curr, acc) => if String.size acc >= String.size curr then acc else curr) "" list

val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 [] = ""
val test2_3 = longest_string1 ["A","bc","C", "dd"] = "bc"


fun longest_string2 list = List.foldr (fn (curr, acc) => if String.size acc >= String.size curr then acc else curr) "" list

val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 [] = ""
val test3_3 = longest_string2 ["A","bc","C", "dd"] = "dd"

fun longest_string_helper f list = List.foldl f "" list

val longest_string3 = longest_string_helper (fn (curr, acc) => if String.size acc >= String.size curr then acc else curr)


val test4_1 = longest_string3 ["A","bc","C"] = "bc"
val test4_2 = longest_string3 [] = ""
val test4_3 = longest_string3 ["A","bc","C", "dd"] = "bc"


val longest_string4 = longest_string_helper (fn (curr, acc) => if String.size acc > String.size curr then acc else curr)

val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 [] = ""
val test3_3 = longest_string2 ["A","bc","C", "dd"] = "dd"


fun longest_capitalized list = let
  val isTitle = Char.isUpper o String.sub
in
  List.foldl (fn (curr, acc) => if isTitle (curr, 0) andalso String.size curr > String.size acc then curr else acc) "" list
end

val test5_1 = longest_capitalized ["A","bc","C"] = "A"
val test5_2 = longest_capitalized ["A","bc","Cd"] = "Cd"

fun rev_string s = let
  val rev_s_list =List.rev (List.map Char.toString (String.explode s))
in
  List.foldl (fn (curr, acc) => acc ^ curr) "" rev_s_list
end

val test6 = rev_string "abc" = "cba"


exception NoAnswer

fun first_answer f list = case list of
  [] => raise NoAnswer
  | x::xs' => (let val res = f x in if isSome res then valOf(res) else first_answer f xs' end)

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

fun all_answers f list = case list of
  [] => NONE
  | _ => (let val xs = List.filter isSome (List.map f list) in if null xs then NONE else SOME(xs) end)

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [SOME [2],SOME [4],SOME [6]]


datatype pattern = Wildcard | Variable of string | UnitP | ConstP of int
| TupleP of pattern list | ConstructorP of string * pattern

datatype valu = Const of int | Unit | Tuple of valu list | Constructor of string * valu

fun count_wildcards p = 
  case p of
     Wildcard => 1
   | TupleP(xs) => List.foldl (fn (curr, acc) => acc + count_wildcards(curr)) 0 xs
   | ConstructorP(_, p) => count_wildcards(p)
   | _ => 0

val test9a_1 = count_wildcards Wildcard = 1
val test9a_4 = count_wildcards (TupleP [Wildcard,Wildcard,Wildcard,TupleP [Wildcard, UnitP, ConstructorP ("", Wildcard)]]) = 5

fun count_wild_and_variable_lengths p = 
  case p of
     Wildcard => 1
   | TupleP(xs) => List.foldl (fn (curr, acc) => acc + count_wild_and_variable_lengths(curr)) 0 xs
   | ConstructorP(_, p) => count_wild_and_variable_lengths(p)
   | Variable(s) => String.size s
   | _ => 0

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP [Wildcard,TupleP [Wildcard, Variable("222"), ConstructorP ("", Wildcard)]]) = 6

fun count_some_var v =
  case v of
     (s, Variable(s1)) => if s = s1 then 1 else 0
   | (s, TupleP(xs)) => List.foldl (fn (curr, acc) => acc + count_some_var((s, curr))) 0 xs
   | (s, ConstructorP(_, p)) => count_some_var(s, p)
   | _ => 0

val test9c = count_some_var ("x", Variable("x")) = 1


 (* todo *)
(* fun check_pat v =
  case v of
     Variable(s) => true
   | TupleP(xs) => List.foldl (fn (curr, acc) => case curr of
      pat1 => body1
    | pat2 => body2) true xs
   | ConstructorP(_, p) => check_pat(p)
   | _ => true *)
