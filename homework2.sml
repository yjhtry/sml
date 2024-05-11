fun all_except_option(target: string, xs: string list) =
  case xs of
    [] => NONE
  | hd::tl => if hd = target then SOME(tl) else let val rest = all_except_option(target, tl)
    in
      case rest of
          NONE => NONE
        | SOME(v) => SOME(hd::v)
    end

val test1 = all_except_option ("string", ["other","string","other"]) = SOME ["other","other"]


fun get_substitutions1(xs: string list list, target: string) =
  case xs of 
    [] => []
  | hd::tl => case all_except_option(target, hd) of
                NONE => get_substitutions1(tl, target)
              | SOME(v) => v @get_substitutions1(tl, target)
             

val test2_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_2 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]



fun get_substitutions2(xs: string list list, target: string) =
  let
    fun sub(xs, target, res) =
      case xs of
        [] => res
      | hd::tl => case all_except_option(target, hd) of
                  NONE => sub(tl, target, res)
                | SOME(v) => sub(tl, target, res @ v)
  in
    sub(xs, target, [])
  end

val test3_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_2 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]

fun similar_names(xs: string list list, r ) =
  let
    fun make(xs, m, l) = 
      case xs of
        [] => []
      | hd::tl => { first = hd, middle = m, last = l } :: make(tl, m, l)
    
  in
    case r of
    {first = f, middle = m, last = l} => case get_substitutions2(xs, f) of
                                          [] => []
                                        | res =>  make(f::res, m, l)
  end


val test4 = similar_names (
  [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], 
  {first="Fred", middle="W", last="Smith"}
) =
    [
        {first="Fred", last="Smith", middle="W"},
        {first="Fredrick", last="Smith", middle="W"},
        {first="Freddie", last="Smith", middle="W"}, 
        {first="F", last="Smith", middle="W"}
      ]

val d = {first="Fred", middle="W", last="Smith"}


datatype suit = Clubs | Diamonds | Hearts | Spades

datatype color = Black | Red

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

fun card_color(s, _) =
  case s of
    Clubs => Black
  | Spades => Black
  | Diamonds => Red
  | Hearts => Red

val test5 = card_color (Clubs, Num 2) = Black

fun card_value(_, v) =
  case v of
    Jack => 11
  | Queen => 10
  | King => 10
  | Ace => 10
  | Num n => n

val test6 = card_value (Clubs, Num 2) = 2

exception IllegalMove of card 

fun remove_card(cs, c: card, e) =
  case cs of
    [] => raise e c
  | hd::tl => if hd = c then tl else hd::remove_card(tl, c, e)


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

fun all_same_color(cs) =
  case cs of
    [] => true
  | hd::[] => true
  | (s1, _)::(s2, _)::tl => if s1 = s2 then all_same_color(tl) else false

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true


fun sum_cards(xs) =
  let
    fun sum(xs, t) =
        case xs of
          [] => t
        | hd::tl => sum(tl, t + card_value(hd))
  in
    sum(xs, 0)
  end

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4


fun score(cs, t) =
  let
    val sum = sum_cards(cs)
  in
    case (sum > t, all_same_color(cs)) of
     (true, true) =>  (sum * 2) div 2
    | (true, false) => sum * 3
    | (false, true) => (t - sum) div 2
    | (false, false) => t - sum
  end

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10)  = 4 w
