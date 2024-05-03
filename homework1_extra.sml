fun alternate(xs: int list) = 
  let 
    fun count(xs: int list, res: int, isInc: bool) = 
      if null xs
      then res
      else
        if isInc
        then count(tl xs, res + hd(xs), false)
        else count(tl xs, res - hd(xs), true)
  in
    count(xs, 0, true)
  end


val test1 = alternate([1,2,3,4]) 


fun min_max(xs: int list) =
  if null xs
  then (0, 0)
    else let
      fun min_max_rest(xs: int list) =
        if null (tl(xs))
        then (hd(xs), hd(xs))
        else let val rest = min_max_rest(tl xs)
          val max  = if hd(xs) > (#2 rest) then hd(xs) else (#2 rest)
          val min = if hd(xs) < (#1 rest) then hd(xs) else (#1 rest)
        in
          (min, max)
        end

    in
      min_max_rest(xs)
    end

val test2 = min_max([5, 87, 2, 90, 1])  = (1, 90)

fun cumsum(xs: int list) =
  if null xs
  then []
  else let 
    fun acc(xs: int list, res: int list, total: int) =
    if null xs
    then res
    else acc(tl(xs), (res @ [total + hd(xs)]), total + hd(xs))
  in
    acc(xs, [], 0)
  end

val test3 = cumsum([1,2,3,4,5,6,7,8,9,10]) = [1,3,6,10,15,21,28,36,45,55]


fun greeting(name: string option) = 
  "Hello there, " ^ (if isSome name then valOf(name) else "you")

val test4_1 = greeting(SOME("john")) = "Hello there, john"
val test4_2 = greeting(NONE) = "Hello there, you"


fun repeat(xs: int list * int list) =
  if null (#2 xs)
  then []
  else let
    fun repeat_by(v: int, count: int) =
      if count = 0
      then []
      else v :: repeat_by(v, count - 1)
  in
    repeat_by(hd(#1 xs), hd(#2 xs)) @ repeat((tl (#1 xs), tl(#2 xs)))
  end

val test5 = repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]


fun addOpt(a: int option, b: int option) =
  if isSome a andalso isSome b
  then SOME(valOf a + valOf b)
  else NONE


val test6 = addOpt(SOME(1), SOME(2)) = SOME(3)


fun addAllOpt(xs: int option list) =
  if null xs
  then NONE
  else if isSome (hd(xs))
    then let 
      val first = valOf (hd(xs))
      val rest = addAllOpt(tl xs)
    in
      if isSome rest
      then SOME(first + (valOf rest))
      else SOME(first)
    end
    else addAllOpt(tl xs)

val test7 = addAllOpt([SOME(1), NONE, SOME(4), NONE, NONE, SOME(~2)]) = SOME(3)

fun any(xs: bool list) =
  if null xs
  then false
  else if hd xs = true
  then true
  else any(tl xs)

val test8 = any([false, false, true, false]) = true

fun all(xs: bool list) =
  if null xs
  then true
  else if hd xs = false
  then false
  else all(tl xs)


val test8 = all([true, true, true, false]) = false


fun zip(a: int list, b: int list) =
  if null a orelse null b
  then []
  else (hd(a), hd(b)) :: zip(tl a, tl b)

val test9 = zip([1,2,3,4], [9,8,7]) = [(1,9),(2,8),(3,7)]

fun len(xs: int list) =
  if null xs
  then 0
  else 1 + len(tl xs)

fun zipRecycle(oa: int list, ob: int list) =
  let
    val len_oa = len(oa)
    val len_ob = len(ob)
    fun zip_in(na: int list, nb: int list) =
      if null na andalso not (null nb) andalso len_oa < len_ob
      then (hd(oa), hd(nb)) :: zip_in(tl oa, tl nb)
      else if null nb andalso not (null na) andalso len_ob < len_oa
      then (hd(na), hd(ob)) :: zip_in(tl na, tl ob)
      else if not (null na) andalso not (null nb)
      then (hd(na), hd(nb)) :: zip_in(tl na, tl nb)
      else []
        
  in
    zip_in(oa, ob)
  end

val test10 = zipRecycle([1,2,3,4], [9,8,7,1,2,3]) = [(1,9),(2,8),(3,7),(4,1),(1,2),(2,3)]

fun zipOpt(a: int list, b: int list) =
   if len(a) <> len(b)
   then NONE
   else SOME(zip(a, b))

val test11 = zipOpt([1,2,3], [4,5,6]) = SOME([(1,4),(2,5),(3,6)])
val test12 = zipOpt([1,2,3], [4,5]) = NONE
