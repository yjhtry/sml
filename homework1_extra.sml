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


val test9 = all([true, true, true, false]) = false


fun zip(a: int list, b: int list) =
  if null a orelse null b
  then []
  else (hd(a), hd(b)) :: zip(tl a, tl b)

val test11 = zip([1,2,3,4], [9,8,7]) = [(1,9),(2,8),(3,7)]

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

val test11 = zipRecycle([1,2,3,4], [9,8,7,1,2,3]) = [(1,9),(2,8),(3,7),(4,1),(1,2),(2,3)]

fun zipOpt(a: int list, b: int list) =
   if len(a) <> len(b)
   then NONE
   else SOME(zip(a, b))

val test12_1 = zipOpt([1,2,3], [4,5,6]) = SOME([(1,4),(2,5),(3,6)])
val test12_2 = zipOpt([1,2,3], [4,5]) = NONE

fun lookup(xs: (string * int) list, s: string) =
  if null xs
  then NONE
  else if #1 (hd(xs)) = s
  then SOME(#2 (hd(xs)))
  else lookup(tl xs, s)


val test13_1 = lookup([("hello", 1)], "hello")
val test13_2 = lookup([("helloo", 1)], "hello")

fun splitup(xs: int list) =
  if null xs
  then ([], [])
  else let
    fun gt(xs: int list, n: int) =
      if null xs
      then []
      else if hd(xs) > n
      then hd(xs) :: gt(tl xs, n)
      else gt(tl xs, n)
    fun lt(xs: int list, n: int) =
      if null xs
      then []
      else if hd(xs) < n
      then hd(xs) :: lt(tl xs, n)
      else lt(tl xs, n)
  in
  (gt(xs, ~1), lt(xs, 0))
  end

val test14 = splitup([1,2,3,4,~1,~3,1,2,~9]) = ([1,2,3,4,1,2],[~1,~3,~9])

fun splitAt(xs: int list, n) =
  if null xs
  then ([], [])
  else let
    fun gt(xs: int list) =
      if null xs
      then []
      else if hd(xs) >= n
      then hd(xs) :: gt(tl xs)
      else gt(tl xs)
    fun lt(xs: int list) =
      if null xs
      then []
      else if hd(xs) < n
      then hd(xs) :: lt(tl xs)
      else lt(tl xs)
  in
  (gt(xs), lt(xs))
  end

val test15 = splitAt([1,2,3,4,~1,~3,1,2,~9], 3) = ([3,4],[1,2,~1,~3,1,2,~9])

fun isSorted(xs: int list) =
  if null xs
  then true
  else let
    fun compare(xs: int list, n: int) =
      if null xs
      then true
      else if n < hd(xs)
      then compare(tl xs, hd(xs))
      else false
  in
    compare(tl xs, hd xs)
  end

val test16_1 = isSorted([1,2,3,4,5]) = true
val test16_2 = isSorted([1,2,4,3,5]) = false

fun isAnySorted(xs: int list) =
 if null xs
  then true
  else let
    fun predicate_gt(a: int, b: int) =
      a > b
    fun predicate_lt(a: int, b: int) =
      a < b
    fun compare(xs: int list, n: int, predicate) =
      if null xs
      then true
      else if predicate(n, hd(xs))
      then compare(tl xs, hd(xs), predicate)
      else false
  in
    compare(tl xs, hd xs, predicate_gt) orelse compare(tl xs, hd xs, predicate_lt)
  end 


val test17_1 = isAnySorted([1,2,3,4,5]) = true
val test17_2 = isAnySorted([1,2,4,3,5]) = false
val test17_3 = isAnySorted([5,4,3,2,1]) = true
val test17_4 = isAnySorted([5,3,2,4,1]) = false


fun sortedMerge(a: int list, b: int list) =
  if null a andalso null b
  then []
  else if null a andalso not (null b)
  then b
  else if null b andalso not (null a)
  then a
  else if hd(a) > hd(b)
  then hd(b) :: sortedMerge(a, tl b)
  else hd(a) :: sortedMerge(tl a, b)


val test18 = sortedMerge([1,4,7], [5,8,9]) = [1,4,5,7,8,9]

fun qsort(xs: int list) =
  if null xs
  then []
  else let
    val first = hd(xs)
    val res = splitAt(tl xs, first)
    val left = #1 res
    val right  = #2 res
  in
    qsort(right) @ [first] @ qsort(left)
  end


val test19 = qsort([1,4,2,6,5,3,4,1])

fun divide(xs: int list) =
  if null xs
  then ([], [])
  else let
    fun skip_one(xs: int list, skip: bool) =
    if null xs
    then []
    else if skip
    then skip_one(tl xs, false)
    else hd(xs) :: skip_one(tl xs, true)
  in
  (skip_one(xs, false), skip_one(tl xs, false))
  end

val test20 = divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])

fun fullDivide(p: int * int) =
  let
    val p1 = #1 p
    val p2 = #2 p

    fun full(a: int, b: int, c: int) =
      if (b mod a) = 0
      then full(a, (b div a), c + 1)
      else (c, b)
  in
    full(p1, p2, 0)
  end


val test21_1 = fullDivide(2, 40) = (3, 5)
val test21_2 = fullDivide(3, 10) = (0, 10)
