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
