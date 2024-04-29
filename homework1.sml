(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
fun is_order(a: int * int * int, b: int * int * int) = #1 a < #1 b andalso #2 a < #2 b andalso #3 a < #3 b

val test1 = is_order ((1,2,3),(2,3,4)) = true


(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month(xs: (int * int * int) list, m: int) =
  if null xs
  then 0
  else if #2 (hd xs) = m
    then 1 + number_in_month(tl xs, m)
    else number_in_month(tl xs, m)

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1


(* Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months(xs: (int * int * int) list, ms: int list) =
  if null ms
  then 0
  else number_in_month(xs, hd ms) + number_in_months(xs, tl ms)

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month(xs: (int * int * int) list, m: int) =
  if null xs
  then []
  else if #2 (hd xs) = m
    then hd(xs) :: dates_in_month(tl xs, m)
    else dates_in_month(tl xs, m)

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]


(* Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)
fun dates_in_months(xs: (int * int * int) list, ms: int list): (int * int * int) list =
  if null ms
  then []
  else dates_in_month(xs, hd ms) @ dates_in_months(xs, tl ms)

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]


(* Write a function get_nth that takes a list of strings and an int n and returns the nth element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(xs: string list, idx: int) = 
  if null xs
  then ""
  else if idx = 1
    then hd xs
    else get_nth(tl xs, idx - 1)


val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"


(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
fun date_to_string(y: int, m: int, d: int) =
  let 
    val MONTHS = ["January","February","March","April","May","June","July","August","September","October","Novell","December"];
  in
    get_nth(MONTHS, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
  end

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"


(* Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum(sum: int, xs: int list) =
  if sum > 0
  then if sum - hd(xs) > 0
    then 1 + number_before_reaching_sum(sum - hd(xs), tl(xs))
    else 0
  else 0

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

(* Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem. *)
fun what_month(d: int) =
  let 
    val MONTHS_DAYS = [31,29,31,30,31,30,31,31,30,30,30,31]
  in
    number_before_reaching_sum(d, MONTHS_DAYS) + 1
  end

val test9 = what_month 70

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2. *)
fun month_range(d1: int, d2: int) = 
  if d1 > d2
  then []
  else what_month(d1) :: month_range(d1 + 1, d2)

val test10 = month_range (31, 34) = [1,2,2,2]


(* Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(xs: (int * int * int) list) =
  if null xs
  then NONE
  else 
    let
      val minrest = oldest(tl xs) 
      val f = hd(xs)
    in
      if isSome minrest
      then 
        if (Int.toString(#1 f) ^ Int.toString(#2 f) ^ Int.toString(#3 f)) > (Int.toString(#1 (valOf(minrest))) ^ Int.toString(#2 (valOf(minrest))) ^ Int.toString(#3 (valOf(minrest))))
        then minrest
        else SOME(f)

      else SOME(f)
    end

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)


(* ********** Challenge *************** *)

(* fun includes(xs: int list, x: int) =
  if null xs
  then false
  else if hd(xs) = x
    then true
    else includes(tl(xs), x)

fun deduplication(xs: int list) =
  if null xs
  then []
  else 
    let 
      fun forEach(xs: int list) =

    in
    end *)
