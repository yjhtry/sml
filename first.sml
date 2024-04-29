val a = 1;

(* variable shadow *)
val a = 3;

fun fibonacci 0 = 0
  | fibonacci 1 = 1
  | fibonacci x = fibonacci(x-1) + fibonacci(x-2);


val f10 = fibonacci 10;

fun push(list: int list, value: int) = 
  if null list 
  then [value] 
  else 
  hd(list) :: push(tl(list), value);

val l = push([ 1, 2 ], 3);

fun max(xs: int list) = 
  if null xs
  then NONE
  else let
      fun max_nonone(xs: int list) =
        if null (tl xs)
        then hd xs
        else let val maxrest = max_nonone(tl xs);
          in
            if hd xs > maxrest
            then hd xs
            else maxrest
          end
      in  
        SOME(max_nonone(xs))
      end

val max = max([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

fun flatten(xs: 'a list list) = 
  if null xs
  then []
  else if null (hd(xs))
    then flatten(tl(xs))
    else hd(hd(xs)) :: flatten(tl(hd(xs)) :: tl(xs))

val f = flatten([[1]]) = [1]
