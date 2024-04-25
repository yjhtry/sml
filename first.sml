val a = 1;

(* variable shadow *)
val a = 3;

fun fibonacci 0 = 0
  | fibonacci 1 = 1
  | fibonacci x = fibonacci(x-1) + fibonacci(x-2);


val f10 = fibonacci 10;
