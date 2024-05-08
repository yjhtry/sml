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
