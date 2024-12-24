type named_func = { name : string; win_sz : int; f : (float -> float) option }

val apply_data : (float * float) list -> named_func list
