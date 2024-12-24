type command = Exit | Point of (float * float) | Freq of float

val read_command : unit -> command
