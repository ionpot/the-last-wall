let log = Log.fopen ()

let ln () =
  Log.ln log;
  print_newline ()

let write str =
  Log.write log str;
  print_string str

let writeln str =
  Log.writeln log str;
  print_endline str

let writelns = List.iter writeln

let pairln s1 s2 =
  writeln (s1 ^ ": " ^ s2)

let ifpairln s1 s2 =
  if s2 <> "" then pairln s1 s2

let spln s1 s2 =
  writeln (s1 ^ " " ^ s2)

let lnwrite str =
  ln (); write str

let lnwriteln str =
  ln (); writeln str

let readln () =
  let line = read_line () in
  Log.writeln log line;
  line

let prompt str =
  write (str ^ "> ");
  readln ()

let lnprompt str =
  ln (); prompt str

let prompt_int str =
  try int_of_string (prompt str)
  with Failure _ -> 0

let prompt_yn str =
  prompt str = "y"

let flush () =
  Log.flush log

let fin () =
  Log.fclose log
