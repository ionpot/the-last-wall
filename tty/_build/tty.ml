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

let pairln s1 s2 =
  let str = s1 ^ ": " ^ s2 in
  writeln str

let lnwrite str =
  ln (); write str

let lnwriteln str =
  ln (); writeln str

let readln () =
  let line = read_line () in
  Log.writeln log line;
  line

let prompt str =
  let s = str ^ " " in
  write s;
  readln ()

let lnprompt str =
  ln (); prompt str

let prompt_char str =
  prompt str |> Convert.str2char

let lnprompt_char str =
  ln (); prompt_char str

let prompt_yn str =
  prompt_char str |> Convert.char2bool

let flush () =
  Log.flush log

let fin () =
  Log.fclose log
