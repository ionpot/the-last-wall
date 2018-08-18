open Printf

let lf = "\r\n"

let fopen () =
  open_out "log.txt"

let fclose =
  close_out

let write =
  output_string

let ln file =
  write file lf

let writeln file str =
  write file (str ^ lf)

let lnwrite file str =
  write file (lf ^ str)

let lnwriteln file str =
  lnwrite file (str ^ lf)

let flush = flush
