open Printf

let lf = "\r\n"

let get_fname () =
  let t = Unix.(time () |> localtime) in
  sprintf "%d-%02d-%02d %02d:%02d:%02d.txt"
    (t.tm_year + 1900) t.tm_mon t.tm_mday
    t.tm_hour t.tm_min t.tm_sec

let fopen () =
  let name = get_fname () in
  open_out name

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
