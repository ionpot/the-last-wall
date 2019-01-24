type ('cond, 'evt, 'input, 'nfy) kind =
  | Check of 'cond
  | Event of 'evt
  | Input of 'input
  | Notify of 'nfy

type ('cond, 'evt, 'input, 'nfy) t =
  | Ask of 'input
  | Do of 'evt
  | JmpIfNo of (t * t)
  | Last
  | Nfy of 'nfy
  | Try of 'cond
