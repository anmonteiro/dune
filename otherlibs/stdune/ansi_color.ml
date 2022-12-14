module Style = struct
  type t =
    | Fg_default
    | Fg_black
    | Fg_red
    | Fg_green
    | Fg_yellow
    | Fg_blue
    | Fg_magenta
    | Fg_cyan
    | Fg_white
    | Fg_bright_black
    | Fg_bright_red
    | Fg_bright_green
    | Fg_bright_yellow
    | Fg_bright_blue
    | Fg_bright_magenta
    | Fg_bright_cyan
    | Fg_bright_white
    | Bg_default
    | Bg_black
    | Bg_red
    | Bg_green
    | Bg_yellow
    | Bg_blue
    | Bg_magenta
    | Bg_cyan
    | Bg_white
    | Bg_bright_black
    | Bg_bright_red
    | Bg_bright_green
    | Bg_bright_yellow
    | Bg_bright_blue
    | Bg_bright_magenta
    | Bg_bright_cyan
    | Bg_bright_white
    | Bold
    | Dim
    | Italic
    | Underline

  let to_ansi_int = function
    | Fg_default -> 39
    | Fg_black -> 30
    | Fg_red -> 31
    | Fg_green -> 32
    | Fg_yellow -> 33
    | Fg_blue -> 34
    | Fg_magenta -> 35
    | Fg_cyan -> 36
    | Fg_white -> 37
    | Fg_bright_black -> 90
    | Fg_bright_red -> 91
    | Fg_bright_green -> 92
    | Fg_bright_yellow -> 93
    | Fg_bright_blue -> 94
    | Fg_bright_magenta -> 95
    | Fg_bright_cyan -> 96
    | Fg_bright_white -> 97
    | Bg_default -> 49
    | Bg_black -> 40
    | Bg_red -> 41
    | Bg_green -> 42
    | Bg_yellow -> 43
    | Bg_blue -> 44
    | Bg_magenta -> 45
    | Bg_cyan -> 46
    | Bg_white -> 47
    | Bg_bright_black -> 100
    | Bg_bright_red -> 101
    | Bg_bright_green -> 102
    | Bg_bright_yellow -> 103
    | Bg_bright_blue -> 104
    | Bg_bright_magenta -> 105
    | Bg_bright_cyan -> 106
    | Bg_bright_white -> 107
    | Bold -> 1
    | Dim -> 2
    | Italic -> 3
    | Underline -> 4

  (* TODO use constructor names *)
  let to_dyn s = Dyn.int (to_ansi_int s)

  module Of_ansi_code = struct
    type nonrec t =
      | Clear
      | Unknown
      | Style of t
  end

  let of_ansi_code : int -> Of_ansi_code.t = function
    | 39 -> Style Fg_default
    | 30 -> Style Fg_black
    | 31 -> Style Fg_red
    | 32 -> Style Fg_green
    | 33 -> Style Fg_yellow
    | 34 -> Style Fg_blue
    | 35 -> Style Fg_magenta
    | 36 -> Style Fg_cyan
    | 37 -> Style Fg_white
    | 90 -> Style Fg_bright_black
    | 91 -> Style Fg_bright_red
    | 92 -> Style Fg_bright_green
    | 93 -> Style Fg_bright_yellow
    | 94 -> Style Fg_bright_blue
    | 95 -> Style Fg_bright_magenta
    | 96 -> Style Fg_bright_cyan
    | 97 -> Style Fg_bright_white
    | 49 -> Style Bg_default
    | 40 -> Style Bg_black
    | 41 -> Style Bg_red
    | 42 -> Style Bg_green
    | 43 -> Style Bg_yellow
    | 44 -> Style Bg_blue
    | 45 -> Style Bg_magenta
    | 46 -> Style Bg_cyan
    | 47 -> Style Bg_white
    | 100 -> Style Bg_bright_black
    | 101 -> Style Bg_bright_red
    | 102 -> Style Bg_bright_green
    | 103 -> Style Bg_bright_yellow
    | 104 -> Style Bg_bright_blue
    | 105 -> Style Bg_bright_magenta
    | 106 -> Style Bg_bright_cyan
    | 107 -> Style Bg_bright_white
    | 1 -> Style Bold
    | 2 -> Style Dim
    | 3 -> Style Italic
    | 4 -> Style Underline
    | 0 -> Clear
    | _ -> Unknown

  let is_not_fg = function
    | Fg_default
    | Fg_black
    | Fg_red
    | Fg_green
    | Fg_yellow
    | Fg_blue
    | Fg_magenta
    | Fg_cyan
    | Fg_white
    | Fg_bright_black
    | Fg_bright_red
    | Fg_bright_green
    | Fg_bright_yellow
    | Fg_bright_blue
    | Fg_bright_magenta
    | Fg_bright_cyan
    | Fg_bright_white -> false
    | _ -> true

  let is_not_bg = function
    | Bg_default
    | Bg_black
    | Bg_red
    | Bg_green
    | Bg_yellow
    | Bg_blue
    | Bg_magenta
    | Bg_cyan
    | Bg_white
    | Bg_bright_black
    | Bg_bright_red
    | Bg_bright_green
    | Bg_bright_yellow
    | Bg_bright_blue
    | Bg_bright_magenta
    | Bg_bright_cyan
    | Bg_bright_white -> false
    | _ -> true

  let escape_sequence l =
    let l = "0" :: List.map l ~f:(fun s -> string_of_int (to_ansi_int s)) in
    Printf.sprintf "\027[%sm" (String.concat l ~sep:";")

  let escape_sequence_no_reset l =
    let l = List.map l ~f:(fun s -> string_of_int (to_ansi_int s)) in
    Printf.sprintf "\027[%sm" (String.concat l ~sep:";")
end

let supports_color fd =
  let is_smart =
    match Stdlib.Sys.getenv "TERM" with
    | exception Not_found -> false
    | "dumb" -> false
    | _ -> true
  and clicolor =
    match Stdlib.Sys.getenv "CLICOLOR" with
    | exception Not_found -> true
    | "0" -> false
    | _ -> true
  and clicolor_force =
    match Stdlib.Sys.getenv "CLICOLOR_FORCE" with
    | exception Not_found -> false
    | "0" -> false
    | _ -> true
  in
  (is_smart && Unix.isatty fd && clicolor) || clicolor_force

let stdout_supports_color = lazy (supports_color Unix.stdout)

let stderr_supports_color = lazy (supports_color Unix.stderr)

let output_is_a_tty = lazy (Unix.isatty Unix.stderr)

let rec tag_handler current_styles ppf styles pp =
  Format.pp_print_as ppf 0 (Style.escape_sequence_no_reset styles);
  Pp.to_fmt_with_tags ppf pp
    ~tag_handler:(tag_handler (current_styles @ styles));
  Format.pp_print_as ppf 0 (Style.escape_sequence current_styles)

let make_printer supports_color ppf =
  let f =
    lazy
      (if Lazy.force supports_color then
       Pp.to_fmt_with_tags ppf ~tag_handler:(tag_handler [])
      else Pp.to_fmt ppf)
  in
  Staged.stage (fun pp ->
      Lazy.force f pp;
      Format.pp_print_flush ppf ())

let print =
  Staged.unstage (make_printer stdout_supports_color Format.std_formatter)

let prerr =
  Staged.unstage (make_printer stderr_supports_color Format.err_formatter)

let strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop start i =
    if i = len then (
      if i - start > 0 then Buffer.add_substring buf str start (i - start);
      Buffer.contents buf)
    else
      match String.unsafe_get str i with
      | '\027' ->
        if i - start > 0 then Buffer.add_substring buf str start (i - start);
        skip (i + 1)
      | _ -> loop start (i + 1)
  and skip i =
    if i = len then Buffer.contents buf
    else
      match String.unsafe_get str i with
      | 'm' -> loop (i + 1) (i + 1)
      | _ -> skip (i + 1)
  in
  loop 0 0

let index_from_any str start chars =
  let n = String.length str in
  let rec go i =
    if i >= n then None
    else
      match List.find chars ~f:(fun c -> Char.equal str.[i] c) with
      | None -> go (i + 1)
      | Some c -> Some (i, c)
  in
  go start

let parse_line str styles =
  let len = String.length str in
  let add_chunk acc ~styles ~pos ~len =
    if len = 0 then acc
    else
      let s = Pp.verbatim (String.sub str ~pos ~len) in
      let s =
        match styles with
        | [] -> s
        | _ -> Pp.tag styles s
      in
      Pp.seq acc s
  in
  let rec loop (styles : Style.t list) i acc =
    match String.index_from str i '\027' with
    | None -> (styles, add_chunk acc ~styles ~pos:i ~len:(len - i))
    | Some seq_start -> (
      let acc = add_chunk acc ~styles ~pos:i ~len:(seq_start - i) in
      (* Skip the "\027[" *)
      let seq_start = seq_start + 2 in
      if seq_start >= len || str.[seq_start - 1] <> '[' then (styles, acc)
      else
        match index_from_any str seq_start [ 'm'; 'K' ] with
        | None -> (styles, acc)
        | Some (seq_end, 'm') ->
          let styles =
            if seq_start = seq_end then
              (* Some commands output "\027[m", which seems to be interpreted
                 the same as "\027[0m" by terminals *)
              []
            else
              String.sub str ~pos:seq_start ~len:(seq_end - seq_start)
              |> String.split ~on:';'
              |> List.fold_left ~init:(List.rev styles) ~f:(fun styles s ->
                     match Int.of_string s with
                     | None -> styles
                     | Some code -> (
                       match Style.of_ansi_code code with
                       | Clear -> []
                       | Unknown -> styles
                       (* If the foreground is set to default, we filter out any
                          other foreground modifiers. Same for background. *)
                       | Style Fg_default ->
                         List.filter styles ~f:Style.is_not_fg
                       | Style Bg_default ->
                         List.filter styles ~f:Style.is_not_bg
                       | Style s -> s :: styles))
              |> List.rev
          in
          loop styles (seq_end + 1) acc
        | Some (seq_end, _) -> loop styles (seq_end + 1) acc)
  in
  loop styles 0 Pp.nop

let parse =
  let rec loop styles lines acc =
    match lines with
    | [] -> Pp.vbox (Pp.concat ~sep:Pp.cut (List.rev acc))
    | line :: lines ->
      let styles, pp = parse_line line styles in
      loop styles lines (pp :: acc)
  in
  fun str -> loop [] (String.split_lines str) []
