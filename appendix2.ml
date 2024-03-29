(* If you'd rather write `Parser.name` instead of `name`
   for the combinators we've given in the `Parser` module,
   you can remove this `open` statement. This would allow you to use the
   same name for something else without conflicts.
   In the project, that will probably be very useful. *)
open Parser

(* Exercise 1: parse a simple, non-recursive structure. *)

(* We HIGHLY recommend building up your parsers from small pieces,
   which will often require you to write small helper-parsers that
   we didn't ask for and don't test. Test them yourself!
   In this case, we will guide you towards our solution.
   Here are brief descriptions of the helpers we used: *)

(* In a couple of places, we need to check extra conditions.
   Use this given 'guard' combinator to perform such checks.
   See how easy it is to write your own combinators? *)
let guard (ok : bool) : unit parser = if ok then of_value () else fail
(* use like this: guard <condition> |>> ...keep parsing... *)

(* `int_digits` is defined in the prelude and parses an `int`. *)
(* `comma` parses a single ',' character and nothing else. *)
let comma : char parser = satisfy ((=) ',')

(* `text` parses all of the input up to the next ',' character or end of line,
   but not the ',' character. We used `satisfy_many` for this. 
   We also have to be sure that we parsed anything at all, but we've implemented
   this check for you as an example. *)
let allowed_in_text c = c <> ',' && c <> '\n'
let text : string parser = 
  satisfy_many allowed_in_text
  |*> fun result ->
    guard (String.length result > 0) |>
    const_map result

(* `bool` parses either the word "true" or the word "false"
   and returns the corresponding boolean value.
   To do that, we wrote two smaller helpers, for handling each case.
   Those parsers are implemented with `const_map` and `accept_string`. *)
let parse_true : bool parser =
  const_map true (accept_string "true")
let parse_false : bool parser =
  const_map false (accept_string "false")
(* accepts either "true" or "false." Use `first_of_2` for this. *)
let bool : bool parser = 
  first_of_2 parse_true parse_false

(* Now put everything together, to parse syntax like the following:
    55,some text,true
  That is, `run isb "55,some text,true"` should return `(55, "some text", true)` *)
(* Use |*> to assemble the pieces. *)
let isb = 
  int_digits |*> (fun i ->
      comma |>> text |*> (fun s ->
          comma |>> bool |*> (fun b ->
              of_value (i, s, b))))

(** DO NOT Change This Definition *)
let parse_isb = run (between (of_value ()) eof isb)

(* Exercise 2: Parse a CSV. *)

(* csv_int should parse an int, and wrap it up in a `Csv_int` constructor.
   You should use `map` to accomplish this. *)
let csv_int = map (fun i -> Csv_int i) int_digits

(* csv_string is like `text` above, but wraps it up in `Csv_string` constructor.
   You should use both `map` and `text` for this - don't reinvent the wheel! *)
let csv_string = map (fun s -> Csv_string s) text
(* csv_item parses a CSV Item: either an int, or a string.
   Be careful to not parse "5" as `Csv_string "5"`! *)
let csv_item = first_of_2 csv_int csv_string

(* Each field in the CSV is actually optional. How to parse that?
   Why with the parser named `optional` of course! *)
let field = optional csv_item

(* Rows are a little bit more complicated. The _first_ row of a CSV is special,
   and consists of headers for each column. The result of parsing that will be
   a list of header names. How will we get that into our row parser?

   This is one of the strengths of writing parsers this way. We can write our own
   combinator that takes the list of header names and _constructs_ an appropriate
   row parser! *)
(* First, the header: this should be comma-separated `text`. For this, you should
   use the `sep_by_1` function from the Prelude, as well as your `text` and
   `comma` parsers. *)
let header = sep_by_1 text comma

(* Now, other rows: a comma-separated sequence of fields. Then you can combine
   the fields with the names from the header using `List.combine`.
   
   Once you have that working, also ensure that the number of fields matches the
   number of header names. Otherwise, bad input will crash your program! *)

let row header_names = 
  let fields_parser = sep_by_1 field comma in
  fields_parser |*> fun fields ->
    match List.length header_names = List.length fields with
    | true -> of_value (List.combine header_names fields)
    | false -> fail (* Handle the error as appropriate for your library *)

(* Finally, put it together into a CSV parser. A CSV is a header, followed
   by many newlines and rows. *)
let csv = header |*> fun headers ->
    many (newline |>> row headers) |*>  of_value 

(** DO NOT Change This Definition *)
let parse_csv = run (between (of_value ()) eof csv)


