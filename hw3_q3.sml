datatype Atom = NIL | SYMBOL of string
datatype SExp = ATOM of Atom | CONS of (SExp * SExp)

(* Auxiliary parse function *)
fun parseTokens tokens =
  let
    (* Function to parse a list from tokens *)
    fun parseList [] = (ATOM NIL, [])
      | parseList (")" :: rest) = (ATOM NIL, rest)
      | parseList tokens =
          let
            val (first, remaining) = parseTokens tokens
            val (restList, finalTokens) = parseList remaining
          in
            (CONS(first, restList), finalTokens)
          end

    (* Parse a single S-expression *)
    fun parseSExp ("(" :: rest) = parseList rest
      | parseSExp (sym :: rest) = (ATOM (SYMBOL sym), rest)
  in
    parseSExp tokens
  end

(* Main parse function *)
fun parse tokens =
  let
    val (result, _) = parseTokens tokens
  in
    result
  end





