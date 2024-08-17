fun to_binary 0 = []
  | to_binary x = (x mod 2) :: to_binary (x div 2);


fun encode list =
    let
        fun countBits vector = List.foldl (fn (x, (zeros, ones)) => if x = 0 then (zeros+1, ones) 
                                                                    else (zeros, ones+1)) (0,0) vector;

        fun flipBit bit = if bit = 0 then 1 else 0;
        val (zeros, ones) = countBits list;

        fun flipHelper ([], _, _, acc, idx) = acc @ to_binary idx
          | flipHelper (x::xs, zeros, ones, acc, idx) =
            if zeros = ones then acc @ (x::xs) @ to_binary idx (* Append remaining bits and last flipped index *)
            else
                let
                    val flipped  = flipBit x;
                    val newZeros = if flipped = 0 then zeros + 1 else zeros - 1;
                    val newOnes  = if flipped = 1 then ones + 1 else ones - 1;
                in
                    flipHelper (xs, newZeros, newOnes, acc @ [flipped], idx + 1)
                end
    in
        flipHelper (list, zeros, ones, [], 0)
    end;



fun decode (list, n) =
    let
        fun to_decimal lst =
            List.foldr (fn (x, acc) => 2 * acc + x) 0 lst;
        val (encodedVector, lastFlippedBit) = List.splitAt (list, n)
        val lastIdx = to_decimal lastFlippedBit
        fun flipBit bit = if bit = 0 then 1 else 0

        fun decodeAux ([], _, _) = []
            | decodeAux (x::xs, idx, lastIdx) =
                if idx <= lastIdx then
                    flipBit x :: decodeAux (xs, idx + 1, lastIdx)
                else
                    x :: decodeAux (xs, idx + 1, lastIdx);
    in
        decodeAux (encodedVector, 1, lastIdx)
    end;
