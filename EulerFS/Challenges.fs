module Challenges

/// Sum of all numbers (up to but excluding n) divisible by 3 or 5
let problem1 n =
    seq { for x in 1 .. n - 1 do if x % 3 = 0 || x % 5 = 0 then yield x }
    |> Seq.sum

/// Sum of all even fibonacci numbers (up to and including n)
let problem2 n =
    let rec fibSum x y acc =
        if y < n
        then fibSum y (x + y) (acc +
                if y % 2 = 0
                    then y
                    else 0)
        else acc
    fibSum 0 1 0

/// Largest integer factor of n
let problem3 (n : int64) =
    let rec gpf (n : int64) q =
        let factorSeq = seq { for x in 2L .. (int64 (sqrt((float n)))) + 1L do if n % x = 0L then yield x }
        if Seq.isEmpty factorSeq
            then (max n q)
            else let factor = Seq.head factorSeq
                 gpf (n / factor) (max factor q)
    gpf n 0L