module internal Dictionary

    type Dictionary =
        | Leaf of bool
        | Node of bool * Map<char, Dictionary>

    let (|??|) a b =    
        match a with
        | Some a -> a
        | None -> b

    let empty () = Leaf false

    let anchor = "#"

    // let rec insert (s: string) (d: Dictionary) =
    //     match d with
    //     | Leaf _ when s = "" -> Leaf true
    //     | Node (_, m) when s = "" -> Node (true, m)
    //     | Leaf b -> Node (b, Map.empty |> Map.add (s.Chars(0)) (insert (s.Substring(1, s.Length-1)) (empty ())))
    //     | Node (b, m) -> Node (b, m.Add (s.Chars(0), insert (s.Substring(1, s.Length-1)) ((m.TryFind (s.Chars(0))) |??| (empty ()))))

    let rec insert (s: string) (d: Dictionary) =
        
        let words = 
            [for i in 0..(s.Length-1) -> 
                (s.Substring(0,i+1) |> Array.ofSeq |> Array.rev |> System.String) + $"{anchor}{s[i+1..]}"
            ]
        
        let rec aux (s: string) (d: Dictionary) =
            match d with
            | Leaf _ when s = "" -> Leaf true
            | Node (_, m) when s = "" -> Node (true, m)
            | Leaf b -> Node (b, Map.empty |> Map.add (s.Chars(0)) (aux (s.Substring(1, s.Length-1)) (empty ())))
            | Node (b, m) -> Node (b, m.Add (s.Chars(0), aux (s.Substring(1, s.Length-1)) ((m.TryFind (s.Chars(0))) |??| (empty ()))))
                // match m.TryFind (s.Chars(0)) with
                // | Some x -> Node (b, Map.empty |> Map.add (s.Chars(0)) (aux (s.Substring(1, s.Length-1)) x))
                // | None -> Node (b, Map.empty |> Map.add (s.Chars(0)) (aux (s.Substring(1, s.Length-1)) (empty ())))


        List.fold (fun acc word -> aux word acc) d words


    let rec lookup (s: string) (d: Dictionary) =
        match d with
        | Leaf b when s.Length = 0 -> b
        | Leaf _ -> false
        | Node (b, _) when s.Length = 0 -> b
        | Node (_, m) -> lookup (s.Substring(1, s.Length-1)) ((m.TryFind (s.Chars(0))) |??| empty ())

    let step (c: char) (d: Dictionary): (bool * Dictionary) option =
        match d with
        | Leaf _ -> None
        | Node (_, m) -> match m.TryFind c with
                         | Some (Node (b, m1)) -> Some (b, Node (b, m1))
                         | Some (Leaf b) -> Some (b, Leaf b)
                         | None -> None

    //let reverse (c: char) (d: Dictionary)

    let reverse =
        step anchor.[0]