module internal MultiSet

    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>

    let (|??|) a b =
        match a with
        | Some a -> a
        | None -> b

    let empty = M Map.empty

    let isEmpty (M s) = Map.isEmpty s

    let size (M s) = s |> Map.fold (fun k _ acc -> acc + (uint32 k)) 0u

    let contains a (M s) = Map.containsKey a s

    let numItems a (M s) = Map.tryFind a s |??| 0u

    let add a n (M s) = M (Map.add a (n + ((Map.tryFind a s) |??| 0u)) s)

    let addSingle a (M s) = M (Map.add a (1u + ((Map.tryFind a s) |??| 0u)) s)

    let remove a n (M s) = match ((Map.tryFind a s) |??| 0u) with
                               | x when x > n -> M (Map.add a (x - n) s)
                               | _ -> M (Map.remove a s)

    let removeSingle a (M s) = match ((Map.tryFind a s) |??| 1u) with
                               | x when x > 1u -> M (Map.add a (x - 1u) s)
                               | _ -> M (Map.remove a s)

    let fold f acc (M s) = Map.fold f acc s

    let foldBack f (M s) acc = Map.foldBack f s acc

    let ofList lst = lst |> List.fold (fun acc a -> addSingle a acc) empty

    let toList s =
        s
        |> (fold (fun acc k v -> Array.append acc (Array.create (int v) k)) Array.empty)
        |> List.ofArray
        
    let map f (M s) = (M s) |> fold (fun acc k v -> add (f k) v acc) (M Map.empty)

    let union (M s1) (M s2): MultiSet<'a> =
        s1
        |> Map.fold (fun acc k v -> Map.add k (max v (Map.tryFind k s2 |??| 0u)) acc) s2
        |> M
        
    let sum s1 s2 =
        s1
        |> fold (fun acc k v -> add k v acc) s2
        
    let subtract s1 s2 =
        s2
        |> fold (fun acc k v -> remove k v acc) s1
        
    let intersection s1 s2 =
        subtract s1 s2 |> subtract s1 