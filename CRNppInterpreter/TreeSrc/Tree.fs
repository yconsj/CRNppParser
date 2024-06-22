// @author Simon Janum
// @author August Valentin
// @date 13/6/2024
namespace DrawTrees

module Trees =

    type 'a Tree = Node of 'a * ('a Tree list)


    let moveTree ((Node((label, x), subTree)), x') = Node((label, x + x'), subTree)


    type Extent = (float * float) list

    let moveextent (e: Extent, x) =
        List.map (fun (p, q) -> (p + x, q + x)) e

    let rec merge (e1: Extent) (e2: Extent) =
        match (e1, e2) with
        | ([], qs) -> qs
        | (ps, []) -> ps
        | ((p, _) :: ps, (_, q) :: qs) -> (p, q) :: merge ps qs


    let mergelist (es: List<Extent>) = List.fold merge [] es


    let rmax (p: float, q: float) = if p > q then p else q
    (* fit: Extent*Extent -> real *)
    let rec fit (e1: Extent) (e2: Extent) =
        match (e1, e2) with
        | ((_, p) :: ps), ((q, _) :: qs) -> rmax ((fit ps qs), (p - q + 1.0))
        | (_, _) -> 0.0

    let fitListl l =
        let rec fitListl' acc (l) =
            match (l) with
            | [] -> []
            | (e :: es) ->
                let x = fit acc e
                x :: fitListl' (merge acc (moveextent (e, x))) es

        fitListl' [] l


    let fitListr l =
        let rec fitListr' acc (l) =
            match (l) with
            | [] -> []
            | (e :: es) ->
                let x = -1.0 * (fit e acc)
                x :: fitListr' (merge (moveextent (e, x)) acc) es

        List.rev (fitListr' [] (List.rev l))

    let mean (x, y) = (x + y) / 2.0

    let fitList es =
        List.map mean (List.zip (fitListl es) (fitListr es))

    let design tree =
        let rec design' (Node(label, subTree)) =
            let (trees, extents) = List.unzip (List.map design' subTree)
            let positions = fitList extents
            let ptrees = List.map moveTree (List.zip trees positions)
            let pextents = List.map moveextent (List.zip extents positions)
            let resultextent = (0.0, 0.0) :: mergelist pextents
            let resulttree = Node((label, 0.0), ptrees)
            (resulttree, resultextent)

        fst (design' tree)
