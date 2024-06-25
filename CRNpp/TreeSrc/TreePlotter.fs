// @author Simon Janum
// @author August Valentin
// @date 13/6/2024
namespace DrawTrees

module PlotTrees =
    open Plotly.NET
    open Plotly.NET.LayoutObjects // this namespace contains all object abstractions for layout styling

    open Trees


    let mirroredXAxis =
        LinearAxis.init (
            //        Title = Title.init(Text="Mirrored axis"),
            ShowLine = false,
            Mirror = StyleParam.Mirror.False,
            ShowGrid = false
        //        Ticks = StyleParam.TickOptions.Inside
        )

    let mirroredYAxis =
        LinearAxis.init (
            //        Title = Title.init(Text="Log axis"),
            //        AxisType = StyleParam.AxisType.Log,
            ShowLine = false,
            Mirror = StyleParam.Mirror.False,
            ShowGrid = false
        )

    let addPoint p name seq =
        let pSeq =
            Seq.singleton (

                Chart.Point(
                    [ p ],
                    MultiText = [ name ],
                    MultiTextPosition = [ StyleParam.TextPosition.MiddleCenter ],
                    ShowLegend = false

                )
                |> Chart.withMarkerStyle (Opacity = 0.0) // Make the marker invisible
            )


        Seq.append seq pSeq

    let addLine (x1, y1) (x2, y2) seq =
        let lSeq =
            Seq.singleton (Chart.Line([ x1; x2 ], [ y1; y2 ], LineColor = Color.fromString "green", ShowLegend = false))

        Seq.append seq lSeq

    let formatter a sep =

        let s = string a
        let l = (Seq.toList s)
        let charL = List.splitInto (max (l.Length / sep) 1) l
        List.map (fun elem -> System.String(elem |> List.toArray)) charL

    let drawTreeSimple t =
        let rec drawTreeChildren tree parent =
            let (parentX, parentY, parentOffset) = parent

            match tree with
            | Node((a, v), []) ->
                let formatted = (formatter a 7)

                let offset =
                    if (float formatted.Length) = 1 then
                        0.1
                    else
                        (float formatted.Length) * 0.08

                let seq =
                    addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) Seq.empty

                addLine (parentX, parentY - parentOffset) (parentX + v, parentY - 1.0 + offset) seq


            | Node((a, v), st) ->
                let formatted = (formatter a 7)

                let offset =
                    if (float formatted.Length) = 1 then
                        0.1
                    else
                        (float formatted.Length) * 0.08

                let seq =
                    addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) Seq.empty

                let seq =
                    addLine (parentX, parentY - parentOffset) (parentX + v, parentY - 1.0 + offset) seq

                List.fold
                    (fun acc elem -> Seq.append acc (drawTreeChildren elem (parentX + v, parentY - 1.0, offset)))
                    seq
                    st

        // Starting at root - shouldnt draw a line
        match t with
        | Node((a, v), []) ->
            let formatted = (formatter a 7)
            addPoint (0.0, 0.0) (String.concat "<br>" formatted) Seq.empty
        | Node((a, v), st) ->
            let formatted = (formatter a 7)

            let offset =
                if (float formatted.Length) = 1 then
                    0.1
                else
                    (float formatted.Length) * 0.08

            let seq = addPoint (0.0, 0.0) (String.concat "<br>" formatted) Seq.empty
            List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (0.0, 0.0, offset))) seq st

    let drawTreePrettier t =
        let rec drawTreeChildren tree parent =
            let (parentX, parentY) = parent

            match tree with
            | Node((a, v), []) ->
                let formatted = (formatter a 7)

                let offset =
                    if (float formatted.Length) = 1 then
                        0.1
                    else
                        (float formatted.Length) * 0.08

                let seq =
                    addLine (parentX + v, parentY - 1.0 + offset) (parentX + v, parentY + 0.5 - 1.0) Seq.empty

                addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) seq


            | Node((a, v), st) ->
                let formatted = (formatter a 7)

                let offset =
                    if (float formatted.Length) = 1 then
                        0.1
                    else
                        (float formatted.Length) * 0.08

                let seq =
                    addLine (parentX + v, parentY - 1.0 + offset) (parentX + v, parentY + 0.5 - 1.0) Seq.empty // line up

                let seq =
                    addLine (parentX + v, parentY - 1.0 - offset) (parentX + v, parentY - 0.5 - 1.0) seq // Down line

                let (Node((_, vfirst), _)) = st.Head
                let (Node((_, vlast), _)) = (List.rev st).Head

                let seq =
                    addLine (parentX + v + vfirst, parentY - 1.5) (parentX + v + vlast, parentY - 1.5) seq



                let seq = addPoint (parentX + v, parentY - 1.0) (String.concat "<br>" formatted) seq

                List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (parentX + v, parentY - 1.0))) seq st

        // Starting at root - shouldnt draw a line
        match t with
        | Node((a, v), []) -> addPoint (0.0, 0.0) a Seq.empty
        | Node((a, v), st) ->
            let formatted = (formatter a 7)

            let offset =
                if (float formatted.Length) = 1 then
                    0.1
                else
                    (float formatted.Length) * 0.08

            let seq = addPoint (0.0, 0.0) (String.concat "<br>" formatted) Seq.empty
            let seq = addLine (0.0, 0.0 - offset) (0.0, - 0.5) seq
            let (Node((_, vfirst), _)) = st.Head
            let (Node((_, vlast), _)) = (List.rev st).Head
            let seq = addLine (vfirst, -0.5) (vlast, -0.5) seq
            List.fold (fun acc elem -> Seq.append acc (drawTreeChildren elem (0.0, 0.0))) seq st



    let visualizeTree t b =
        if b then
            drawTreeSimple t
            |> Chart.combine
            |> Chart.withXAxis mirroredXAxis
            |> Chart.withYAxis mirroredYAxis
            |> Chart.show
        else
            drawTreePrettier t
            |> Chart.combine
            |> Chart.withXAxis mirroredXAxis
            |> Chart.withYAxis mirroredYAxis
            |> Chart.withSize (1200.0, 700.0)
            |> Chart.show
