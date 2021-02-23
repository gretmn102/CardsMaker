module Index

open Elmish
open Feliz
open Browser
type EscapeOptions =
    {
        EscapeForMarkdown: bool
        DisplayInTranslatedBox: bool
    }

/// Поскольку `Size` для всех клеток общий, то незачем прикручивать его к каждому элементу
type Card =
    {
        Location: int * int
        Scale: float
    }
type Img =
    {
        Img: Types.HTMLImageElement option
        HorCardsCount: int
        VerCardsCount: int
        CardSize: int * int

        // Rectangle: {| X:int; Y:int; Height:int; Width:int |}
        Cards: Card []
    }

type State =
    {
        InputImageSrc: string
        ImageSrc: string
        Img:Img
    }

type Msg =
    | SetImageSrc
    | InputImageSrc of string
    | SetImage of Img
// open Zanaptak.TypedCssClasses
open Fable.Core

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
// open Fulma.Extensions.Wikiki

let rgb (r:byte, g:byte, b:byte) =
    U3.Case1 (sprintf "rgb(%d, %d, %d)" r g b)

module Draw =
    // open System.Drawing

    let gen' step count width =
        List.unfold (fun (st, i) ->
            if i > 0 then
                let st' = st + width - 1
                Some ((st,st'), (st' + 1 + step, i - 1))
            else None) (0, count)
    // gen' 0 10 10
    let gen step count width =
        List.unfold (fun (st, i) ->
            if i > 0 then
                let st' = st + width - 1
                Some (st, (st' + 1 + step, i - 1))
            else None) (0, count)
    // assert
        // gen 3 4 4
    /// Разбивает изображение на ячейки изображений. Например, при `cellCountHor, cellCountVer = 3, 4`, получим такой вид:
    /// ```
    /// |0 |1 |2 |
    /// |3 |4 |5 |
    /// |6 |7 |8 |
    /// |9 |10|11|
    /// ```
    /// Размерность каждой ячейки известна, а расположение отдельно взятой ячейки начинается из верхнего левого угла.
    /// Если добавить `imgWidth, imgHeight = 30, 40`, то в итоге получится это:
    /// ```
    /// ((10, 10),
    ///  [
    ///       (0, (0, 0));  (1, (10, 0));   (2, (20, 0));
    ///       (3, (0, 10)); (4, (10, 10));  (5, (20, 10));
    ///       (6, (0, 20)); (7, (10, 20));  (8, (20, 20));
    ///       (9, (0, 30)); (10, (10, 30)); (11, (20, 30))
    ///  ])
    /// ```
    let splitToCells (imgWidth, imgHeight) (cellCountHor, cellCountVer) =
        let cellW = imgWidth / cellCountHor
        let cellH = imgHeight / cellCountVer
        let ws = gen 0 cellCountHor cellW
        let hs = gen 0 cellCountVer cellH
        let m =
            hs |> List.fold (fun st y ->
                ws |> List.fold (fun (m,i) x ->
                    Map.add i (x, y) m, i + 1) st ) (Map.empty, 0)
        (cellW, cellH), fst m

    // let splitToCellsTest () =
    //     let act = splitToCells (30, 40) (3, 4) |> mapSnd Map.toList
    //     let exp =
    //         ((10, 10),
    //          [(0, (0, 0)); (1, (10, 0)); (2, (20, 0)); (3, (0, 10)); (4, (10, 10));
    //           (5, (20, 10)); (6, (0, 20)); (7, (10, 20)); (8, (20, 20)); (9, (0, 30));
    //           (10, (10, 30)); (11, (20, 30))])
    //     exp = act

    // let splitToCellsImg (cellCountHor, cellCountVer) (path:string) =
    //     let bmpSource = new Bitmap(path)
    //     let (wDest, hDest) as size, positions = splitToCells (bmpSource.Width, bmpSource.Height) (cellCountHor, cellCountVer)
    //     let xs =
    //         positions
    //         |> Map.map (fun _ (x, y) ->
    //             Rectangle(x, y, wDest, hDest)
    //         )
    //     bmpSource, size, xs

    let sizeCellDef widthGridEdge lenDst count =
        let x = lenDst + widthGridEdge - count * widthGridEdge
        match x % count with 0 -> () | x -> failwithf "%d" x
        x / count
    let lenDstDef widthGridEdge sizeCell count =
        sizeCell * count + (count - 1) * widthGridEdge + widthGridEdge * 2
    // let drawGrid (gridColor:Color) gridEdgeWidth (cellWidth, cellHeight) (bmp:Bitmap) =
    //     // let size = 3
    //     // let step = 5
    //     // let x0 = size + step/2
    //     // let x1 = x0 + size + step
    //     // let x2 = x1 + size + step
    //     // |0|1|2|3|4|5|6|7|8|9|0|1|2|3|4|5
    //     // |_|_|^|_|_|^|_|_|^|               // 2; 5
    //     // |_|_|+|^|+|_|_|+|^|+|_|_|+|^|     // 3; 8; 13
    //     // |_|_|+|+|^|+|+|_|_|+|+|^|+|+|_|_| // 4; 11; 18
    //     // |_|+|+|^|+|+|_|+|+|^|+|+|_|       // 3; 9;

    //     // let size = 3
    //     // let step = 5
    //     // let x0 = size / 2
    //     // let x1 = x0 + size + step
    //     // let x2 = x1 + size + step
    //     // |0|1|2|3|4|5|6|7|8|9|0|1|2|3|4|5|6|7|8|9|0
    //     // |^|_|_|^|_|_|^|_|_|^|                       // size = 1; step = 2; [0; 3; 6; 9]
    //     // |+|^|+|_|_|+|^|+|_|_|+|^|+|_|_|+|^|         // size = 3; step = 2; [1; 6; 11; 16]
    //     // |+|+|^|+|+|_|_|+|+|^|+|+|_|_|+|+|^|+|+|_|_| // size = 5; step = 2; [2; 9; 16; 23]
    //     // |+|+|^|+|+|_|+|+|^|+|+|_|+|+|^|+|+|_|+|+|^| // size = 5; step = 1; [2; 8; 14; 20]
    //     let lines size step count =
    //         Seq.unfold (fun (st,count) ->
    //                 if count > 0 then
    //                     Some(st, (st + size + step, count - 1))
    //                 else None)
    //             // (size + step/2, count)
    //             (size / 2, count)
    //     // lines 5 1 4
    //     use g = Graphics.FromImage bmp
    //     use pen = new Pen(gridColor, float32 gridEdgeWidth)

    //     lines gridEdgeWidth cellWidth (bmp.Width / cellWidth + 1)
    //     |> Seq.iter (fun x -> g.DrawLine(pen, x, 0, x, bmp.Height))
    //     lines gridEdgeWidth cellHeight (bmp.Height / cellHeight + 1)
    //     |> Seq.iter (fun x -> g.DrawLine(pen, 0, x, bmp.Width, x))


module GraphicsEngine =
    let mutable fOffsetX, fOffsetY = 0., 0.
    let mutable fScaleX, fScaleY = 1., 1.

    let WorldToScreen (fWorldX, fWorldY) =
        let nScreenX = ((fWorldX - fOffsetX) * fScaleX)
        let nScreenY = ((fWorldY - fOffsetY) * fScaleY)
        nScreenX, nScreenY
    let ScreenToWorld (nScreenX, nScreenY) =
        let fWorldX = (nScreenX / fScaleX) + fOffsetX
        let fWorldY = (nScreenY / fScaleY) + fOffsetY
        fWorldX, fWorldY

    let mutable fMouseX, fMouseY = 0., 0.
    let mutable mouseButtonHeld = false

    let mutable fStartPanX, fStartPanY = 0., 0.
    let mutable qKeyHeld = false
    let mutable aKeyHeld = false

    let update (canvas:Types.HTMLCanvasElement) =
        canvas.onmousemove <- (fun e ->
            fMouseX <- e.offsetX
            fMouseY <- e.offsetY
        )

        canvas.onmousedown <- (fun e ->
            mouseButtonHeld <- true
        )
        canvas.onmouseup <- (fun e ->
            mouseButtonHeld <- false
        )
        canvas.onmouseleave <- (fun e ->
            mouseButtonHeld <- false
        )

        canvas.onkeydown <- (fun e ->
            printfn "%A" e.key
            match e.code with
            | "KeyQ" -> qKeyHeld <- true
            | "KeyA" -> aKeyHeld <- true
            | _ -> ()
        )
        canvas.onkeyup <- (fun e ->
            match e.code with
            | "KeyQ" -> qKeyHeld <- false
            | "KeyA" -> aKeyHeld <- false
            | _ -> ()
        )

        Mainloop.mainloop.setUpdate (fun fElapsedTime ->
            if mouseButtonHeld then
                // printfn "%A" (fOffsetX, fOffsetY)
                fOffsetX <- fOffsetX - ((fMouseX - fStartPanX) / fScaleX)
                fOffsetY <- fOffsetY - ((fMouseY - fStartPanY) / fScaleY)
                fStartPanX <- fMouseX
                fStartPanY <- fMouseY
            else
                fStartPanX <- fMouseX
                fStartPanY <- fMouseY

            let fMouseWorldX_BeforeZoom, fMouseWorldY_BeforeZoom = ScreenToWorld(fMouseX, fMouseY)

            if qKeyHeld then
                // printfn "%A" fScaleX
                fScaleX <- fScaleX * 1.01
                fScaleY <- fScaleY * 1.01
            if aKeyHeld then
                fScaleX <- fScaleX * 0.99
                fScaleY <- fScaleY * 0.99

            let fMouseWorldX_AfterZoom, fMouseWorldY_AfterZoom = ScreenToWorld(fMouseX, fMouseY)

            fOffsetX <- fOffsetX + (fMouseWorldX_BeforeZoom - fMouseWorldX_AfterZoom)
            fOffsetY <- fOffsetY + (fMouseWorldY_BeforeZoom - fMouseWorldY_AfterZoom)
        )
        |> ignore


    let mutable (img:Img option) = None


    let draw (canvas:Types.HTMLCanvasElement) =
        let canvasCtx = canvas.getContext_2d()

        Mainloop.mainloop.setDraw (fun interpolationPercentage ->
            canvasCtx.clearRect(0., 0., canvas.width, canvas.height)

            let fWorldLeft, fWorldTop = ScreenToWorld(0., 0.)
            let fWorldRight, fWorldBottom = ScreenToWorld(canvas.width, canvas.height)

            let gridColor = U3.Case1 "red"
            // Вообще, если делать по уму, то сперва нужно разбить
            let shift = 10.
            let mutable cardsRenderedCount = 0
            match img with
            | Some img ->
                match img.Img with
                | Some img1 ->
                    img.Cards
                    |> Array.iter (fun card ->
                        let sx, sy = card.Location
                        let sx, sy = float sx, float sy
                        let srcW, srcH = img.CardSize
                        let srcW, srcH = float srcW, float srcH
                        let sx2, sy2 = sx + srcW, sy + srcH
                        if ((fWorldLeft <= sx && sx <= fWorldRight)
                            || (fWorldLeft <= sx2 && sx2 <= fWorldRight)
                            || (fWorldLeft >= sx && sx2 >= fWorldRight))
                           &&
                           ((fWorldTop <= sy && sy <= fWorldBottom)
                            || (fWorldTop <= sy2 && sy2 <= fWorldBottom)
                            || (fWorldTop >= sy && sy2 >= fWorldBottom)) then

                            cardsRenderedCount <- cardsRenderedCount + 1
                            let f x = if x = 0.0 then x else x + shift

                            let pixel_sx, pixel_sy = WorldToScreen(f sx, f sy)
                            let pixel_ex, pixel_ey = WorldToScreen(sx2, sy2)

                            canvasCtx.drawImage
                                (U3.Case1 img1, sx, sy, srcW, srcH,
                                 pixel_sx, pixel_sy, pixel_ex - pixel_sx, pixel_ey - pixel_sy)
                    )
                | None -> ()

                ()
            | None -> ()
            // let drawGrid gridEdgeWidth (cellWidth, cellHeight) =
            //     let lines size step count =
            //         Seq.unfold (fun (st,count) ->
            //                 if count > 0 then
            //                     Some(st, (st + size + step, count - 1))
            //                 else None)
            //             // (size + step/2, count)
            //             (size / 2, count)
            //     // lines 5 1 4
            //     // use g = Graphics.FromImage bmp
            //     // use pen = new Pen(gridColor, float32 gridEdgeWidth)
            //     match img with
            //     | Some img ->

            //         lines gridEdgeWidth cellWidth (int canvas.width / cellWidth + 1)
            //         |> Seq.iter (fun x ->
            //             let x = float x
            //             if x >= fWorldLeft && x <= fWorldRight then
            //                 // g.DrawLine(pen, x, 0, x, bmp.Height)
            //                 let sx = x
            //                 let sy = 0.0
            //                 let ex = x
            //                 let ey = img.height

            //                 let pixel_sx, pixel_sy = WorldToScreen(sx, sy)
            //                 let pixel_ex, pixel_ey = WorldToScreen(ex, ey)
            //                 canvasCtx.strokeStyle <- gridColor

            //                 canvasCtx.beginPath()
            //                 canvasCtx.moveTo (pixel_sx, pixel_sy)
            //                 canvasCtx.lineTo (pixel_ex, pixel_ey)
            //                 canvasCtx.stroke()
            //         )

            //         lines gridEdgeWidth cellHeight (int canvas.height / cellHeight + 1)
            //         |> Seq.iter (fun y ->
            //             let y = float y

            //             // g.DrawLine(pen, 0, x, bmp.Width, x)
            //             if y >= fWorldTop && y <= fWorldBottom then
            //                 let sx = 0.0
            //                 let sy = y
            //                 let ex = img.width
            //                 let ey = y

            //                 let pixel_sx, pixel_sy = WorldToScreen(sx, sy)
            //                 let pixel_ex, pixel_ey = WorldToScreen(ex, ey)
            //                 canvasCtx.strokeStyle <- gridColor

            //                 canvasCtx.beginPath()
            //                 canvasCtx.moveTo (pixel_sx, pixel_sy)
            //                 canvasCtx.lineTo (pixel_ex, pixel_ey)
            //                 canvasCtx.stroke()
            //         )
            //     | None -> ()

            let mutable linesRenderedCount = 0
            for y in 0. .. 10. do
                if y >= fWorldTop && y <= fWorldBottom then
                    let sx = 0.0
                    let sy = y
                    let ex = 10.0
                    let ey = y

                    let pixel_sx, pixel_sy = WorldToScreen(sx, sy)
                    let pixel_ex, pixel_ey = WorldToScreen(ex, ey)
                    canvasCtx.strokeStyle <- gridColor

                    canvasCtx.beginPath()
                    canvasCtx.moveTo (pixel_sx, pixel_sy)
                    canvasCtx.lineTo (pixel_ex, pixel_ey)
                    canvasCtx.stroke()
                    linesRenderedCount <- linesRenderedCount + 1
            for x in 0. .. 10. do
                if x >= fWorldLeft && x <= fWorldRight then
                    let sx = x
                    let sy = 0.0
                    let ex = x
                    let ey = 10.0

                    let pixel_sx, pixel_sy = WorldToScreen(sx, sy)
                    let pixel_ex, pixel_ey = WorldToScreen(ex, ey)
                    canvasCtx.strokeStyle <- gridColor

                    canvasCtx.beginPath()
                    canvasCtx.moveTo (pixel_sx, pixel_sy)
                    canvasCtx.lineTo (pixel_ex, pixel_ey)
                    canvasCtx.stroke()
                    linesRenderedCount <- linesRenderedCount + 1

            // match img with
            // | Some img ->
            //     let x, y = WorldToScreen(0., 0.)
            //     canvasCtx.drawImage(U3.Case1 img, x, y, img.width * fScaleX, img.height * fScaleY)
            // | None -> ()

            canvasCtx.fillStyle <- U3.Case1 "red"
            canvasCtx.font <- "20px Georgia"

            canvasCtx.fillText(sprintf "%A, counts = %A, cardsRenderedCount = %A" fScaleX linesRenderedCount cardsRenderedCount, 0., 35.)
            canvasCtx.fillText(sprintf "%A" (fOffsetX, fOffsetY), 0., 35. + 35.)
            canvasCtx.fillText(sprintf "%A" (fWorldLeft, fWorldTop, fWorldRight, fWorldBottom), 0., 35. + 35. + 35.)
            match img with
            | Some img ->
                canvasCtx.fillText(sprintf "%A" img.Cards.Length, 0., 35. + 35. + 35. + 35.)
            | None -> ()
            // printf "%A" cells
        )
        |> ignore

    let start (canvas:Types.HTMLCanvasElement) =
        // fOffsetX <- -canvas.width / 2.
        // fOffsetY <- -canvas.height / 2.
        update canvas
        draw canvas
        Mainloop.mainloop.start()

let init () =
    let st =
        {
            InputImageSrc = ""
            ImageSrc = ""
            // HorCardsCount = 1
            // VerCardsCount = 1
            Img =
                {
                    Img = None
                    HorCardsCount = 1
                    VerCardsCount = 1
                    CardSize = 0, 0
                    Cards = [||]
                }
        }
    st, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | InputImageSrc src ->
        let state = { state with InputImageSrc = src }
        state, Cmd.none
    | SetImageSrc ->
        let state =
            { state with
                ImageSrc = state.InputImageSrc }
        state, Cmd.OfFunc.result (SetImage { state.Img with Img = None })
    | SetImage img ->
        GraphicsEngine.img <- Some img

        let state = { state with Img = img }
        state, Cmd.none
let containerBox (state : State) (dispatch : Msg -> unit) =
    Box.box' [] [
        if not <| System.String.IsNullOrEmpty state.ImageSrc then
            Html.img [
                prop.hidden true
                prop.src state.ImageSrc

                prop.onLoadedData (fun e ->
                    printfn "onLoadedData %A" e
                )
                prop.ref (fun e ->
                    if isNull e then ()
                    else
                        let img = e :?> Types.HTMLImageElement

                        let img1 = state.Img
                        match img1.Img with
                        | Some _ -> ()
                        | None ->
                            let cellSize, cells =
                                Draw.splitToCells (int img.width, int img.height)
                                    (img1.HorCardsCount, img1.VerCardsCount)

                            let img =
                                { img1 with
                                    Img = Some img
                                    CardSize = cellSize
                                    Cards =
                                        cells
                                        |> Map.toArray
                                        |> Array.map (fun (k, v) ->
                                            {
                                                Location = v
                                                Scale = 1.0
                                            }
                                        )
                                }
                            SetImage img
                            |> dispatch
                )
            ]

        Columns.columns [] [
            Column.column [
            ] [
                Html.canvas [
                    let w, h = 640, 480
                    prop.width w // img.width
                    prop.height h // img.height

                    prop.tabIndex -1
                    prop.ref (fun x ->
                        match x :?> Types.HTMLCanvasElement with
                        | null -> ()
                        | canvas ->
                            GraphicsEngine.start canvas |> ignore
                    )
                ]
            ]
            Column.column [
            ] [
                Column.column [] [
                    Field.div [ Field.HasAddons;  ] [
                        Control.p [ Control.IsExpanded ] [
                            Input.input [
                                Input.Placeholder "Image url"
                                Input.Value state.InputImageSrc
                                Input.OnChange (fun e ->
                                    e.Value
                                    |> InputImageSrc
                                    |> dispatch
                                )
                            ]
                        ]
                        Control.p [] [
                            Button.button [
                                Button.OnClick (fun e ->
                                    SetImageSrc
                                    |> dispatch
                                )
                            ] [
                                Fa.i [ Fa.Solid.Check ] []
                            ]
                        ]
                    ]
                ]
                let f description st cmd =
                    Column.column [] [
                        Html.div [ str description ]
                        Control.p [ Control.IsExpanded ] [
                            Input.number [
                                Input.Placeholder description

                                Input.Value (string st)
                                Input.OnChange (fun e ->
                                    match System.Int32.TryParse e.Value with
                                    | false, _ -> ()
                                    | true, x ->
                                        if x > 0 then
                                            x
                                            |> cmd
                                            |> SetImage
                                            |> dispatch
                                )
                            ]
                        ]
                    ]

                f "Number of cards horizontally" state.Img.HorCardsCount
                    (fun x ->
                        { state.Img with
                            Img = None
                            HorCardsCount = x }
                    )

                f "Number of cards vertically" state.Img.VerCardsCount
                    (fun x ->
                        { state.Img with
                            Img = None
                            VerCardsCount = x }
                    )
            ]
        ]
    ]

let navBrand =
    Navbar.Brand.div [] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/fable.ico"
                Alt "Logo"
            ]
        ]
    ]

let view (state : State) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.IsFullHeight
    ] [
        Hero.head [] [
            Navbar.navbar [] [
                Container.container [] [
                    navBrand
                ]
            ]
        ]

        Hero.body [] [
            Container.container [] [
                Column.column [
                    // Column.Width (Screen.All, Column.Is6)
                    // Column.Offset (Screen.All, Column.Is3)
                ] [
                    // Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "CardsMaker" ]
                    containerBox state dispatch
                ]
            ]
        ]
    ]