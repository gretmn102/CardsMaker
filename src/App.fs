module App

open Elmish
open Elmish.React

open Elmish.React

open Index

let init () =
    let st =
        {
            InputImageSrc = "http://localhost:99/https://imgur.com/a/lL2vxsD"
            ImageSrc = "http://localhost:99/https://imgur.com/a/lL2vxsD"
            Img =
                {
                    Img = None
                    HorCardsCount = 5
                    VerCardsCount = 2
                    CardSize = 0, 0
                    Cards = [||]
                    CurrentCardIdx = -1
                }
            SetFullImg = false
        }
    st, Cmd.none


Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run