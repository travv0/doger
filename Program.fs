module Main

open Love

let tileSize = 32f

let (windowWidth, windowHeight) = (640, 480)

type Doge() =
    inherit Scene()

    let mutable x, y =
        float32 windowWidth / 2f, float32 windowHeight - tileSize

    let image =
        Graphics.NewImage("media/sprites/doge.png")

    let moveSpeed = 32

    override __.Draw() =
        Graphics.Draw(image, x, y, 0f, 1f, 1f, 0f, 0f)

    override __.KeyPressed(key, _, _) =
        match key with
        | KeyConstant.Right -> x <- x + tileSize
        | KeyConstant.Left -> x <- x - tileSize
        | KeyConstant.Up -> y <- y - tileSize
        | KeyConstant.Down -> y <- y + tileSize
        | _ -> ()

type Playing() =
    inherit Scene()

    let doge = Doge()

    override __.Draw() = doge.Draw()

    override __.KeyPressed(key, scancode, isRepeat) =
        doge.KeyPressed(key, scancode, isRepeat)

type Program() =
    inherit Scene()

    let mutable state = Playing() :> Scene

    override __.Draw() = state.Draw()

    override __.KeyPressed(key, scancode, isRepeat) =
        state.KeyPressed(key, scancode, isRepeat)

[<EntryPoint>]
let main _ =
    Boot.Init(BootConfig(WindowTitle = "Doger", WindowWidth = windowWidth, WindowHeight = windowHeight))
    Boot.Run(Program())

    0
