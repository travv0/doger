module Main

open Love

let tileSize = 32f

let (windowWidth, windowHeight) = (640, 480)

[<AbstractClass>]
type Object(x: float32, y: float32, sprite: Drawable) =
    inherit Scene()
    member val X = x with get, set
    member val Y = y with get, set
    member __.Sprite = sprite

    default this.Draw() =
        Graphics.Draw(this.Sprite, this.X, this.Y, 0f, 1f, 1f, 0f, 0f)

type Car(x, y, moveSpeed) =
    inherit Object(x, y, Graphics.NewImage("media/sprites/doge.png"))

    override car.Update(dt) = car.X <- car.X + moveSpeed * dt

type Doge() =
    inherit Object(float32 windowWidth / 2f,
                   float32 windowHeight - tileSize,
                   Graphics.NewImage("media/sprites/doge.png"))

    override doge.KeyPressed(key, _, _) =
        match key with
        | KeyConstant.Right -> doge.X <- doge.X + tileSize
        | KeyConstant.Left -> doge.X <- doge.X - tileSize
        | KeyConstant.Up -> doge.Y <- doge.Y - tileSize
        | KeyConstant.Down -> doge.Y <- doge.Y + tileSize
        | _ -> ()

type Playing() =
    inherit Scene()

    let doge = Doge()
    let objects : Object list = [ doge; Car(0f, 0f, 40f) ]

    override __.Draw() =
        for object in objects do
            object.Draw()

    override __.KeyPressed(key, scancode, isRepeat) =
        for object in objects do
            object.KeyPressed(key, scancode, isRepeat)

    override __.Update(dt) =
        for object in objects do
            object.Update(dt)

type Program() =
    inherit Scene()

    let mutable state = Playing() :> Scene

    override __.Draw() = state.Draw()

    override __.Update(dt) = state.Update(dt)

    override __.KeyPressed(key, scancode, isRepeat) =
        state.KeyPressed(key, scancode, isRepeat)

[<EntryPoint>]
let main _ =
    Boot.Init(BootConfig(WindowTitle = "Doger", WindowWidth = windowWidth, WindowHeight = windowHeight))
    Boot.Run(Program())

    0
