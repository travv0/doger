module Main

open Love

let tileSize = 32f

let (windowWidth, windowHeight) = (640, 480)

[<AbstractClass>]
type GameObject(x: float32, y: float32, sprite: Image) =
    inherit Scene()
    member val X = x with get, set
    member val Y = y with get, set
    member __.Width = sprite.GetWidth() |> float32
    member __.Height = sprite.GetHeight() |> float32
    member __.Sprite = sprite

    default this.Draw() =
        Graphics.Draw(this.Sprite, this.X, this.Y, 0f, 1f, 1f, 0f, 0f)

    member object.Intersects(otherObject: GameObject) =
        [ {| X = object.X; Y = object.Y |}
          {| X = object.X + object.Width - 1f
             Y = object.Y |}
          {| X = object.X + object.Width - 1f
             Y = object.Y + object.Height - 1f |}
          {| X = object.X
             Y = object.Y + object.Height - 1f |} ]
        |> Seq.exists
            (fun corner ->
                corner.X >= otherObject.X
                && corner.X < otherObject.X + otherObject.Width
                && corner.Y >= otherObject.Y
                && corner.Y < otherObject.Y + otherObject.Height)

    abstract member Collide : GameObject -> unit
    default __.Collide(_) = ()

type HorizontalMover(x, y, moveSpeed, sprite) =
    inherit GameObject(x, y, sprite)

    override object.Update(dt) =
        object.X <- object.X + moveSpeed * dt

        if object.X < -object.Width then
            object.X <- float32 windowWidth + object.Width

        if object.X > float32 windowWidth + object.Width then
            object.X <- -object.Width

type Car(x, y, moveSpeed) =
    inherit HorizontalMover(x, y, moveSpeed, Graphics.NewImage("media/sprites/doge.png"))

type Log(x, y, moveSpeed) =
    inherit HorizontalMover(x, y, moveSpeed, Graphics.NewImage("media/sprites/doge.png"))

type Doge() =
    inherit GameObject(float32 windowWidth / 2f,
                       float32 windowHeight - tileSize,
                       Graphics.NewImage("media/sprites/doge.png"))

    override doge.KeyPressed(key, _, _) =
        match key with
        | KeyConstant.Right -> doge.X <- doge.X + tileSize
        | KeyConstant.Left -> doge.X <- doge.X - tileSize
        | KeyConstant.Up -> doge.Y <- doge.Y - tileSize
        | KeyConstant.Down -> doge.Y <- doge.Y + tileSize
        | _ -> ()

    override doge.Collide(otherObject) = printfn "doge collide"

type Playing() =
    inherit Scene()

    let doge = Doge()

    let objects : GameObject list =
        [ doge
          Car(0f, 0f, 100f)
          Car(0f, 32f, -200f)
          Log(0f, 32f * 5f, -100f)
          Log(0f, 32f * 6f, 200f) ]

    override __.Draw() =
        for object in objects do
            object.Draw()

    override __.KeyPressed(key, scancode, isRepeat) =
        if key = KeyConstant.Escape then
            Event.Quit(0)

        for object in objects do
            object.KeyPressed(key, scancode, isRepeat)

    override __.Update(dt) =
        for object in objects do
            object.Update(dt)

        for object in objects do
            for otherObject in objects do
                if
                    object <> otherObject
                    && object.Intersects(otherObject)
                then
                    object.Collide(otherObject)

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
