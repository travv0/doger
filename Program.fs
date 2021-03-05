module Main

open Love

let tileSize = 32f

let (windowWidth, windowHeight) = (640, 480)

[<AbstractClass>]
type GameObject(x: float32, y: float32, sprite: Image option) =
    inherit Scene()

    member val X = x with get, set
    member val Y = y with get, set

    member __.Width =
        match sprite with
        | Some sprite -> sprite.GetWidth() |> float32
        | None -> tileSize

    member __.Height =
        match sprite with
        | Some sprite -> sprite.GetHeight() |> float32
        | None -> tileSize

    member __.Sprite = sprite

    default this.Draw() =
        match this.Sprite with
        | Some sprite -> Graphics.Draw(sprite, this.X, this.Y, 0f, 1f, 1f, 0f, 0f)
        | None -> ()

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

    abstract member PostUpdate : unit -> unit
    default __.PostUpdate() = ()

type Water(x, y) =
    inherit GameObject(x, y, Some(Graphics.NewImage("media/sprites/doge.png")))

type HorizontalMover(x, y, moveSpeed, sprite) =
    inherit GameObject(x, y, Some sprite)

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
                       Some(Graphics.NewImage("media/sprites/doge.png")))

    let mutable isOnLog = false
    let mutable isOnWater = false

    override doge.KeyPressed(key, _, _) =
        match key with
        | KeyConstant.Right -> doge.X <- doge.X + tileSize
        | KeyConstant.Left -> doge.X <- doge.X - tileSize
        | KeyConstant.Up -> doge.Y <- doge.Y - tileSize
        | KeyConstant.Down -> doge.Y <- doge.Y + tileSize
        | _ -> ()

    override __.Update(dt) =
        isOnLog <- false
        isOnWater <- false

    override __.Collide(otherObject) =
        match otherObject with
        | :? Car -> printfn "doge smoosh"
        | :? Log -> isOnLog <- true
        | :? Water -> isOnWater <- true
        | _ -> ()

    override __.PostUpdate() =
        if isOnWater && not isOnLog then
            printfn "doge ded"
        else if isOnLog && isOnWater then
            printfn "doge safe on log"

type Playing() =
    inherit Scene()

    let makeRiver y : GameObject list =
        [ for i in 0 .. windowWidth / int tileSize -> Water(tileSize * float32 i, y) ]

    let doge = Doge()

    let objects : GameObject list =
        List.concat [ makeRiver (32f * 5f)
                      makeRiver (32f * 6f)
                      [ Log(0f, 32f * 5f, -100f)
                        Log(0f, 32f * 6f, 100f)
                        Car(0f, 0f, 100f)
                        Car(0f, 32f, -200f)
                        doge ] ]

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

        for object in objects do
            object.PostUpdate()

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
