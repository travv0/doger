module Main

open System.IO
open Love

[<Literal>]
let tileSize = 32f

[<Literal>]
let windowWidth = 640

[<Literal>]
let windowHeight = 480

Boot.Init(BootConfig(WindowTitle = "Doger", WindowWidth = windowWidth, WindowHeight = windowHeight))

let spritesPath = "media/sprites/"

let dogeSprite =
    Graphics.NewImage(Path.Join(spritesPath, "doge.png"))

type GameObject(x: float32, y: float32, sprite: Image option) =
    inherit Scene()

    member val X = x with get, set
    member val Y = y with get, set

    member val Width =
        match sprite with
        | Some sprite -> sprite.GetWidth() |> float32
        | None -> tileSize

    member val Height =
        match sprite with
        | Some sprite -> sprite.GetHeight() |> float32
        | None -> tileSize

    member val Sprite = sprite with get, set

    default object.Draw() =
        match object.Sprite with
        | Some sprite -> Graphics.Draw(sprite, object.X, object.Y, 0f, 1f, 1f, 0f, 0f)
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

    abstract Collide : GameObject -> unit
    default __.Collide(_) = ()

type Water(x, y) =
    inherit GameObject(x, y, Some dogeSprite)

type HorizontalMover(x, y, moveSpeed, sprite) =
    inherit GameObject(x, y, Some sprite)

    member val MoveSpeed = moveSpeed

    override object.Update(dt) =
        object.X <- object.X + moveSpeed * dt

        if object.X < -object.Width then
            object.X <- float32 windowWidth + object.Width

        if object.X > float32 windowWidth + object.Width then
            object.X <- -object.Width

type Car(x, y, moveSpeed) =
    inherit HorizontalMover(x, y, moveSpeed, dogeSprite)

type WaterFloater(x, y, moveSpeed, sprite) =
    inherit HorizontalMover(x, y, moveSpeed, sprite)

type Log(x, y, moveSpeed, sprite) =
    inherit WaterFloater(x, y, moveSpeed, sprite)

type Turtle(x, y, moveSpeed, sprite) =
    inherit WaterFloater(x, y, moveSpeed, sprite)

    let toggleTime = 3f
    let mutable elapsedTime = 0f

    member val IsUnderwater = false with get, set

    override turtle.Update(dt) =
        if elapsedTime >= toggleTime then
            turtle.IsUnderwater <- not turtle.IsUnderwater

            turtle.Sprite <-
                if turtle.IsUnderwater then
                    None
                else
                    Some sprite

            elapsedTime <- 0f
        else
            elapsedTime <- elapsedTime + dt

        base.Update(dt)

type Doge() =
    inherit GameObject(float32 windowWidth / 2f, float32 windowHeight - tileSize, Some dogeSprite)

    let mutable onLog : WaterFloater option = None
    let mutable isOnWater = false

    override doge.KeyPressed(key, _, _) =
        match key with
        | KeyConstant.Right -> doge.X <- doge.X + tileSize
        | KeyConstant.Left -> doge.X <- doge.X - tileSize
        | KeyConstant.Up -> doge.Y <- doge.Y - tileSize
        | KeyConstant.Down -> doge.Y <- doge.Y + tileSize
        | _ -> ()

    override doge.Update(dt) =
        if isOnWater && onLog.IsNone then
            printfn "doge ded"

        match onLog with
        | Some log -> doge.X <- doge.X + log.MoveSpeed * dt
        | None -> ()

        onLog <- None
        isOnWater <- false

    override __.Collide(otherObject) =
        match otherObject with
        | :? Car -> printfn "doge smoosh"
        | :? Log as log -> onLog <- Some(log :> WaterFloater)
        | :? Turtle as turtle when not turtle.IsUnderwater -> onLog <- Some(turtle :> WaterFloater)
        | :? Water -> isOnWater <- true
        | _ -> ()

type Playing() =
    inherit Scene()

    let makeRiver y =
        [ for i in 0 .. windowWidth / int tileSize -> Water(tileSize * float32 i, y) ]

    let doge = Doge()

    let objects : GameObject list =
        List.concat [ makeRiver (tileSize * 5f)
                      |> List.map (fun w -> w :> GameObject)
                      makeRiver (tileSize * 6f)
                      |> List.map (fun w -> w :> GameObject)
                      [ Log(0f, tileSize * 5f, -100f, dogeSprite)
                        Turtle(0f, tileSize * 6f, 100f, dogeSprite)
                        Car(0f, 0f, 100f)
                        Car(0f, tileSize, -200f)
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

type Program() =
    inherit Scene()

    let mutable state = Playing() :> Scene

    override __.Draw() = state.Draw()

    override __.Update(dt) = state.Update(dt)

    override __.KeyPressed(key, scancode, isRepeat) =
        state.KeyPressed(key, scancode, isRepeat)

[<EntryPoint>]
let main _ =
    Boot.Run(Program())

    0
