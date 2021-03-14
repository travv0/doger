module Main

open System.IO
open Love

[<Literal>]
let tileSize = 32f

[<Literal>]
let windowWidth = 480

[<Literal>]
let windowHeight = 480

Boot.Init(BootConfig(WindowTitle = "Doger", WindowWidth = windowWidth, WindowHeight = windowHeight))

let spritesPath = "media/sprites/"

let dogeSprite =
    Graphics.NewImage(Path.Join(spritesPath, "doge.png"))

let mutable dogeLives = 3

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

type Car(x, y, moveSpeed, sprite) =
    inherit HorizontalMover(x, y, moveSpeed, sprite)

type WaterFloater(x, y, moveSpeed, sprite) =
    inherit HorizontalMover(x, y, moveSpeed, sprite)

type Log(x, y, moveSpeed, sprite) =
    inherit WaterFloater(x, y, moveSpeed, sprite)

type Turtle(x, y, moveSpeed, sinkTimer, sprite) =
    inherit WaterFloater(x, y, -moveSpeed, sprite)

    let mutable elapsedTime = 0f

    member val IsUnderwater = false with get, set

    override turtle.Update(dt) =
        match sinkTimer with
        | Some sinkTimer ->
            let toggleTime : float32 =
                if turtle.IsUnderwater then
                    sinkTimer / 4f
                else
                    sinkTimer

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
        | None -> ()

        base.Update(dt)

let startPos =
    {| X = float32 windowWidth / 2f
       Y = float32 windowHeight - tileSize |}

type Doge() as doge =
    inherit GameObject(startPos.X, startPos.Y, Some dogeSprite)

    let moveSpeed = 300f

    let mutable onLog : WaterFloater option = None
    let mutable isOnWater = false
    let mutable goalPos = None

    let die () =
        dogeLives <- dogeLives - 1
        doge.X <- startPos.X
        doge.Y <- startPos.Y

    override doge.KeyPressed(key, _, _) =
        if goalPos.IsNone then
            match key with
            | KeyConstant.Right -> goalPos <- Some {| X = doge.X + tileSize; Y = doge.Y |}
            | KeyConstant.Left -> goalPos <- Some {| X = doge.X - tileSize; Y = doge.Y |}
            | KeyConstant.Up -> goalPos <- Some {| X = doge.X; Y = doge.Y - tileSize |}
            | KeyConstant.Down -> goalPos <- Some {| X = doge.X; Y = doge.Y + tileSize |}
            | _ -> ()

    override doge.Update(dt) =
        let moveSpeed = moveSpeed * dt

        match goalPos with
        | None ->
            if isOnWater && onLog.IsNone then die ()

            match onLog with
            | Some log -> doge.X <- doge.X + log.MoveSpeed * dt
            | None -> ()

            onLog <- None
            isOnWater <- false
        | Some goal ->
            if goal.X > doge.X then
                doge.X <- doge.X + moveSpeed
            else if goal.X < doge.X then
                doge.X <- doge.X - moveSpeed
            else if goal.Y > doge.Y then
                doge.Y <- doge.Y + moveSpeed
            else if goal.Y < doge.Y then
                doge.Y <- doge.Y - moveSpeed

            if goal.X - doge.X |> abs < moveSpeed then
                doge.X <- goal.X

            if goal.Y - doge.Y |> abs < moveSpeed then
                doge.Y <- goal.Y

            if doge.X = goal.X && doge.Y = goal.Y then
                goalPos <- None

    override __.Collide(otherObject) =
        if goalPos.IsNone then
            match otherObject with
            | :? Car -> die ()
            | :? Log as log -> onLog <- Some(log :> WaterFloater)
            | :? Turtle as turtle when not turtle.IsUnderwater -> onLog <- Some(turtle :> WaterFloater)
            | :? Water -> isOnWater <- true
            | _ -> ()

let font = Graphics.NewFont(16)

type Playing() =
    inherit Scene()

    let makeRiver y : GameObject list =
        [ for i in 0 .. windowWidth / int tileSize -> Water(tileSize * float32 i, y * tileSize) ]

    let makeLog x y speed width : GameObject list =
        [ for i in 0 .. width - 1 -> Log(x * tileSize + float32 i * tileSize, y * tileSize, speed, dogeSprite) ]

    let makeTurtles x y speed width sinkTimer : GameObject list =
        [ for i in 0 .. width - 1 ->
              Turtle(x * tileSize + float32 i * tileSize, y * tileSize, speed, sinkTimer, dogeSprite) ]

    let makeCar x y speed sprite : GameObject list =
        [ Car(x * tileSize, y * tileSize, speed, sprite) ]

    let doge = Doge()

    let lives =
        Graphics.NewText(font, sprintf "Lives: %d" dogeLives)

    let objects : GameObject list =
        List.concat [ makeRiver 3f
                      makeRiver 4f
                      makeRiver 5f
                      makeRiver 6f
                      makeRiver 7f
                      makeLog 0f 3f 100f 3
                      makeLog 4.5f 3f 100f 3
                      makeLog 9f 3f 100f 3
                      makeTurtles 0f 4f 100f 2 None
                      makeTurtles 3.5f 4f 100f 2 None
                      makeTurtles 7f 4f 100f 2 (Some 2f)
                      makeLog 0f 5f 150f 4
                      makeLog 6f 5f 150f 4
                      makeLog 12f 5f 150f 4
                      makeLog 0f 6f 50f 2
                      makeLog 4f 6f 50f 2
                      makeLog 8f 6f 50f 2
                      makeLog 12f 6f 50f 2
                      makeTurtles 0f 7f 100f 3 None
                      makeTurtles 4f 7f 100f 3 None
                      makeTurtles 8f 7f 100f 3 (Some 3f)
                      makeTurtles 12f 7f 100f 3 None
                      makeCar 0f 9f -50f dogeSprite
                      makeCar 5f 9f -50f dogeSprite
                      makeCar 0f 10f 300f dogeSprite
                      makeCar 0f 11f -100f dogeSprite
                      makeCar 4f 11f -100f dogeSprite
                      makeCar 8f 11f -100f dogeSprite
                      makeCar 0f 12f 75f dogeSprite
                      makeCar 5f 12f 75f dogeSprite
                      makeCar 10f 12f 75f dogeSprite
                      makeCar 6f 13f -100f dogeSprite
                      makeCar 11f 13f -100f dogeSprite
                      makeCar 16f 13f -100f dogeSprite
                      [ doge ] ]

    override __.Draw() =
        for object in objects do
            object.Draw()

        Graphics.Draw(lives)

    override __.KeyPressed(key, scancode, isRepeat) =
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

        lives.Set(ColoredStringArray.Create(sprintf "Lives: %d" dogeLives))

type GameOver() =
    inherit Scene()

    let text =
        Graphics.NewText(font, "Game Over\nPress R to restart")

    override __.Draw() = Graphics.Draw(text)

type Program() =
    inherit Scene()

    let mutable state = Playing() :> Scene

    let reset () =
        dogeLives <- 3
        state <- Playing()

    override __.Draw() = state.Draw()

    override __.Update(dt) =
        state.Update(dt)

        if dogeLives < 0 then
            state <- GameOver()

    override __.KeyPressed(key, scancode, isRepeat) =
        match key with
        | KeyConstant.Escape -> Event.Quit(0)
        | KeyConstant.R -> reset ()
        | _ -> state.KeyPressed(key, scancode, isRepeat)

[<EntryPoint>]
let main _ =
    Boot.Run(Program())

    0
