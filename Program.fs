module Main

open System.IO
open Love

[<Literal>]
let tileSize = 32f

[<Literal>]
let windowWidth = 480

[<Literal>]
let windowHeight = 480

Boot.Init(
    BootConfig(
        WindowTitle = "Doger",
        WindowWidth = windowWidth,
        WindowHeight = windowHeight
    )
)

let spritesPath = "media/sprites/"

let dogeSprite =
    Graphics.NewImage(Path.Join(spritesPath, "doge.png"))

let logLeftEndSprite =
    Graphics.NewImage(Path.Join(spritesPath, "log_end1.png"))

let logRightEndSprite =
    Graphics.NewImage(Path.Join(spritesPath, "log_end2.png"))

let logMiddleSprite =
    Graphics.NewImage(Path.Join(spritesPath, "log_middle.png"))

type GameObject(x : float32, y : float32, sprite : Image option) =
    inherit Scene()

    member val X = x with get, set
    member val Y = y with get, set

    abstract Width : float32

    default _.Width =
        match sprite with
        | Some sprite -> sprite.GetWidth() |> float32
        | None -> tileSize

    abstract Height : float32

    default _.Height =
        match sprite with
        | Some sprite -> sprite.GetHeight() |> float32
        | None -> tileSize

    member val Sprite = sprite with get, set

    default object.Draw() =
        match object.Sprite with
        | Some sprite ->
            Graphics.Draw(sprite, object.X, object.Y, 0f, 1f, 1f, 0f, 0f)
        | None -> ()

    member object.Intersects(otherObject : GameObject) =
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
    default _.Collide _ = ()

type Wall(x, y) =
    inherit GameObject(x, y, Some dogeSprite)

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

type Goal(x, y) =
    inherit GameObject(x, y, None)

    member val IsAchieved = false with get, set

type Doge() as doge =
    inherit GameObject(startPos.X, startPos.Y, Some dogeSprite)

    let moveSpeed = 300f

    let mutable onLog : WaterFloater option = None
    let mutable isOnWater = false
    let mutable goalPos = None

    let resetPos () =
        doge.X <- startPos.X
        doge.Y <- startPos.Y

    let die () =
        doge.Lives <- doge.Lives - 1
        resetPos ()

    member val Lives = 6 with get, set

    override doge.KeyPressed(key, _, _) =
        if goalPos.IsNone then
            match key with
            | KeyConstant.Right ->
                goalPos <-
                    Some
                        {| X =
                               min
                                   (doge.X + tileSize)
                                   (float32 windowWidth - tileSize)
                           Y = doge.Y |}
            | KeyConstant.Left ->
                goalPos <-
                    Some
                        {| X = doge.X - tileSize |> max 0f
                           Y = doge.Y |}
            | KeyConstant.Up ->
                goalPos <-
                    Some
                        {| X = doge.X
                           Y = doge.Y - tileSize |> max 0f |}
            | KeyConstant.Down ->
                goalPos <-
                    Some
                        {| X = doge.X
                           Y =
                               min
                                   (doge.Y + tileSize)
                                   (float32 windowHeight - tileSize) |}
            | _ -> ()

    override doge.Update(dt) =
        let moveSpeed = moveSpeed * dt

        match goalPos with
        | None ->
            if isOnWater && onLog.IsNone then die ()

            match onLog with
            | Some log ->
                if log.MoveSpeed > 0f
                   && doge.X < float32 windowWidth - tileSize
                   || log.MoveSpeed < 0f && doge.X > 0f then
                    doge.X <- doge.X + log.MoveSpeed * dt
            | None -> ()

            onLog <- None
            isOnWater <- false
        | Some goal ->
            if goal.X > doge.X then
                doge.X <- doge.X + moveSpeed
            elif goal.X < doge.X then
                doge.X <- doge.X - moveSpeed
            elif goal.Y > doge.Y then
                doge.Y <- doge.Y + moveSpeed
            elif goal.Y < doge.Y then
                doge.Y <- doge.Y - moveSpeed

            if goal.X - doge.X |> abs < moveSpeed then
                doge.X <- goal.X

            if goal.Y - doge.Y |> abs < moveSpeed then
                doge.Y <- goal.Y

            if doge.X = goal.X && doge.Y = goal.Y then
                goalPos <- None

    override _.Collide(otherObject) =
        if goalPos.IsNone then
            match otherObject with
            | :? Car
            | :? Wall -> die ()
            | :? Log as log -> onLog <- Some(log :> WaterFloater)
            | :? Turtle as turtle when not turtle.IsUnderwater ->
                onLog <- Some(turtle :> WaterFloater)
            | :? Water -> isOnWater <- true
            | :? Goal as goal when goal.IsAchieved -> die ()
            | :? Goal as goal ->
                goal.IsAchieved <- true
                goal.Sprite <- Some dogeSprite
                resetPos ()
            | _ -> ()

let font = Graphics.NewFont(16)

type State =
    | Playing
    | GameOver
    | Victory

type Playing() =
    inherit Scene()

    let makeRiver y : GameObject list =
        [ for i in 0 .. windowWidth / int tileSize ->
              Water(tileSize * float32 i, y * tileSize) ]

    let makeLog x y speed width : GameObject list =
        [ for i in 0 .. width - 1 ->
              Log(
                  x * tileSize + float32 i * tileSize,
                  y * tileSize,
                  speed,
                  (if i = 0 then
                       logLeftEndSprite
                   elif i = width - 1 then
                       logRightEndSprite
                   else
                       logMiddleSprite)
              ) ]

    let makeTurtles x y speed width sinkTimer : GameObject list =
        [ for i in 0 .. width - 1 ->
              Turtle(
                  x * tileSize + float32 i * tileSize,
                  y * tileSize,
                  speed,
                  sinkTimer,
                  dogeSprite
              ) ]

    let makeCar x y speed sprite : GameObject list =
        [ Car(x * tileSize, y * tileSize, speed, sprite) ]

    let goals =
        [ for i in 1 .. windowWidth / int tileSize - 1 do
              if i % 2 <> 0 then
                  yield Goal(float32 i * tileSize, 2f * tileSize) ]


    let walls : GameObject list =
        List.concat [ [ for i in 0 .. windowWidth / int tileSize - 1 do
                            yield Wall(float32 i * tileSize, 1f * tileSize) ]
                      [ for i in 0 .. windowWidth / int tileSize - 1 do
                            if i % 2 = 0 then
                                yield Wall(float32 i * tileSize, 2f * tileSize) ] ]

    let doge = Doge()

    let lives =
        Graphics.NewText(font, $"Lives: %d{doge.Lives}")

    let objects : GameObject list =
        List.concat [ goals |> List.map (fun goal -> goal :> GameObject)
                      walls
                      makeRiver 3f
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

    member val State = State.Playing with get, set

    override _.Draw() =
        for object in objects do
            object.Draw()

        Graphics.Draw(lives)

    override _.KeyPressed(key, scancode, isRepeat) =
        for object in objects do
            object.KeyPressed(key, scancode, isRepeat)

    override this.Update(dt) =
        for object in objects do
            object.Update(dt)

        for object in objects do
            for otherObject in objects do
                if
                    object <> otherObject
                    && object.Intersects(otherObject)
                then
                    object.Collide(otherObject)

        if doge.Lives < 0 then
            this.State <- GameOver

        if goals |> Seq.forall (fun goal -> goal.IsAchieved) then
            this.State <- Victory

        lives.Set(ColoredStringArray.Create $"Lives: %d{doge.Lives}")

type GameOver() =
    inherit Scene()

    let text =
        Graphics.NewText(font, "Game Over\nPress R to restart")

    override _.Draw() = Graphics.Draw(text)

type Victory() =
    inherit Scene()

    let text =
        Graphics.NewText(font, "You win!\nPress R to restart")

    override _.Draw() = Graphics.Draw(text)

type Program() =
    inherit Scene()

    let mutable state = Playing() :> Scene

    let reset () = state <- Playing()

    override _.Draw() = state.Draw()

    override _.Update(dt) =
        state.Update(dt)

        match state with
        | :? Playing as playing ->
            match playing.State with
            | GameOver -> state <- GameOver()
            | Victory -> state <- Victory()
            | _ -> ()
        | _ -> ()

    override _.KeyPressed(key, scancode, isRepeat) =
        match key with
        | KeyConstant.Escape -> Event.Quit(0)
        | KeyConstant.R -> reset ()
        | _ -> state.KeyPressed(key, scancode, isRepeat)

[<EntryPoint>]
let main _ =
    Boot.Run(Program())

    0
