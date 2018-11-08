module RobotSimulator

open System

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let turnRight d =
    match d with
    | Direction.North -> Direction.East
    | Direction.East -> Direction.South
    | Direction.South -> Direction.West
    | Direction.West -> Direction.North

let turnLeft d =
    match d with
    | Direction.North -> Direction.West
    | Direction.West -> Direction.South
    | Direction.South -> Direction.East
    | Direction.East -> Direction.North

let advance d p =
    match d with
    | Direction.North -> (fst p, snd p + 1)
    | Direction.West -> (fst p - 1, snd p)
    | Direction.South -> (fst p, snd p - 1)
    | Direction.East -> (fst p + 1, snd p)

let create direction position = { direction = direction; position = position }

let rec move (instructions:string) (robot:Robot): Robot = 
    let movements = instructions |> Seq.toList
    match movements with
    | [] -> robot
    | head::tail ->
        let nextMovements = new String(tail |> List.toArray)
        let newRobot = match head with
                        | 'R' -> { direction = robot.direction |> turnRight; position = robot.position }
                        | 'L' ->  { direction = robot.direction |> turnLeft; position = robot.position }
                        | 'A' ->  { direction = robot.direction; position = advance robot.direction robot.position }
                        | _ ->  { direction = robot.direction; position = robot.position }

        move nextMovements newRobot