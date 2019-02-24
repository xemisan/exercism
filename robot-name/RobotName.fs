module RobotName

type Robot = { Name: string }

let randomName() = 
    let rnd = System.Random()
    System.String.Concat [
        (rnd.Next(65, 90) |> char).ToString(); 
        (rnd.Next(65, 90) |> char).ToString(); 
        rnd.Next(0, 9).ToString();
        rnd.Next(0, 9).ToString(); 
        rnd.Next(0, 9).ToString()]

let rec mkRobot() = 
    { Name = randomName() }
    
let name (robot: Robot): string = 
    robot.Name

let reset robot = 
    { robot with Name = randomName() }