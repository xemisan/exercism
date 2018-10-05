module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    match school.ContainsKey(grade) with
    | true -> school.Add(grade, student::school.[grade])
    | _ -> school.Add(grade, [student])

let roster (school: School): (int * string list) list = 
    school
    |> Map.map (fun _ value -> List.sort value)
    |> Map.toList

let grade (number: int) (school: School): string list =
    match school.ContainsKey(number) with
    | true -> List.sort school.[number]
    | _ -> []