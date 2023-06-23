// Part 2
// N = 12
// Лисин Роман, М8О-306Б-20

// Load the data
#load "one.fsx"


let find_group student = 
  Data.studs
  |> Seq.filter(fun (name, group) -> name = student)
  |> Seq.head
  |> snd

let find_subj abbr = 
  Data.subjs 
  |> Seq.filter(fun (subj_abbr, subj) -> subj_abbr = abbr)
  |> Seq.head
  |> snd

// Напечатайте список групп и среднюю оценку в каждой из групп
let group_and_average_mark =
  Data.marks
  |> Seq.map(fun (name, _, mark) -> ((find_group name), mark))
  |> Seq.groupBy fst
  |> Seq.map(fun (group, pairs) -> (group, pairs |> Seq.map (snd >> float) |> Seq.average))
  |> Seq.iter(fun (g, m) -> printfn "Group: %d, mark: %f" g m)


// Для каждого предмета, напечатайте список студентов, проваливших экзамен (оценка=2)
let subj_and_failed_students =
  Data.marks
  |> Seq.map(fun (name, subj_abbr, mark) -> ((find_subj subj_abbr), name, mark))
  |> Seq.groupBy (fun (subj, name, mark) -> subj)
  |> Seq.map(fun (subj, subj_name_mark) -> (subj, subj_name_mark |> Seq.filter (fun (subj, name, mark) -> (mark = 2)) ))
  |> Seq.iter(fun (s, l) -> printfn "Subject: %s, failed students: %A" s (Seq.toList (l |> Seq.map(fun (subj, name, mark) -> name))))


// Найдите число несдавших студентов в каждой из групп
let group_and_failed_students_number =
  Data.marks
  |> Seq.map(fun (name, _, mark) -> ((find_group name), name, mark))
  |> Seq.groupBy (fun (group, name, mark) -> group)
  |> Seq.map(fun (group, group_name_mark) -> (group, group_name_mark |> Seq.filter (fun (group, name, mark) -> (mark = 2)) |> Seq.distinctBy (fun (group, name, mark) -> name) |> Seq.length))
  |> Seq.iter(fun (g, n) -> printfn "Group: %d, failed students' number: %d" g n)
