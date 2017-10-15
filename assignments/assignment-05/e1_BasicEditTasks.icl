module e1_BasicEditTasks

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 5, 2017
 * 
 * - use this a project with environment iTasks
 * - executable must be in Examples/iTasks or a subdirectory
 *   You can also use the -sdk commandline flag to set the path
 * - check Project Options -> Profiling -> Dynamics to prevent recompilation
*/

import iTasks
import iTasks.API.Extensions.Admin.WorkflowAdmin

:: Student =
	{ name :: String
	, snum :: Int
	, bama :: BaMa
	, year :: Int
	}

:: BaMa = Bachelor | Master

derive class iTask Student, BaMa

enterStudentTask :: Task Student
enterStudentTask = enterInformation "Enter a student" []

enterListOfStudentsTask :: Task [Student]
enterListOfStudentsTask = enterInformation "Enter a list of students" []

// Ground
undef = undef

updateStudentTask :: Task Student
updateStudentTask = updateInformation "Update the student" [] (hd students)

selectFavoriteStudentTask :: Task Student
selectFavoriteStudentTask = enterChoice "Select your favorite student" [ChooseFromGrid id] students

selectFavoriteStudentOnlyDisplayingNameTask :: Task Student
selectFavoriteStudentOnlyDisplayingNameTask = enterChoice "Select your favorite student" [ChooseFromGrid (\s -> s.Student.name)] students

selectFavoriteStudentDisplayingGToStringTask :: Task Student
selectFavoriteStudentDisplayingGToStringTask = enterChoice "Select your favorite student" [ChooseFromGrid gToString{|*|}] students

selectStudentsOnlyNameBaMaTask :: Task [Student]
selectStudentsOnlyNameBaMaTask = enterMultipleChoice "Select your potential partners" [ChooseFromGrid (\s -> (s.Student.name, s.Student.bama))] students

updateStudentNameOnlyTask :: Task Student
updateStudentNameOnlyTask = updateInformation "Update the student name" [UpdateAs id (\s s` -> {Student | s & name = s`.Student.name})] (hd students)

tasks = [
    workflow "Students/Enter a student" "Enter student" enterStudentTask,
    workflow "Students/Enter a list of students" "Enter students" enterListOfStudentsTask,
    workflow "Students/Update the student" "Update student" updateStudentTask,
    workflow "Students/Select your favorite student" "Select favorite student" selectFavoriteStudentTask,
    workflow "Students/Select your favorite student using only their name" "Select favorite student only name" selectFavoriteStudentOnlyDisplayingNameTask,
    workflow "Students/Select your favorite student using gToString" "Select favorite student gToString" selectFavoriteStudentDisplayingGToStringTask,
    workflow "Students/Select multiple students" "Select multiple students" selectStudentsOnlyNameBaMaTask,
    workflow "Students/Update the student name" "Update student name" updateStudentNameOnlyTask
    ]

Start w = startEngine [publish "/" (\_ -> manageWorklist tasks)] w

// For full compliance with the excercise ;-)
task1 = enterStudentTask
task2 = enterListOfStudentsTask
task3 = updateStudentTask
task4 = selectFavoriteStudentTask
task5 = selectFavoriteStudentOnlyDisplayingNameTask
task6 = selectFavoriteStudentDisplayingGToStringTask
task7 = selectStudentsOnlyNameBaMaTask
task8 = updateStudentNameOnlyTask

students :: [Student]
students =
	[{name = "Alice"
	 ,snum = 1000
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Bob"
	 ,snum = 1003
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Carol"
	 ,snum = 1024
	 ,bama = Master
	 ,year = 2
	 }
	,{name = "Dave"
	 ,snum = 2048
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Eve"
	 ,snum = 4096
	 ,bama = Master
	 ,year = 1
	 }
	,{name = "Frank"
	 ,snum = 1023
	 ,bama = Master
	 ,year = 1
	 }
	]

generic gToString a :: a -> String

gToString{|Int|} i = toString i
gToString{|String|} s = s
gToString{|UNIT|} UNIT = ""
gToString{|PAIR|} tsx tsy (PAIR x y) = tsx x +++ " " +++ tsy y
gToString{|EITHER|} tsl tsr (LEFT l) = tsl l
gToString{|EITHER|} tsl tsr (RIGHT r) = tsr r
gToString{|OBJECT|} ts (OBJECT o) = ts o
gToString{|CONS of c|} ts (CONS o)
  | c.gcd_arity == 0 = c.gcd_name
  | otherwise = "(" +++ c.gcd_name +++ " " +++ ts o +++ ")"
gToString{|FIELD of f|} ts (FIELD o) = ", " +++ f.gfd_name +++ " = " +++ ts o
gToString{|RECORD|} ts (RECORD o) = "{" +++ drop` 2 (ts o) +++ "}"
 
derive gToString Student, BaMa

drop` :: Int String -> String
drop` i s = toString (drop i strlist)
  where strlist :: [Char]
        strlist = fromString s

instance + String where + s t = s +++ t
