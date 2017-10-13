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
updateStudentTask = updateInformation "Update the student" [] undef

selectFavoriteStudentTask :: Task Student
selectFavoriteStudentTask = enterChoice "Select your favorite student" [ChooseFromGrid id] students

tasks = [
    workflow "Students/Enter a student" "Enter student" enterStudentTask,
    workflow "Students/Enter a list of students" "Enter students" enterListOfStudentsTask,
    workflow "Students/Update the student" "Update student" updateStudentTask,
    workflow "Students/Select your favorite student" "Select favorite student" selectFavoriteStudentTask
    ]

Start w = startEngine [publish "/" (\_ -> manageWorklist tasks)] w

// For full compliance with the excercise ;-)
task1 = enterStudentTask
task2 = enterListOfStudentsTask

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

instance + String where + s t = s +++ t
