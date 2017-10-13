module e1_BasicEditTasks

import iTasks

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

Start w = startEngine enterStudentTask w

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
