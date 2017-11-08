module e1_AppointmentSystem

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import _SystemArray
import iTasks
import iTasks.Extensions.DateTime

:: Appointment =
    {
        title        :: String,
        when         :: DateTime,
        duration     :: Time,
        owner        :: User,
        participants :: [User]
    }


derive class iTask Appointment

// Ground
undef = undef
const = \_ -> return ()

// Task hierarchy

adminTask   :== "Admin/"
appointmentTask :== "Appointments/"

viewAppointments :: (Shared [Appointment]) -> Task ()
viewAppointments appointments = get currentUser 
        >>= \me              -> get currentDateTime 
        >>= \now             -> viewSharedInformation "Appointment viewer" [ViewAs (filter (\app -> (isMember me app.participants && app.when > now)))] appointments
        >>=                     const

makeAppointment :: (Shared [Appointment]) -> Task ()
makeAppointment appointments = get currentUser 
        >>= \me             -> get currentDateTime 
        >>= \now            -> get users
        >>= \users -> ((      enterInformation "Appointment title" [] 
                        -&&-  enterInformation "Starting time" [] 
                       )-&&-( enterInformation "Duration" [] 
                        -&&-  enterMultipleChoice "Choose participants" [] users
                      )) 
                      @ (\((title,when),(duration,participants)) -> {title=title, when=when, duration=duration, owner=me, participants=participants})
        >>*                  [ OnAction (Action "Make") (hasValue (\app -> upd (\apps -> [app : apps]) appointments >>= const))
                             , OnAction (Action "Cancel") (always (return ()))
                             ]
              

tasks :: (Shared [Appointment]) -> [Workflow]
tasks appointments = [
    restrictedWorkflow (adminTask +++ "Manage users") "Manage system users..." ["admin"] manageUsers
  , workflow (appointmentTask +++ "View appointments") "View your appointments" (viewAppointments appointments)
  , workflow (appointmentTask +++ "Make appointments") "Make new appointment" (makeAppointment appointments)
  ]


Start w = startEngine [publish "/" (\_ -> withShared [] (\apps -> loginAndManageWorkList "Appointments" (tasks apps)))] w
