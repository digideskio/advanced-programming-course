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
        >>= \now            -> enterInformation "Make new appointment" []
        >>*                  [ OnAction (Action "Make") (hasValue (\app -> upd (\apps -> [app : apps]) appointments >>= const))
                             , OnAction (Action "Cancel") (always (return ()))
                             ]
              

tasks :: (Shared [Appointment]) -> [Workflow]
tasks appointments = [
    restrictedTransientWorkflow (adminTask +++ "Manage users") "Manage system users..." ["admin"] (forever manageUsers)
  , transientWorkflow (appointmentTask +++ "View appointments") "View your appointments" (viewAppointments appointments)
  , transientWorkflow (appointmentTask +++ "Make appointments") "Make new appointment" (makeAppointment appointments)
  ]


Start w = startEngineWithOptions 
    (\cli options.defaultEngineCLIOptions cli {options & sessionTime = 1000000000})
    [publish "/" (\_ -> withShared [] (\apps -> loginAndManageWorkList "Appointments" (tasks apps)))]
    w
