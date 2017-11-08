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

adminTask       :== "Admin/"
appointmentTask :== "Appointments/"

// Shared data
schedule :: Shared [Appointment]
schedule = sharedStore "schedule" []

viewAppointments :: Task ()
viewAppointments = get currentUser 
        >>= \me              -> get currentDateTime 
        >>= \now             -> viewSharedInformation "Appointment viewer" [ViewAs (filter (\app -> (isMember me app.participants && app.when > now)))] schedule
        >>=                     const

makeAppointment :: Task ()
makeAppointment = get currentUser 
        >>= \me             -> get currentDateTime 
        >>= \now            -> get users
        >>= \users ->  ((      enterInformation "Appointment title" [] 
                         -&&-  updateInformation "Starting time" [] (nextHour now)
                        )-&&-( updateInformation "Duration" [] {hour=1, min=0, sec=0}
                         -&&-  enterMultipleChoice "Choose participants" [ChooseFromCheckGroup (\s -> s)] users
                       )) 
                       @ (\((title,when),(duration,participants)) -> {title=title, when=when, duration=duration, owner=me, participants=participants})
        >>*                  [ OnAction (Action "Make") (hasValue (\appointment -> addAppointmentToShare appointment
                               >>= \_ -> viewInformation "Success" [] "The appointment has been added."
                               >>= \_ -> makeAppointment))
                             , OnAction (Action "Cancel") (always (return ()))
                             ]
        where nextHour :: DateTime -> DateTime
              nextHour now = {DateTime | now & DateTime.hour=now.DateTime.hour+1, min=0, sec=0}

addAppointmentToShare :: Appointment -> Task ()
addAppointmentToShare appointment = upd (\appointments -> [appointment : appointments]) schedule
                                    >>= const

tasks :: [Workflow]
tasks = [
    restrictedTransientWorkflow (adminTask +++ "Manage users") "Manage system users..." ["admin"] (forever manageUsers)
  , transientWorkflow (appointmentTask +++ "View appointments") "View your appointments" viewAppointments
  , transientWorkflow (appointmentTask +++ "Make appointments") "Make new appointment" makeAppointment
  ]


Start w = startEngineWithOptions 
    (\cli options.defaultEngineCLIOptions cli {options & sessionTime = 1000000000})
    [publish "/" (\_ -> loginAndManageWorkList "Appointments" tasks)]
    w
