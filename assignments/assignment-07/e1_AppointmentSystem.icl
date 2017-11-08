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
        id           :: Int,
        title        :: String,
        when         :: DateTime,
        duration     :: Time,
        owner        :: User,
        participants :: [User]
    }

:: Proposal = 
    {
        id       :: Int,
        title    :: String,
        when     :: [DateTime],
        duration :: Time,
        owner    :: User,
        participants :: [User]
    }

derive class iTask Appointment
derive class iTask Proposal

// Ground
undef = undef
const = \_ -> return ()

// Task hierarchy

adminTask       :== "Admin/"
appointmentTask :== "Appointments/"

// Shared data
schedule :: Shared [Appointment]
schedule = sharedStore "schedule" []

proposals :: Shared [Proposal]
proposals = sharedStore "proposals" []

id :: Shared Int
id = sharedStore "idIncrement" 0

getNextID :: Task Int
getNextID = get id
            >>= \id` -> set (id` + 1) id
            >>= (\_ -> return id`)

viewAppointments :: Task ()
viewAppointments = get currentUser 
        >>= \me              -> get currentDateTime 
        >>= \now             -> viewSharedInformation "Appointment viewer" [ViewAs (filter (\app -> (isMember me app.Appointment.participants && app.Appointment.when > now)))] schedule
        >>=                     const

makeAppointment :: Task ()
makeAppointment = get currentUser 
        >>= \me             -> get currentDateTime 
        >>= \now            -> getNextID
        >>= \id             -> get users
        >>= \users ->  ((      enterInformation "Appointment title" [] 
                         -&&-  updateInformation "Starting time" [] (nextHour now)
                        )-&&-( updateInformation "Duration" [] {Time|hour=1, min=0, sec=0}
                         -&&-  enterMultipleChoice "Choose participants" [ChooseFromCheckGroup (\s -> s)] users
                       )) 
                       @ (\((title,when),(duration,participants)) -> {Appointment|id=id, title=title, when=when, duration=duration, owner=me, participants=participants})
        >>*                  [ OnAction (Action "Make") (hasValue (\appointment -> addAppointmentToShare appointment
                               >>= \_ -> viewInformation "Success" [] "The appointment has been added."
                               >>= \_ -> makeAppointment))
                             , OnAction (Action "Cancel") (always (return ()))
                             ]
        where nextHour :: DateTime -> DateTime
              nextHour now = {DateTime|now&hour=(now.DateTime.hour+1), min=0, sec=0}

addAppointmentToShare :: Appointment -> Task ()
addAppointmentToShare appointment = upd (\appointments -> [appointment : appointments]) schedule
                                    >>= \_ -> addAppointmentTasks appointment appointment.Appointment.participants

addAppointmentTasks :: Appointment [User] -> Task ()
addAppointmentTasks appointment [] = return ()
addAppointmentTasks appointment [participant:participants] = 
        assign
            (workerAttributes participant
                         [ ("title",      appointment.Appointment.title)
                         , ("createdBy",  toString (toUserConstraint appointment.Appointment.owner))
                         , ("createdAt",  toString appointment.Appointment.when)
                         //, ("completeBefore", toString (appointment.Appointment.when + appointment.Appointment.duration))
                         , ("completeBefore", toString (appointment.Appointment.when))
                         , ("priority",   toString 5)
                         , ("createdFor", toString (toUserConstraint participant))
                         ]) 
            (viewInformation "Placeholder" [] "Placeholder")
    >>= \_ -> addAppointmentTasks appointment participants

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
