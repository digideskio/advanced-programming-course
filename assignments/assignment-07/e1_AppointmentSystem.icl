module e1_AppointmentSystem

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import iTasks
import iTasks.Extensions.DateTime
import System.Time

// ------------------------------------------------------------------ //
// | Data structures & utility functions                            | //
// ------------------------------------------------------------------ //

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

addTime :: DateTime Time -> DateTime
addTime base added = timestampToGmDateTime (Timestamp (unTimestamp (utcDateTimeToTimestamp base) + 3600*added.Time.hour + 60*added.Time.min + added.Time.sec))
    where unTimestamp (Timestamp x) = x

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

// Convenience functions to get appointments and proposals by ID
class getByID` a :: Int [a] -> Maybe a

instance getByID` Appointment where
    getByID` _ [] = Nothing
    getByID` id [x : xs] = if (x.Appointment.id == id) (Just x) (getByID` id xs)

instance getByID` Proposal where
    getByID` _ [] = Nothing
    getByID` id [x : xs] = if (x.Proposal.id == id) (Just x) (getByID` id xs)
    
class getByID a :: Int -> Task (Maybe a)

instance getByID Appointment where
    getByID id = get schedule
                 >>= \appointments -> return (getByID` id appointments)

instance getByID Proposal where
    getByID id = get proposals
                 >>= \proposals -> return (getByID` id proposals)

// Convenience function to get and increment the ID share
getNextID :: Task Int
getNextID = get id
            >>= \id` -> set (id` + 1) id
            >>= (\_ -> return id`)

// ------------------------------------------------------------------ //
// | Appointment tasks                                              | //
// ------------------------------------------------------------------ //

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
                         -&&-  updateInformation "Starting time" [] (addTime now {Time|hour=1, min=(0-now.DateTime.min), sec=(0-now.DateTime.sec)})
                        )-&&-( updateInformation "Duration" [] {Time|hour=1, min=0, sec=0}
                         -&&-  enterMultipleChoice "Choose participants" [ChooseFromCheckGroup (\s -> s)] users
                       )) 
                       @ (\((title,when),(duration,participants)) -> {Appointment|id=id, title=title, when=when, duration=duration, owner=me, participants=participants})
        >>*                  [ OnAction (Action "Make") (hasValue (\appointment -> addAppointmentToShare appointment
                               >>= \_ -> viewInformation "Success" [] "The appointment has been added."
                               >>= \_ -> makeAppointment))
                             , OnAction (Action "Cancel") (always (return ()))
                             ]

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

// ------------------------------------------------------------------ //
// | Proposal tasks                                                 | //
// ------------------------------------------------------------------ //

makeProposal :: Task ()
makeProposal = get currentUser
        >>= \me             -> get currentDateTime
        >>= \now            -> getNextID
        >>= \id             -> get users
        >>= \users ->  (       enterInformation "Appointment title" []
                         -&&- updateInformation "Duration" [] {Time|hour=1, min=0, sec=0}
                       )
        @ (\(title, duration) -> title +++ "placeholder to ensure title's type can be inferred")
        >>= const

addProposalToShare :: Proposal -> Task ()
addProposalToShare proposal = upd (\proposals -> [proposal : proposals]) proposals
                                    >>= const

sendInvites :: Proposal -> Task ()
sendInvites proposal = sendInvites` proposal proposal.Proposal.participants
    where sendInvites` :: Proposal [User] -> Task ()
          sendInvites` _ [] = return ()
          sendInvites` proposal [participant:participants] =
                  get currentDateTime 
              >>= \now -> assign
                        (workerAttributes participant
                                     [ ("title",      "Please respond: " +++ proposal.Proposal.title)
                                     , ("createdBy",  toString (toUserConstraint proposal.Proposal.owner))
                                     , ("createdAt",  toString now)
                                     , ("priority",   toString 5)
                                     , ("createdFor", toString (toUserConstraint participant))
                         ]) 
                         (invitation proposal)

invitation :: Proposal -> Task ()
invitation _ = undef

// ------------------------------------------------------------------ //
// | Worfkow boilerplate                                            | //
// ------------------------------------------------------------------ //

tasks :: [Workflow]
tasks = [
    restrictedTransientWorkflow (adminTask +++ "Manage users") "Manage system users..." ["admin"] (forever manageUsers)
  , transientWorkflow (appointmentTask +++ "View appointments") "View your appointments" viewAppointments
  , transientWorkflow (appointmentTask +++ "Make appointments") "Make new appointment" makeAppointment
  , transientWorkflow (appointmentTask +++ "Make proposal") "Make new proposal" makeProposal
  ]


Start w = startEngineWithOptions 
    (\cli options.defaultEngineCLIOptions cli {options & sessionTime = 1000000000})
    [publish "/" (\_ -> loginAndManageWorkList "Appointments" tasks)]
    w
