module e1_AppointmentSystem

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import Data.List
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
        id           :: Int,
        title        :: String,
        when         :: [DateTime],
        duration     :: Time,
        owner        :: User,
        participants :: [User]
    }

:: ProposalResponse =
    {
        id           :: Int,
        responses    :: [(User,[DateTime])]
    }

derive class iTask Appointment
derive class iTask Proposal
derive class iTask ProposalResponse

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

proposalResponses :: Shared [ProposalResponse]
proposalResponses = sharedStore "proposalResponses" []

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

instance getByID` ProposalResponse where
    getByID` _ [] = Nothing
    getByID` id [x : xs] = if (x.ProposalResponse.id == id) (Just x) (getByID` id xs)
    
class getByID a :: Int -> Task (Maybe a)

instance getByID Appointment where
    getByID id = get schedule
                 >>= \appointments -> return (getByID` id appointments)

instance getByID Proposal where
    getByID id = get proposals
                 >>= \proposals -> return (getByID` id proposals)

instance getByID ProposalResponse where
    getByID id = get proposalResponses
                 >>= \proposalResponses -> return (getByID` id proposalResponses)

// Convenience function to get and increment the ID share
getNextID :: Task Int
getNextID = get id
            >>= \id` -> set (id` + 1) id
            >>= (\_ -> return id`)

// ------------------------------------------------------------------ //
// | Appointment tasks                                              | //
// ------------------------------------------------------------------ //

viewAppointments :: Task [Appointment]
viewAppointments = get currentUser 
        >>= \me              -> get currentDateTime 
        >>= \now             -> viewSharedInformation "Appointment viewer" [ViewAs (filter (\app -> (isMember me app.Appointment.participants && app.Appointment.when > now)))] schedule

makeAppointment :: Task ()
makeAppointment = get currentUser 
        >>= \me             -> get currentDateTime 
        >>= \now            -> getNextID
        >>= \id             -> get users
        >>= \users ->  ((      enterInformation "Appointment title" [] 
                         -&&-  updateInformation "Starting time" [] (addTime now {Time | hour=1, min=(0-now.DateTime.min), sec=(0-now.DateTime.sec)})
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
    ||-      addAppointmentTasks appointment participants

// ------------------------------------------------------------------ //
// | Proposal tasks                                                 | //
// ------------------------------------------------------------------ //

makeProposal :: Task ()
makeProposal = get currentUser
        >>= \me             -> get currentDateTime
        >>= \now            -> getNextID
        >>= \id             -> get users
        >>= \users ->  ( (     enterInformation "Appointment title" []
                          -&&- updateInformation "Duration" [] {Time|hour=1, min=0, sec=0}
                         )-&&- enterMultipleChoice "Choose participants" [ChooseFromCheckGroup (\s -> s)] users
                       )
        >>= \((ttl,dur),ps) -> enterInformation "Choose proposed dates" []
        >>* [OnAction (Action "Choose times") (ifValue (not o isEmpty) (\dates -> updateDateTimes dates (map (\_ -> []) dates)))] 
        >>= \times          -> addProposalToShare {Proposal | id=id, title=ttl, when=times, duration=dur, owner=me, participants=ps}
    where updateDateTimes :: [Date] [[Time]] -> Task [DateTime]
          updateDateTimes dates times = 
                               if (length dates > 1)
                                   (allTasks (zipWith (\date -> updateInformation ("Proposed times for " +++ toString date) []) dates times))
                                   (updateInformation ("Proposed times for " +++ toString (dates!!0)) [] [] >>= \times -> return [times]) // Hacky solution to solve "hd of []" when only one date is presented to allTasks
                        >>*  [ OnAction (Action "Propose") (ifValue (all (not o isEmpty)) (\times -> return (combineDateTimes dates times)))
                             , OnAction (Action "Copy times from first date") (ifValue (not o isEmpty o hd) (\times -> updateDateTimes dates (map (\_ -> hd times) dates)))
                             ]
          combineDateTimes dates times = flatten (zipWith (\date -> map (\time -> {DateTime | year=date.Date.year, mon=date.Date.mon, day=date.Date.day, hour=time.Time.hour, min=time.Time.min, sec=time.Time.sec})) dates times)

addProposalToShare :: Proposal -> Task ()
addProposalToShare proposal =  upd (\proposals -> [proposal : proposals]) proposals
        >>= \_              -> upd (\proposalResponses -> [{ProposalResponse | id=proposal.Proposal.id, responses=[]} : proposalResponses]) proposalResponses
        >>= \_              -> sendManageProposal proposal
        >>= \_              -> sendInvites proposal

sendInvites :: Proposal -> Task ()
sendInvites proposal = sendInvites` proposal proposal.Proposal.participants
    where sendInvites` :: Proposal [User] -> Task ()
          sendInvites` _ [] = return ()
          sendInvites` proposal [participant:participants] =
                  get currentDateTime 
              >>= \now -> assign
                        (workerAttributes participant
                                     [ ("title",      "Appointment proposed: " +++ proposal.Proposal.title)
                                     , ("createdBy",  toString (toUserConstraint proposal.Proposal.owner))
                                     , ("createdAt",  toString now)
                                     , ("priority",   toString 5)
                                     , ("createdFor", toString (toUserConstraint participant))
                         ]) 
                         (invitation proposal)
              ||- sendInvites` proposal participants

invitation :: Proposal -> Task ()
invitation proposal = getByID proposal.Proposal.id 
                  >>* [ OnValue (hasValue nextTask) ]
    where nextTask :: (Maybe Proposal) -> Task ()
          nextTask Nothing  =  viewInformation "This proposal no longer exists." [] ()
          nextTask (Just proposal) = get currentUser
            >>= \user       -> viewInformation "Appointment proposed! Please respond:" [] proposal
                           ||- enterMultipleChoice "Choose available times" [ChooseFromCheckGroup (\s -> s)] proposal.Proposal.when
            >>= \chosen     -> upd (addResponse proposal.Proposal.id user chosen) proposalResponses
            >>=                const
          addResponse id user chosen [] = [] // just to be sure
          addResponse id user chosen [response:responses] 
            | response.ProposalResponse.id == id = [{ProposalResponse | response & responses = [(user, chosen) : response.ProposalResponse.responses]} : responses]
            | otherwise = [response : addResponse id user chosen responses]
            
sendManageProposal :: Proposal -> Task ()
sendManageProposal proposal = get currentDateTime 
            >>= \now -> assign
                        (workerAttributes proposal.Proposal.owner
                                     [ ("title",      "Manage appointment proposal: " +++ proposal.Proposal.title)
                                     , ("createdBy",  toString (toUserConstraint proposal.Proposal.owner))
                                     , ("createdAt",  toString now)
                                     , ("priority",   toString 5)
                                     , ("createdFor", toString (toUserConstraint proposal.Proposal.owner))
                         ]) 
                         (manageProposal proposal)
            
manageProposal :: Proposal -> Task ()
manageProposal proposal = getByID proposal.Proposal.id
                      >>= \maybeResponse -> 
                          case maybeResponse of 
                              Just response = viewInformation "Proposal responses." [] response.ProposalResponse.responses
                              Nothing = undef // Should never occur!
                          ||- enterChoice "Select time" [ChooseFromGrid (\s -> s)] proposal.Proposal.when
                          >>* [ OnAction (Action "Schedule") (hasValue (\dateTime -> return ())),
                                OnAction (Action "Cancel proposal") (always (return ()))]
                      >>= const

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
