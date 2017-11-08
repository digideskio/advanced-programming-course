module e1_AppointmentSystem

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import _SystemArray
import iTasks

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

tasks :: [Workflow]
tasks = [
    
    ]

Start w = startEngine [publish "/" (\_ -> loginAndManageWorkList "Appointments"  tasks)] w
