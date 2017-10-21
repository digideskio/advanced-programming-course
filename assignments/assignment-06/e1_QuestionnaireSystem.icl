module e1_QuestionnaireSystem

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import _SystemArray
import iTasks

:: Question =
  { question :: String
  , answers  :: [String]
  , correct  :: Int
  }

derive class iTask Question

mockquestions :: [Question]
mockquestions = [{question="Pick A", answers=["A","B","C"], correct=0}, {question="Don't pick A", answers=["A","B"], correct=1} ]

// mapRead pattern from Examples/iTasks/BasicAPIExamples.icl
teacherTask :: (Shared [Question]) -> Task ()
teacherTask questions = enterChoiceWithShared "Teacher menu - Choose an item to edit" [ChooseFromGrid id] (mapRead (\qs -> [(i,q) \\ q <- qs & i <- [0..]]) questions)
             >>* [ OnAction (Action "Append") (hasValue (\(i,_) -> recurse (applyQ (insertAt (i+1) defaultValue))))
                 , OnAction (Action "Delete") (hasValue (\(i,_) -> recurse (applyQ (removeAt i))))
                 , OnAction (Action "Edit")   (hasValue (\(i,p) -> recurse (confirmQuitEditor i (updateInformation "Edit question" [] p))))
                 , OnAction (Action "Clear")  (hasValue (\(i,_) -> recurse (confirmQuitEditor i (enterInformation "Edit question" []))))
                 , OnAction (Action "First")  (always             (recurse (applyQ (insertAt 0 defaultValue))))
                 , OnAction (Action "Quit")   (always (return ()))
                 ]
    where confirmQuitEditor i task = let update = \p -> applyQ (updateAt i p) in 
                 task >>*
                 [ OnAction ActionContinue (hasValue update)
                 , OnValue (ifStable update)
                 , OnAction ActionCancel (always (return defaultValue))
                 ]
          recurse task = task ||- teacherTask questions
          applyQ f = upd f questions
          
administratorTask :: (Shared [Question]) -> Task ()
administratorTask questions = updateSharedInformation "Administrator menu - Choose an item to edit" [] questions
                   >>= \_ -> return ()

// Ground
undef = undef

studentTask :: (Shared [Question]) -> Task ()
studentTask questions = get questions 
             >>= \questions -> answerQuestions questions []
             >>= \answers -> gradeAnswers questions answers
    where
    answerQuestions :: [Question] [Int] -> Task [Int]
    answerQuestions [] answers = return answers
    answerQuestions [q:qs] answers = enterChoice q.question [ChooseFromGrid (\answer -> q.answers !! answer)] [0 .. length q.answers - 1]
                     >>= \answer -> answerQuestions qs (answers ++ [answer])
    gradeAnswers :: [Question] [Int] -> Task ()
    gradeAnswers questions answers
                  # numCorrect = sum [if (q.correct == a) 1 0 \\ q <- questions & a <- answers]
                    numQuestions = length questions
                    questionQuestions = if (numQuestions == 1) "question" "questions" 
                  = viewInformation "Your score" [] ("You answered " +++ toString numCorrect +++ " out of " +++ toString numQuestions +++ " " +++ questionQuestions +++ " correctly.")
                  >>= \_ -> return ()

mainTask :: Task ()
mainTask = withShared mockquestions studentTask
// TODO change mockquestions to [] when done testing

Start w = startEngine mainTask w