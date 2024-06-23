port module Main exposing (..)

{-
   These ports get data in from Torus to initialize the exercise and then send
   data back to Torus to let it know that we're done.

   I have to use a port instead of flags because Torus doesn't get all the initialization
   data to my code quickly enough.
-}

import Browser
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html


type alias Settings =
    { threshold : Int -- User needs to get <threshold> questions right...
    , window : Int -- out of the last <window> questions
    , debug : Bool -- True when we should show debug info
    }


port getFromTorus : (Settings -> msg) -> Sub msg


port sendToTorus : Bool -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = initializeModel
        , view = viewModel
        , update = updateModel
        , subscriptions = mySubscriptions
        }



{-
   This is the model for the state. I keep the question in here along with keeping track of
   the user's response to the question being asked, how many questions the user has gotten
   right, and the number of questions the user is supposed to be asked.
-}


type alias Model =
    { progress : List RightOrWrong -- How many questions has the user gotten right/wrong?
    , threshold : Int -- How many questions does the user need to get right?
    , window : Int -- How big is the window for reaching the threshold?
    , debug : Bool -- Do we show debug information?
    , question : Question -- What question do we show to the user?
    }


type RightOrWrong
    = RightAnswer -- user chose the correct answer
    | WrongAnswer -- user chose the wrong answer
    | NothingYet -- this is used by progress bar when the window is bigger than the number of responses


type alias QuestionResponse =
    { textPart : String -- What gets displayed on the button for the user to choose
    , feedback : String -- The feedback associated with this answer
    , correctAnswer : Bool -- True when this is the right answer
    }


correctResponse : QuestionResponse
correctResponse =
    { textPart = "This is the right answer"
    , feedback = "You chose the right answer"
    , correctAnswer = True
    }


oneDistractor : QuestionResponse
oneDistractor =
    { textPart = "This is the first distractor"
    , feedback = "You chose the first distractor"
    , correctAnswer = False
    }


anotherDistractor : QuestionResponse
anotherDistractor =
    { textPart = "This is the second distractor"
    , feedback = "You chose the second distractor"
    , correctAnswer = False
    }


type alias ScatterPlot =
    { predictorVariable : String -- name of x-axis variable
    , responseVariable : String -- name of y-axis variable
    , slope : Float -- the m of y = mx + b
    , yIntercept : Float -- the b of y = mx + b
    , standardDeviation : Float -- how much items vary around the line
    }


sampleScatterPlot : ScatterPlot
sampleScatterPlot =
    { predictorVariable = "Age"
    , responseVariable = "Strength"
    , slope = 2.0
    , yIntercept = 3.0
    , standardDeviation = 0.5
    }


type alias Question =
    { stem : String -- Question that gets shown to the user
    , figure : ScatterPlot -- Image that goes along with the question
    , possibleResponses : List QuestionResponse -- List of answers the user can choose from
    }


sampleQuestion : Question
sampleQuestion =
    { stem = "What is the predictor variable?"
    , figure = sampleScatterPlot
    , possibleResponses = [ correctResponse, oneDistractor, anotherDistractor ]
    }


initialModel : Model
initialModel =
    { progress = List.repeat 6 NothingYet
    , threshold = 4
    , window = 6
    , debug = True
    , question = sampleQuestion
    }


initializeModel : () -> ( Model, Cmd Msg )
initializeModel _ =
    ( initialModel
    , Cmd.none
    )



{-
   The view consists of stacked rows. I only display/update the panels that I need to at any given time.

    Question + Image + Buttons for possible responses
    Feedback (after user submits answer)
    Progress
    Debug

   I use the status member of the model to determine which panels get displayed.
-}


viewModel : Model -> Html.Html Msg
viewModel model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (Element.column
            [ Element.width Element.fill ]
            [ viewQuestionPanel model.question
            , viewProgressPanel model.progress
            , viewDebugPanel model
            ]
        )


viewQuestionPanel : Question -> Element.Element Msg
viewQuestionPanel question =
    let
        drawButton index btn =
            Element.Input.button
                [ Element.padding 10
                , Element.Border.width 3
                , Element.Border.rounded 6
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.Font.variant Element.Font.smallCaps
                , Element.width (Element.fillPortion 1)
                ]
                { onPress = Just (MsgUserPressedBtn index)
                , label = Element.el [ Element.centerX ] (Element.text btn.textPart)
                }
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 20
        , Element.explain Debug.todo
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.column
                [ Element.width (Element.fillPortion 1)
                , Element.padding 10
                ]
                [ Element.text question.stem ]
            , Element.column
                [ Element.width (Element.fillPortion 1)
                , Element.padding 10
                ]
                [ Element.el [ Element.centerX ] (Element.text "Image goes here") ]
            ]
        , Element.row
            [ Element.width Element.fill ]
            (List.indexedMap drawButton question.possibleResponses)
        ]


viewProgressPanel : List RightOrWrong -> Element.Element Msg
viewProgressPanel progress =
    let
        -- Creates an empty element with a border (a box) for each item in progress list
        drawProgressBox p =
            let
                fillColor =
                    case p of
                        RightAnswer ->
                            Element.rgb255 0 255 0

                        WrongAnswer ->
                            Element.rgb255 255 0 0

                        NothingYet ->
                            Element.rgb 255 255 255
            in
            Element.el
                [ Element.Background.color fillColor
                , Element.padding 10
                , Element.Border.rounded 6
                , Element.Border.width 3
                , Element.Border.color (Element.rgb255 0 0 0)
                , Element.height Element.fill
                , Element.width (Element.fillPortion 1)
                ]
                Element.none
    in
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px 100)
        , Element.padding 20
        ]
        (List.map drawProgressBox progress)


viewDebugPanel : Model -> Element.Element Msg
viewDebugPanel model =
    if model.debug then
        Element.column
            [ Element.width Element.fill
            , Element.padding 20
            ]
            [ Element.paragraph [] [ Element.text ("threshold: " ++ String.fromInt model.threshold) ]
            , Element.paragraph [] [ Element.text ("window: " ++ String.fromInt model.window) ]
            , Element.paragraph [] [ Element.text "Debugging" ]
            ]

    else
        Element.none



{-
   The flow for this program:
   * Initialize the exercise
       * GetDataFromTorus
   * Make a question
       * GetNextQuestion
       * GotRandomQuestionParameters
           - If the numbers combine to make unique answers -> GotRandomQuestion
           - Otherwise -> GetNextQuestion
   * Present the question and wait for a response
   * Evaluate whether they got it right
       * GotResponse
           - If they are done, then send control back to Torus -> ReturnToTorus
           - Otherwise, get the next question -> GetNextQuestion
-}


type Msg
    = MsgSendToTorus -- The user reached the threshold, go back to Torus (send to JavaScript)
    | MsgGetFromTorus Settings -- Settings for mastery questions coming in from Torus (get from JavaScript)
    | MsgUserPressedBtn Int -- User chose one of the answer buttons


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    case msg of
        -- The user has demonstrated mastery, kick control back to Torus
        MsgSendToTorus ->
            ( model, sendToTorus True )

        -- Data to initialize the exercise has come in from Torus.
        MsgGetFromTorus settings ->
            ( { model
                | threshold = settings.threshold -- threshold comes from JS
                , window = settings.window -- window comes from JS
                , debug = settings.debug -- debug flag comes from JS
                , progress = List.repeat settings.window NothingYet -- progress gets reset to a whole lot of nothing
              }
            , Cmd.none
            )

        -- User chose one of the answer buttons
        MsgUserPressedBtn btnNumber ->
            ( model, Cmd.none )


mySubscriptions : Model -> Sub Msg
mySubscriptions _ =
    getFromTorus MsgGetFromTorus
