port module Main exposing (..)

{-
   These ports get data in from Torus to initialize the exercise and then send
   data back to Torus to let it know that we're done.

   I have to use a port instead of flags because Torus doesn't get all the initialization
   data to my code quickly enough.
-}

import Browser
import Element
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
    }


type RightOrWrong
    = RightAnswer -- user chose the correct answer
    | WrongAnswer -- user chose the wrong answer
    | NothingYet -- this is used by progress bar when the window is bigger than the number of responses


initialModel : Model
initialModel =
    { progress = List.repeat 6 NothingYet
    , threshold = 4
    , window = 6
    , debug = True
    }


initializeModel : () -> ( Model, Cmd Msg )
initializeModel _ =
    ( initialModel
    , Cmd.none
    )



{-
   The view consists of five stacked panels. I only display/update the panels that I need to at any given time.

   I use the status member of the model to determine which panels get displayed.
-}


viewModel : Model -> Html.Html Msg
viewModel model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (viewDebugInfo model)


viewDebugInfo : Model -> Element.Element Msg
viewDebugInfo model =
    let
        debugging : String
        debugging =
            if model.debug then
                "Debugging"

            else
                "Not Debugging"
    in
    Element.column
        []
        [ Element.paragraph [] [ Element.text ("threshold: " ++ String.fromInt model.threshold) ]
        , Element.paragraph [] [ Element.text ("window: " ++ String.fromInt model.window) ]
        , Element.paragraph [] [ Element.text debugging ]
        ]



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


mySubscriptions : Model -> Sub Msg
mySubscriptions _ =
    getFromTorus MsgGetFromTorus
