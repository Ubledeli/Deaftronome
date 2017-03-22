import Html exposing (Html, div, input, label, button)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onInput, onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import AnimationFrame
import Keyboard exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { running : Bool
    , timeDiff : Time
    , beatCount : Int
    , bpm : Int
    , fillColor : String
    , increment : Time
    , interval : Int
    }


defaultBeatCount = 2

defaultBpm = 60

br = 1000

init : ( Model, Cmd Msg )
init =
    let
        timeDiff = 0

        beatCount = defaultBeatCount

        bpm = defaultBpm

        fillColor = "#0B79CE"

        increment = 0 * Time.millisecond

        interval = 60000 // (beatCount * bpm)
    in
        ( (Model False timeDiff beatCount bpm fillColor increment interval), Cmd.none )


type Msg
    = TimeUpdate Time
    | BpmUpdate String
--    | BeatUpdate String
    | StartRunning
    | StopRunning


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate deltaT ->
            ( (updateColor model deltaT), Cmd.none )


        BpmUpdate newBpm ->
            let
                intBpm =
                    case String.toInt newBpm of
                        Err msg ->
                            0

                        Ok intBpm ->
                            intBpm
            in
                ( (updateBpm model intBpm), Cmd.none )

{--        BeatUpdate newBeatCount ->
            let
                beatCount =
                    case String.toInt newBeatCount of
                        Err msg ->
                            defaultBeatCount

                        Ok converted ->
                            converted
            in
                ( (updateBeatCount model beatCount), Cmd.none )
--}
        StartRunning ->
            ( { model | running = True }, Cmd.none )

        StopRunning ->
            ( { model | running = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.running of
        True ->
            AnimationFrame.diffs TimeUpdate

        False ->
            Sub.none


view : Model -> Html Msg
view model =
    div []
        [ {--div []
            [ label []
                [ Html.text "BPM"
                , input [ type_ "text", value (toString model.bpm), onInput BpmUpdate ] []
                ]
            ]
{--        ,--} div []
            [ label []
                [ Html.text "Beat Count"
                , input [ type_ "text", value (toString model.beatCount), onInput BeatUpdate ] []
                ]
            ] FALI ZAREZ
--}
         div [] [ startButton model.running ]
        , svg [ viewBox "0 0 100 100", width "800px" ] --(buildFace model)
        [ circle [ cx "50", cy "50", r "45", fill model.fillColor ] []
        ]

        , div []
        [ Html.h3 [] [ text (toString model.timeDiff) ]]
        , div []
        [ Html.h3 [] [ text (toString model.interval) ]]
        --}
    ]

startButton : Bool -> Html Msg
startButton running =
    case running of
        True ->
            button [ onClick StopRunning ] [ text "Stop" ]

        False ->
            button [ onClick StartRunning ] [ text "Start" ]

updateBpm : Model -> Int -> Model
updateBpm model newBpm =
    { model | bpm = newBpm }


updateColor : Model -> Time -> Model
updateColor model deltaT =
    let
        br = model.interval
        --br1 : Time
        --br1 = model.interval * Time.millisecond
        ms = br
        ms1 = (br - 50)
        mil = round (Time.inMilliseconds model.increment)
        fillColor =
            if model.fillColor == "#0B79CE" then
                "#fe57a1"
            else
                "#0B79CE"
    in

    if mil < ms1 then
        { model | increment = model.increment + deltaT, fillColor = "#0B79CE"}
    else if mil < ms then
        { model | increment = model.increment + deltaT, fillColor = "#fe57a1"}
    else
        { model | timeDiff = deltaT, fillColor = "#0B79CE", increment = model.increment - (toFloat ms)}
