port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import WordPairs exposing (word_pairs)


port speak : ( String, Float ) -> Cmd msg


port done_speaking : (() -> msg) -> Sub msg


type alias Model =
    { pair : ( String, String )
    , which : Bool
    , done_speaking : Bool
    , num_correct : Int
    }


type Msg
    = GenQuestion ( ( String, String ), Bool )
    | DoneSpeaking
    | Choose Bool


gen_pair : Generator ( String, String )
gen_pair =
    (Random.int 0 (List.length word_pairs - 1))
        |> Random.map
            (\i ->
                word_pairs |> List.drop i |> List.head |> Maybe.withDefault ( "r", "l" )
            )


gen_question : Generator ( ( String, String ), Bool )
gen_question =
    Random.pair gen_pair Random.bool


init : Model
init =
    { pair = ( "r", "l" )
    , which = True
    , done_speaking = False
    , num_correct = 0
    }


answer_style : List ( String, String )
answer_style =
    [ ( "text-align", "center" )
    , ( "padding", "50px 20px" )
    , ( "border", "10px solid #aaa" )
    ]


ct_style : List ( String, String )
ct_style =
    [ ( "width", "300px" )
    , ( "display", "inline-block" )
    , ( "text-align", "center" )
    , ( "padding", "50px 50px" )
    ]


view_answer : Bool -> ( String, String ) -> Html Msg
view_answer which pair =
    div [ onClick (Choose which), style answer_style ]
        [ text
            ((if which then
                Tuple.first
              else
                Tuple.second
             )
                pair
            )
        , span [ style [ ( "display", "inline-block" ), ( "width", "20px" ) ] ] []
        , text
            ((if which then
                Tuple.second
              else
                Tuple.first
             )
                pair
            )
        ]


view : Model -> Html Msg
view model =
    div [ style [ ( "font-family", "sans" ), ( "font-size", "32px" ) ] ]
        [ div [ style ct_style ] [ view_answer True model.pair ]
        , div [ style ct_style ] [ view_answer False model.pair ]
        , div [] [ br [] [], text (toString model.num_correct) ]
        ]


next_question : Cmd Msg
next_question =
    Random.generate GenQuestion gen_question


speakit : Model -> Float -> Cmd Msg
speakit { pair, which } rate =
    speak
        ( (if which then
            (Tuple.first pair ++ ". " ++ Tuple.second pair ++ ".")
           else
            (Tuple.second pair ++ ". " ++ Tuple.first pair ++ ".")
          )
        , rate
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenQuestion ( pair, which ) ->
            let
                model_ =
                    { model | pair = pair, which = which, done_speaking = False }
            in
                ( model_
                , speakit model_ 1
                )

        Choose which ->
            if which == model.which then
                ( { model | num_correct = model.num_correct + 1 }, next_question )
            else
                ( { model | done_speaking = False }, speakit model 0.7 )

        DoneSpeaking ->
            ( { model | done_speaking = True }, Cmd.none )


main : Program Never Model Msg
main =
    program
        { init = ( init, next_question )
        , view = view
        , update = update
        , subscriptions = \m -> done_speaking (\_ -> DoneSpeaking)
        }
