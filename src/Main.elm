module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Random.List



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { state : State
    , gameCount : Int
    , answerCount : Int
    , question : Question
    , answerCandidates : List Int
    }


type State
    = Start
    | Game
    | Finish


type alias Question =
    { x : Int
    , y : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Start
      , gameCount = 1
      , answerCount = 0
      , question = { x = 0, y = 0 }
      , answerCandidates = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = StartGame
    | SelectAnswer Int
    | ResetGame
    | QuestionGenerated Question
    | AnswerCandidatesGenerated (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            ( { model | state = Game, gameCount = 1, answerCount = 0 }
            , randomQuestion
            )

        SelectAnswer answer ->
            let
                newCount =
                    model.gameCount + 1

                newState =
                    if newCount <= 10 then
                        Game

                    else
                        Finish

                newAnswerCount =
                    if validAnswer model.question answer then
                        model.answerCount + 1

                    else
                        model.answerCount
            in
            ( { model | state = newState, gameCount = newCount, answerCount = newAnswerCount }, randomQuestion )

        ResetGame ->
            ( { model | state = Start }, Cmd.none )

        QuestionGenerated generatedQuestion ->
            ( { model | question = generatedQuestion }, randomAnswerCandidates generatedQuestion )

        AnswerCandidatesGenerated generatedAnswerCandidates ->
            if isUniqueList generatedAnswerCandidates then
                ( { model | answerCandidates = generatedAnswerCandidates }, Cmd.none )

            else
                ( model, randomAnswerCandidates model.question )


isUniqueList : List Int -> Bool
isUniqueList list =
    case List.head list of
        Just elem ->
            case List.tail list of
                Just tailList ->
                    if List.member elem tailList then
                        False

                    else
                        isUniqueList tailList

                Nothing ->
                    True

        Nothing ->
            True


questionGenerator : Random.Generator Question
questionGenerator =
    let
        generateQuestion : Random.Generator Question
        generateQuestion =
            Random.map2
                Question
                (Random.int 1 99)
                (Random.int 1 99)

        loop : Random.Generator Question -> Random.Generator Question
        loop question =
            question
                |> Random.andThen
                    (\q ->
                        if q.x - q.y > 0 then
                            Random.constant q

                        else
                            generateQuestion |> loop
                    )
    in
    loop (Random.constant { x = 0, y = 0 })


answerCandidatesGenerator : Question -> Random.Generator (List Int)
answerCandidatesGenerator question =
    Random.list 2 (Random.int 0 99)
        |> Random.andThen
            (\list ->
                Random.List.shuffle <| (question.x - question.y) :: list
            )


randomQuestion : Cmd Msg
randomQuestion =
    Random.generate QuestionGenerated questionGenerator


randomAnswerCandidates : Question -> Cmd Msg
randomAnswerCandidates question =
    answerCandidatesGenerator question |> Random.generate AnswerCandidatesGenerated


validAnswer : Question -> Int -> Bool
validAnswer question answer =
    (question.x - question.y) == answer


type Grade
    = Excellent
    | Good
    | Bad


evaluation : Int -> Grade
evaluation answerCount =
    if answerCount > 9 then
        Excellent

    else if answerCount > 4 then
        Good

    else
        Bad



-- VIEW


view : Model -> Html Msg
view model =
    Html.div [ class "container" ]
        [ case model.state of
            Start ->
                startView model

            Game ->
                gameView model

            Finish ->
                finishView model
        ]


startView : Model -> Html Msg
startView _ =
    Html.div [ class "content center" ]
        [ Html.p [ class "title" ]
            [ text "たのしいさんすう"
            , Html.br [] []
            , text "ひきざんへん"
            ]
        , Html.button [ class "button", onClick StartGame ] [ text "スタート" ]
        ]


gameView : Model -> Html Msg
gameView model =
    Html.div [ class "content center" ]
        [ Html.p [ class "title" ] [ text (String.fromInt model.gameCount ++ "もんめ") ]
        , Html.p [ class "title" ]
            [ Html.span [] [ String.fromInt model.question.x |> text ]
            , Html.span [] [ text " - " ]
            , Html.span [] [ String.fromInt model.question.y |> text ]
            , Html.span [] [ text " = ?" ]
            ]
        , Html.div [ class "candidates has-margin-top" ] <|
            List.map
                (\answer ->
                    Html.button
                        [ class "button"
                        , onClick <| SelectAnswer answer
                        ]
                        [ String.fromInt answer |> text ]
                )
                model.answerCandidates
        ]


finishView : Model -> Html Msg
finishView model =
    Html.div [ class "content center" ]
        [ answerRateView model.answerCount
        , Html.div [ class "title" ] <|
            case evaluation model.answerCount of
                Excellent ->
                    [ text "さすがだね！" ]

                Good ->
                    [ text "がんばったね！" ]

                Bad ->
                    [ text "つぎはもっとがんばろう！" ]
        , Html.button [ class "button", onClick ResetGame ] [ text "はじめにもどる" ]
        ]


answerRateView : Int -> Html Msg
answerRateView answerCount =
    Html.div [ class "title" ]
        [ String.join "" [ "10もん ちゅう ", String.fromInt answerCount, "もん せいかい！" ] |> text
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
