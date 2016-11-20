import Html.App as App
import Html exposing (div, Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput, onClick)
import Array
import String
import Time exposing (Time, second)

type Msg
  = InstructionUpdated (Int, String)
  | Play
  | Debug
  | Reset
  | Tick Time


type alias Node = {
    instructions : List String
  , bak : Int
  , acc : Int
  , left : Int
  , right : Int
  , activeInstruction : Int
  , inputRead: Bool
  , outputWritten: Bool
  }


type alias Model = {
    node : Node
  , inputs : List Int
  , activeInput : Int
  , expectedOutputs : List Int
  , actualOutputs : List Int
  , autoRun : Bool
  }

instruction : String
instruction =
  ""

computeNode : Node
computeNode =
  Node [ instruction
       , instruction
       , instruction
       , instruction
       , instruction
       , instruction
       , instruction
       , instruction
       , instruction
       ] 0 0 0 0 0 False False


inputs2 : List Int
inputs2 =
  [1, 2, 3, 12, 18, 41, 16, 9, 11, 8, 12, 23]

expectedOutputs2 : List Int
expectedOutputs2 =
  [2, 4, 6, 24, 36, 82, 32, 18, 22, 16, 24, 46]

inputs1 : List Int
inputs1 =
  [1,2,3,4]

expectedOutputs1 : List Int
expectedOutputs1 =
  [1,2,3,4]

init : Model
init =
  Model computeNode inputs2 0 expectedOutputs2 [] False

nodeStyle : Attribute msg
nodeStyle =
  style [ ("padding", "10px")
        , ("margin", "40px")
        , ("border", "1px solid white")
        , ("align-self", "flex-start")
        ]

instructionStyle : Int -> Int -> Attribute msg
instructionStyle active current =
  let
    color = if active == current then
              "1px dotted red"
            else
              "1px solid white"
  in
      style [ ("width", "310px")
            , ("font-size", "15px")
            , ("margin", "1px")
            , ("margin-left", "7px")
            , ("background-color", "black")
            , ("border", color)
            , ("color", "white")
        ]

instructionSetStyle : Attribute msg
instructionSetStyle =
  style [("padding-top", "10px")]

viewNode : Node -> Html Msg
viewNode node =
  Html.div [ nodeStyle
           ]
        [ viewInstructions node
        , viewLoc "ACC " node.acc
        , viewLoc "BAK " node.bak
        , viewLoc "LEFT " node.left
        , viewLoc "RIGHT " node.right
        ]

viewLoc : String -> Int -> Html Msg
viewLoc name location =
    Html.div []
        [ Html.text name
        , Html.text (toString location)
        ]

viewInstructions : Node -> Html Msg
viewInstructions node =
  Html.div [instructionSetStyle]
    (List.indexedMap (viewInstruction node.activeInstruction) node.instructions)

viewInstruction : Int -> Int -> String -> Html Msg
viewInstruction active id instruction =
  Html.div [] [ Html.text (toString id)
              , Html.input [(instructionStyle active id), onInput (\text -> InstructionUpdated (id, text))] []
              ]

viewList : String -> Int -> List Int -> Html Msg
viewList title active list =
  Html.div [style
              [ ("padding", "10px")
              , ("margin", "40px")
              , ("border", "1px solid white")
              , ("align-self", "flex-start")
              ]
           ]
    [ Html.text title
              , Html.ul [] (List.indexedMap (viewItem active) list)
              ]

viewItem : Int -> Int -> Int -> Html Msg
viewItem active id item =
  let
    color = if id == active then
              "green"
            else
              "white"
  in
      Html.li [style [("color", color)]] [Html.text (toString item)]


viewButtons : Model -> Html Msg
viewButtons model =
    Html.div []
      [ buttonViewer Reset Play "RESET" "PLAY" (model.autoRun)
      , buttonViewer Debug Reset "STEP" "RESET" (moreToRead model)
      ]


buttonViewer : Msg -> Msg -> String -> String -> Bool -> Html Msg
buttonViewer yes no yesd nod on =
  if on then
    Html.div [buttonStyle, onClick yes] [ Html.text  yesd]
  else
    Html.div [buttonStyle, onClick no] [ Html.text  nod]

buttonStyle : Attribute msg
buttonStyle =
  style
  [ ("padding", "10px")
  , ("margin", "40px")
  , ("border", "1px solid white")
  , ("align-self", "flex-start")
  ]

view : Model -> Html Msg
view model =
  Html.div [style
              [ ("height", "100%")
              , ("padding", "0")
              , ("margin", "0")
              , ("display", "-webkit-box")
              , ("display", "-moz-box")
              , ("display", "-ms-flexbox")
              , ("display", "-webkit-flex")
              , ("display", "flex")
              , ("align-items", "center")
              , ("justify-content", "center")
              , ("background-color", "black")
              , ("color", "white")
              ]
           ]
    [ viewList "INPUTS" model.activeInput model.inputs
    , viewNode model.node
    , viewList "EXPECTED" -1 model.expectedOutputs
    , viewList "ACTUAL" -1 model.actualOutputs
    , viewButtons model
    ]

moreToRead : Model -> Bool
moreToRead model =
  model.activeInput < List.length model.inputs

update : Msg -> Model -> Model
update msg model =
  case msg of
    InstructionUpdated (id, text) ->
      let
        node = model.node
        updatedInstructions = (Array.set id text (Array.fromList node.instructions))
        newNode = { node | instructions = (Array.toList updatedInstructions) }
      in
        { model | node = newNode }
    Play ->
      { model | autoRun = not model.autoRun }
    Debug ->
      runInstruction model
    Reset ->
      resetModel model
    Tick time ->
      if model.autoRun && moreToRead model then
        runInstruction model
      else
        model

resetModel : Model -> Model
resetModel model =
    let
        node = model.node
        resetNode = { node |
                      bak = 0
                    , acc = 0
                    , left = 0
                    , right = 0
                    , activeInstruction = 0
                    , inputRead = False
                    , outputWritten = False
                    }
      in
          { model |
              activeInput = 0
          , actualOutputs = []
          , node = resetNode
          , autoRun = False
          }

runInstruction : Model -> Model
runInstruction model =
    let
      curInput = case (Array.get model.activeInput (Array.fromList model.inputs)) of
                         Just input ->
                           input
                         Nothing ->
                           10000
      node = model.node
      preNode = { node |
                  left = curInput
                , inputRead = False
                , right = 0
                , outputWritten = False
                }
      newNode = runNode preNode

      nextInput = if newNode.inputRead then
                    model.activeInput + 1
                  else
                    model.activeInput

      output = if newNode.outputWritten then
                 model.actualOutputs ++ [newNode.right]
               else
                 model.actualOutputs

    in
      { model |
        node = newNode
      , activeInput = nextInput
      , actualOutputs = output
      }


loopingNext : List a -> Int -> Int
loopingNext list index =
  if (List.length list) - 1 <= index then
    0
  else
    index + 1

runNode : Node -> Node
runNode node =
    let
      curInstruction = case (Array.get node.activeInstruction (Array.fromList node.instructions)) of
                         Just instruction ->
                           instruction
                         Nothing ->
                           "skip"
    in
      applyInstruction curInstruction node

getOutput : List String -> Int -> Node -> Node
getOutput instructions input node =
  let
    nodeWithInput = { node | left = input }
  in
    List.foldl applyInstruction nodeWithInput instructions

applyInstruction : String -> Node -> Node
applyInstruction instruction node =
  case (tokenize instruction) of
    "MOV"::rest ->
      mov node rest
    "INC"::_ ->
      inc node
    "SWP"::_ ->
      swp node
    "ADD"::rest ->
      add node rest
    _ -> {node | activeInstruction = loopingNext node.instructions node.activeInstruction }

add : Node -> List String -> Node
add node locations =
  case locations of
    "LEFT"::_ ->
      let
        newAcc = node.acc + node.left
        nextInstruction = loopingNext node.instructions node.activeInstruction
      in
        { node | acc = newAcc, left = 0, inputRead = True, activeInstruction = nextInstruction }
    "ACC"::_ ->
      let
        newAcc = node.acc + node.acc
      in
        { node | acc = newAcc, left = 0, activeInstruction = node.activeInstruction + 1 }
    _ -> node


mov : Node -> List String -> Node
mov node locations =
  let
    -- TODO this is the granularity we need to refresh at for `step`
    x = Debug.log "node" node
    y = Debug.log "node" locations
    nextInstruction = loopingNext node.instructions node.activeInstruction
  in
      case locations of
        "LEFT"::"RIGHT"::_ ->
          { node |
            right = node.left
          , inputRead = True
          , outputWritten = True
          , activeInstruction = nextInstruction
          }
        "LEFT"::"ACC"::_ ->
          let
            left = node.left
          in
              { node |
                acc = left
              , inputRead = True
              ,  activeInstruction = nextInstruction
              }
        "ACC"::"RIGHT"::_ ->
          { node |
            right = node.acc
          , inputRead = False
          , outputWritten = True
          , activeInstruction = nextInstruction
          }
        from::"ACC"::_ ->
          case String.toInt(from) of
            Ok val ->
              { node |
                acc = val
              , activeInstruction = nextInstruction
              }
            Err _ ->
              node
        from::"RIGHT"::_ ->
          case String.toInt(from) of
            Ok val ->
              { node |
                right = val
              , outputWritten = True
              , activeInstruction = nextInstruction
              }
            Err _ ->
              node
        _ -> node

inc : Node -> Node
inc node =
  let
    newAcc = node.acc + 1
    nextInstruction = loopingNext node.instructions node.activeInstruction
  in
    { node |
      acc = newAcc
    , activeInstruction = nextInstruction
    }

swp : Node -> Node
swp node =
  let
    acc = node.acc
    bak = node.bak
    nextInstruction = loopingNext node.instructions node.activeInstruction
  in
    { node |
      acc = bak
    , bak = acc
    , activeInstruction = nextInstruction
    }

tokenize : String -> List String
tokenize string =
  String.split " " string

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [Time.every Time.second Tick]

main : Program Never
main =
  App.program
    { init = (init, Cmd.none)
    , update = \msg model -> (update msg model, Cmd.none)
    , view = view
    , subscriptions = subscriptions
    }
