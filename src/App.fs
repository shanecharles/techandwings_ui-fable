module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Types
open App.State
open Global
open System

importAll "../sass/main.scss"

open Fable.Helpers.React
open Fable.Helpers.React.Props

let loadingSpinner = i [ ClassName "fa fa-cog fa-3x fa-spin center" ] []

let dataLine (label : string) (data : string) =
  div [ ClassName "line" ] 
      [ div [ ClassName "label" ] [ unbox label ]
        div [ ClassName "data" ]  [ unbox data ]]

let monthToString : int -> string = function
  | 1  -> "January"
  | 2  -> "February"
  | 3  -> "March"
  | 4  -> "April"
  | 5  -> "May"
  | 6  -> "June"
  | 7  -> "July"
  | 8  -> "August"
  | 9  -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _  -> "No Month" 

let dayOfWeekToString = function
  | DayOfWeek.Sunday    -> "Sunday"
  | DayOfWeek.Monday    -> "Monday"
  | DayOfWeek.Tuesday   -> "Tuesday"
  | DayOfWeek.Wednesday -> "Wednesday"
  | DayOfWeek.Thursday  -> "Thursday"
  | DayOfWeek.Friday    -> "Friday"
  | DayOfWeek.Saturday  -> "Saturday"

let formatDateString (d : DateTime) = 
  sprintf "%s, %s %d, %d" 
    (d.DayOfWeek |> dayOfWeekToString)
    (d.Month |> monthToString)
    d.Day
    d.Year

let formatPeople : int option -> string = function
  | Some p -> p |> string
  | None   -> "¯\\_(ツ)_/¯"

let footerView = 
  div [ ClassName "footer"] 
       [ p [] [ unbox "Created with "
                a [ Href "http://fable.io" ] [ unbox "Fable" ]]
         p [] [ unbox "Source code found on "
                a [ Href "https://github.com/shanecharles/techandwings_ui-fable.git" ] [ unbox "GitHub" ]]
       ]

let nextMeetupView : Resource<Meetup option> -> React.ReactElement = function
  | Loading         -> div [] [ loadingSpinner ]
  | Loaded None     -> div [] [ unbox "There is no meetup scheduled yet. Check again later." ]
  | Loaded (Some m) -> div [] [ div [ ClassName "logo" ] []
                                div [ ClassName "next-details" ] 
                                    [ dataLine "When"  (formatDateString m.date)
                                      dataLine "Time" (m.date.ToString("t"))
                                      dataLine "Where" m.location ]
                              ]
  | Error err -> div [] [ unbox err ]

let previousMeetupView : Resource<Meetup option> -> React.ReactElement = function
  | Loading           -> div [] [ loadingSpinner ]
  | Loaded (None)     -> div [] [ unbox "Was there really a previous meeting?"]
  | Loaded (Some m)   -> div [] [ dataLine "When" (m.date |> formatDateString) 
                                  dataLine "Where" m.location
                                  dataLine "People" (m.people |> formatPeople)
                                  dataLine "Topics" m.topics
                                ]    
  | Error msg         -> div [] [ unbox msg ]

let futureMeetupsView : Resource<Meetup list> -> React.ReactElement = function
  | Loading    -> div [] [ loadingSpinner ]
  | Loaded []  -> div [] [ unbox "No meetups planned at the moment... check back later." ]
  | Loaded mts -> div [] (mts |> List.map (fun m -> dataLine "When" (m.date |> formatDateString)))
  | Error msg  -> div [] [ unbox msg ]


let root model dispatch =
  div
    [ ClassName "main" ]
    [ div [ ClassName "header" ]
        [ h1 [] [ unbox "Tech & Wings" ]
          h2 [] [ unbox "A group of developers, IT specialiasts, and enthusiasts gathering together to talk all kinds of tech related topics." ] 
        ]
      div [ ClassName "next-meetup" ]
        [ h1 [] [ unbox "Next Meetup" ]
          div [ ClassName "container" ] [ nextMeetupView model.next ] 
        ]
      div [ ClassName "previous-meetup" ]
        [ h2 [] [ unbox "Previous Meetup" ]
          div [ ClassName "container" ] [ previousMeetupView model.previous ]
        ]  
      div [ ClassName "future-meetups" ]
        [ h2 [] [ unbox "Future Meetups" ]
          div [ ClassName "container" ] [ futureMeetupsView model.future ]
        ]
      footerView
    ]
  
open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
