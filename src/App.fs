module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Fable.PowerPack
open Fable.PowerPack.Fetch
open System

importAll "../sass/main.scss"

open Fable.Helpers.React
open Fable.Helpers.React.Props

type Meetup = { 
  date     : DateTime
  location : string
  people   : int option
  topics   : string
}

type Resource<'a> =
  | Loading
  | Loaded of 'a
  | Error of string

type Model = {
  next     : (Meetup option) Resource
  previous : (Meetup option) Resource
  future   : (Meetup list) Resource
  meetups  : (Meetup list) option
}

type Msg =
  | Fetch
  | FetchResult of Meetup list
  | FetchFail of Exception

let init result = 
  ({ next     = Loading
     previous = Loading
     future   = Loading
     meetups  = None
     }, Cmd.ofMsg Fetch)

// add aria-hidden "true"
let loadingSpinner = i [ ClassName "fa fa-cog fa-3x fa-spin center" ] []

let dataLine label data =
  div [ ClassName "line" ] 
      [ div [ ClassName "label" ] [ unbox label ]
        div [ ClassName "data" ]  [ unbox data ]]


let monthToString = function
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

let dayOfWeekToString (d : DateTime) =
  match d.DayOfWeek with 
  | DayOfWeek.Sunday -> "Sunday"
  | DayOfWeek.Monday -> "Monday"
  | DayOfWeek.Tuesday -> "Tuesday"
  | DayOfWeek.Wednesday -> "Wednesday"
  | DayOfWeek.Thursday -> "Thursday"
  | DayOfWeek.Friday -> "Friday"
  | DayOfWeek.Saturday -> "Saturday"

let formatDateString (d : DateTime) = 
  d.ToString("D")
  //sprintf "%s, %s %d, %d" (dayOfWeekToString d) (monthToString d.Month) d.Day d.Year

let formatPeople = function
  | Some p -> p |> string
  | None   -> "¯\\_(ツ)_/¯"

let footerView = 
  div [ ClassName "footer"] 
       [ p [] [ unbox "Created with "
                a [ Href "http://fable.io" ] [ unbox "Fable" ]]
         p [] [ unbox "Source code found on "
                a [ Href "https://github.com/shanecharles/techandwings_ui-fable.git" ] [ unbox "GitHub" ]]
       ]

let nextMeetupView = function
  | Loading         -> div [] [ loadingSpinner ]
  | Loaded None     -> div [] [ unbox "There is no meetup scheduled yet. Check again later." ]
  | Loaded (Some m) -> div [] 
                        [ div [ ClassName "logo" ] []
                          div [ ClassName "next-details" ] 
                            [ dataLine "When"  (formatDateString m.date)
                              dataLine "Time" (m.date.ToString("t"))
                              dataLine "Where" m.location
                          ]]
  | Error err -> div [] [ unbox err ]

let previousMeetupView = function
  | Loading           -> div [] [ loadingSpinner ]
  | Loaded (None)     -> div [] [ unbox "Was there really a previous meeting?"]
  | Loaded (Some m)   -> div [] [ dataLine "When" (m.date |> formatDateString) 
                                  dataLine "Where" m.location
                                  dataLine "People" (m.people |> formatPeople)
                                  dataLine "Topics" m.topics
                                ]    
  | Error msg         -> div [] [ unbox "uh oh... someone talk with the dev!" ]

let futureMeetupsView = function
  | Loading    -> div [] [ loadingSpinner ]
  | Loaded []  -> div [] [ unbox "No meetups planned at the moment... check back later." ]
  | Loaded mts -> div [] (mts |> List.map (fun m -> dataLine "When" (m.date |> formatDateString)))
  | Error msg  -> div [] [ unbox "uh oh... someone talk with the dev!" ]

let root model dispatch =
  div
    [ ClassName "main" ]
    [ div [ ClassName "header" ]
        [ h1 [] [ unbox "Tech & Wings" ]
        ; h2 [] [ unbox "A group of developers, IT specialiasts, and enthusiasts gathering together to talk all kinds of tech related topics." ] 
        ]
      div [ ClassName "next-meetup" ]
        [ h1 [] [ unbox "Next Meetup" ]
        ; div [ ClassName "container" ] [ nextMeetupView model.next ] 
        ]
      div [ ClassName "previous-meetup" ]
        [ h2 [] [ unbox "Previous Meetup" ]
        ; div [ ClassName "container" ] [ previousMeetupView model.previous ]
        ]  
      div [ ClassName "future-meetups" ]
        [ h2 [] [ unbox "Future Meetups" ]
        ; div [ ClassName "container" ] [ futureMeetupsView model.future ]
        ]
      footerView
    ]

let fetchUrl url = promise {
  let! meetups = fetchAs<Meetup[]> url []
  return meetups
  }

let fetchMeetups () =
  let dataUrl = "https://techandwingsapi.azurewebsites.net/api/meetups"
  fetchUrl dataUrl
  |> Promise.map (List.ofSeq >> FetchResult) 
  |> Promise.catch FetchFail
  

let updateModel meetups = 
  let cutoff = DateTime.Now.AddMinutes(-30.)
  let prev, active = meetups |> List.partition (fun m -> m.date < cutoff)
  
  let next, future =
    match active with
    | h :: t  -> (Some h), t
    | _       -> None, [] 

  { next = Loaded next
    previous = Loaded (prev |> List.rev |> List.tryHead)
    future = Loaded future
    meetups = Some meetups }
  
let fetchFail ex = {
  next = Error (ex.ToString())
  previous = Error "nope"
  future = Error "error"
  meetups = None
  }

let update msg model =
  match msg with
  | Fetch          -> (model, Cmd.ofPromise fetchMeetups () id FetchFail)
  | FetchResult ms -> (updateModel ms, Cmd.none)
  | FetchFail ex   -> (fetchFail ex, Cmd.none)

open Elmish.React
open Elmish.Debug

// App
Program.mkProgram init update root
|> Program.withReact "elmish-app"
//-:cnd
#if DEBUG
|> Program.withDebugger
#endif
//+:cnd
|> Program.run
