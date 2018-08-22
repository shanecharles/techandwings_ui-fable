module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Fable.PowerPack
open Fable.PowerPack.Fetch
open System
open Global
open Types

let pageParser: Parser<Page->Page,Page> =
  oneOf [
    map Home (s "home")
  ]

type Msg =
  | Fetch
  | FetchResult of Meetup list
  | FetchFail of exn

let init result =
  ({ next     = Loading
     previous = Loading
     future   = Loading
     meetups  = None
     }, Cmd.ofMsg Fetch)

let fetchUrl url = promise {
  let! meetups = fetchAs<Meetup[]> url []
  return meetups
  }

let fetchMeetups () =
  let dataUrl = "https://techandwingsfunctions.azurewebsites.net/api/meetups"
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

  { next     = Loaded next
    previous = Loaded (prev |> List.rev |> List.tryHead)
    future   = Loaded future
    meetups  = Some meetups }
  
let fetchFail ex = {
  next     = Error (ex.ToString())
  previous = Error "uh oh... someone talk with the dev!"
  future   = Error "Having troubles peering into the void at this time."
  meetups  = None 
  }
  
let update msg model =
  match msg with
  | Fetch          -> (model, Cmd.ofPromise fetchMeetups () id FetchFail)
  | FetchResult ms -> (updateModel ms, Cmd.none)
  | FetchFail ex   -> (fetchFail ex, Cmd.none)