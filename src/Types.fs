module App.Types

open System

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

