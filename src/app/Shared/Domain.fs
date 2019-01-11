namespace TimeOff

open System

// First, we define our domain
type UserId = int

type IDateProvider =
    abstract member getCurrentDate: unit -> DateTime
    abstract member createDate: int -> int -> int -> int -> int -> int -> DateTime

type User =
    | Employee of UserId
    | Manager
    
[<CLIMutable>]
type UserIdAndYear = {
    UserId: UserId
    RequestYear: int
}

[<CLIMutable>]
type EmployeeBalance = {
    UserId: UserId
    AccumulatedDays: float
    AlreadyTakenDays: float
    PlannedDays: float
    CarriedOverDays: float
    RemainingDays: float
}

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}

[<CLIMutable>]
type TimeOff = {
    RequestId: Guid
    Start: Boundary
    End: Boundary
    Days: float
}

[<CLIMutable>]
type TimeOffRequestHistory = {
    UserId: UserId
    TimeOffList: seq<TimeOff>
}

[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
}
