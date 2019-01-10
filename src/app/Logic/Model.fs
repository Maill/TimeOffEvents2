namespace TimeOff

open System

type DateProvider() =
    interface IDateProvider with
        member this.getCurrentDate () =
            DateTime.Today
        member this.createDate year month day hour minute second =
            DateTime(year, month, day, hour, minute, second)

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | RejectCancellation of UserId * Guid
    | CancelRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    | ValidateRequest of UserId * Guid with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | RejectCancellation (userId, _) -> userId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId
        | ValidateRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestCancellationRejected of TimeOffRequest
    | RequestPendingCancellation of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | RequestValidated of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestRefused request -> request
        | RequestCancelled request -> request
        | RequestValidated request -> request
        | RequestPendingCancellation request -> request
        | RequestCancellationRejected request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | PendingCancellation of TimeOffRequest
        | Cancelled of TimeOffRequest
        | Refused of TimeOffRequest
        | Validated of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | PendingCancellation request
            | Refused request 
            | Cancelled request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ -> true
            | PendingCancellation _ -> true
            | Refused _ -> false
            | Cancelled _ -> false
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestRefused request -> Refused request
        | RequestCancelled request -> Cancelled request
        | RequestValidated request -> Validated request
        | RequestPendingCancellation request -> PendingCancellation request
        | RequestCancellationRejected request -> Validated request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let isContainedIn baseRequest comparedRequest = 
        (baseRequest.Start < comparedRequest.End && comparedRequest.Start < baseRequest.Start)
        || (comparedRequest.Start < baseRequest.End && baseRequest.Start < comparedRequest.Start)

    let overlapsWith request1 request2 =
        request1.Start = request2.Start 
        || request1.End = request2.End
        || request1.End = request2.Start
        || request1.Start = request2.End
        || (request1.Start < request2.Start && request1.End >= request2.Start)
        || (request2.Start < request1.Start && request2.End >= request1.Start)
    
    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        otherRequests
        |> Seq.exists (overlapsWith request)
    
    (****************************************************************************************************)
    
    let getDaysSequenceWithoutWeekends (startDate: DateTime) (endDate: DateTime) =
        Seq.initInfinite float
        |> Seq.map (fun index -> startDate.AddDays index)
        |> Seq.where (fun dateTime -> int(dateTime.DayOfWeek) < 6)
        |> Seq.takeWhile (fun dateTime -> dateTime <= endDate)
        |> Seq.length
                        
    (****************************************************************************************************)
    
    let getTakenDaysForProvidedYear (userRequests: TimeOffRequest seq) providedYear =
        //TODO : IMPLEMENT FUNCTIONNALITY TO IGNORE WEEKENDS FROM TIME OFF REQUESTS
        let dateProvider = DateProvider() :> IDateProvider
        userRequests
        |> Seq.map (fun request ->
            if request.Start.Date.Year = providedYear then
                if request.End.Date.Year = providedYear then
                    let startDate = request.Start.Date
                    let endDate = request.End.Date
                    let result = getDaysSequenceWithoutWeekends startDate endDate
                    
                    printfn "%d" result
                    
                    let valueToReturn: float =
                        match request.Start.HalfDay with
                        | AM ->
                            match request.End.HalfDay with
                            | AM -> float(result) + 0.5
                            | PM -> float(result) + 1.0
                        | PM ->
                            match request.End.HalfDay with
                            | AM -> float(result)
                            | PM -> float(result) + 0.5
                    valueToReturn             
                else
                    let startDate = request.Start.Date
                    let result = 365 - startDate.DayOfYear
                    let valueToReturn: float =
                        match request.Start.HalfDay with
                        | AM -> float(result) + 1.0
                        | PM -> float(result) + 0.5
                    valueToReturn
            else
                let currentDate = dateProvider.getCurrentDate()
                let startDate = dateProvider.createDate currentDate.Year 1 1 0 0 0
                let endDate = dateProvider.createDate currentDate.Year 12 31 0 0 0
                let result = getDaysSequenceWithoutWeekends startDate endDate
                float(result)
            )
        |> Seq.sum
    
    (****************************************************************************************************)
    
    let getCurrentYearEarnedBalance: float =
        let dateProvider = DateProvider() :> IDateProvider
        let currentDate = dateProvider.getCurrentDate()
        float(currentDate.Month - 1) * 2.5
    
    (****************************************************************************************************)

    let createRequest activeUserRequests  request =
        let dateProvider = DateProvider() :> IDateProvider
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= dateProvider.getCurrentDate() then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]
        
    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let dateProvider = DateProvider() :> IDateProvider
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.Request.Start.Date.Year = dateProvider.getCurrentDate().Year || state.Request.End.Date.Year = dateProvider.getCurrentDate().Year)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
 
                let currentDate = dateProvider.getCurrentDate()
                let earnedDays = (float(currentDate.Month) * 2.5) - 2.5
                
                
                let getAmountOfDaysAlreadyTaken =
                    activeUserRequests
                    |> Seq.map (fun request ->
                        if request.Start.Date.Year = currentDate.Year then
                            if request.End.Date.Year = currentDate.Year then
                                let startDate = request.Start.Date
                                let endDate = request.End.Date
                                let result = endDate.Subtract(startDate);
                                let a: float =
                                    match request.Start.HalfDay with
                                    | AM ->
                                        match request.End.HalfDay with
                                        | AM -> float(result.Days) + 0.5
                                        | PM -> float(result.Days) + 1.0
                                    | PM ->
                                        match request.End.HalfDay with
                                        | AM -> float(result.Days)
                                        | PM -> float(result.Days) + 0.5
                                a
                                    
                                
                            else
                                let startDay =
                                    match request.Start.HalfDay with
                                    | AM -> (request.Start.Date.DayOfYear * 2) - 1
                                    | PM -> (request.Start.Date.DayOfYear * 2)
                                float(730 - startDay)
                        else
                            730.0
                        )
                    |> Seq.sum
                
                let currentRequestDays =
                    let a = request.End.Date.Subtract(request.Start.Date).Days
                    match request.Start.HalfDay with
                    | AM ->
                        match request.End.HalfDay with
                        | AM -> float(a) + 0.5
                        | PM -> float(a) + 1.0
                    | PM ->
                        match request.End.HalfDay with
                        | AM -> float(a)
                        | PM -> float(a) + 0.5 
                
                printfn "%f" earnedDays
                printfn "%f" getAmountOfDaysAlreadyTaken
                printfn "%f" currentRequestDays
                
                if earnedDays - getAmountOfDaysAlreadyTaken >= currentRequestDays then
                    createRequest activeUserRequests request
                else
                    Error "Not enought balance of days off for this request"
             
            | RefuseRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                if user <> Manager then
                    Error "Unauthorized"
                else
                    match requestState with
                    | PendingValidation request -> Ok [RequestRefused request]
                    | _ ->
                        Error "Request cannot be refused"
                
            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                let dateProvider = DateProvider() :> IDateProvider
                if requestState.Request.Start.Date <= dateProvider.getCurrentDate() then
                    Error "Request date is in the past"
                else
                    match requestState with
                    | PendingValidation request -> Ok [RequestCancelled request]
                    | Validated request ->
                        if user <> Manager then
                            Ok [RequestPendingCancellation request]
                        else
                            Ok [RequestCancelled request]
                    | PendingCancellation request ->
                        if user <> Manager then
                            Error "Unauthorized"
                        else
                            Ok [RequestCancelled request]
                    | _ ->
                        Error "Request cannot be cancelled"

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    let dateProvider = DateProvider() :> IDateProvider
                    if requestState.Request.Start.Date <= dateProvider.getCurrentDate() then
                        Error "Request date is in the past"
                    else
                        validateRequest requestState
                    
            | RejectCancellation (_, requestId) ->
                if user <> Manager then
                    Error "You are unauthorized to reject a time off cancellation request"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    match requestState with
                    | PendingValidation request -> Error "This request is in state 'PendingValidation', the cancellation request cannot be rejected because request must be in state 'PendingCancellation' !"
                    | PendingCancellation request -> Ok [RequestValidated request]
                    | Cancelled request -> Error "This request is in state 'Cancelled', the cancellation request cannot be rejected because request must be in state 'PendingCancellation' !"
                    | Refused request -> Error "This request is in state 'Refused', the cancellation request cannot be rejected because request must be in state 'PendingCancellation' !"
                    | Validated request -> Error "This request is in state 'Validated', the cancellation request cannot be rejected because request must be in state 'PendingCancellation' !"
                    | _ -> Error "This request does not exist !"
