namespace TimeOff

open System

type DateProvider() =
    interface IDateProvider with
        member this.getCurrentDate () =
            DateTime.Today

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
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request
             
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
