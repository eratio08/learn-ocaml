open Base
module Time_ns = Core.Time_ns

module Log_entry = struct
  type t =
    { session_id : string
    ; time : Time_ns.t
    ; important : bool
    ; message : string
    }
end

module Heartbeat = struct
  type t =
    { session_id : string
    ; time : Time_ns.t
    ; status_message : string
    }
end

module Logon = struct
  type t =
    { session_id : string
    ; time : Time_ns.t
    ; user : string
    ; credentials : string
    }
end

type client_message =
  | Logon of Logon.t
  | Heartbeat of Heartbeat.t
  | Log_entry of Log_entry.t

let messages_for_user user messages =
  let user_messages, _ =
    List.fold
      messages
      ~init:([], Set.empty (module String))
      ~f:(fun ((messages, user_sessions) as acc) message ->
        match message with
        | Logon m ->
          if String.(m.user = user)
          then message :: messages, Set.add user_sessions m.session_id
          else acc
        | Heartbeat _ | Log_entry _ ->
          let session_id =
            match message with
            | Logon m -> m.session_id
            | Heartbeat m -> m.session_id
            | Log_entry m -> m.session_id
          in
          if Set.mem user_sessions session_id
          then message :: messages, user_sessions
          else acc)
  in
  List.rev user_messages
;;
