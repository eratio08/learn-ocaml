open Base
module Time_ns = Core.Time_ns

(* inline records *)
type details =
  | Logon of
      { user : string
      ; credentials : string
      }
  | Heartbeat of { status_message : string }
  | Log_entry of
      { important : bool
      ; message : string
      }

module Common = struct
  type t =
    { session_id : string
    ; time : Time_ns.t
    }
end

let messages_for_user user (messages : (Common.t * details) list) =
  let user_messages, _ =
    List.fold
      messages
      ~init:([], Set.empty (module String))
      ~f:(fun ((messages, user_sessions) as acc) ((common, details) as message) ->
        match details with
        | Logon m ->
          if String.( = ) m.user user
          then message :: messages, Set.add user_sessions common.session_id
          else acc
        | Heartbeat _ | Log_entry _ ->
          if Set.mem user_sessions common.session_id
          then message :: messages, user_sessions
          else acc)
  in
  List.rev user_messages
;;
