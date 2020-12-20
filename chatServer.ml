open Lwt.Infix
(* #require "lwt.unix";; *)
(* #require "lwt";; *)

(* a list associating user nicknames to the output channels that write to their connections *)
(* Once we fix the other functions this should change to []*)
(* let sessions = ref [("",Lwt_io.null)] *)
let sessions = ref []
exception Quit

let sm = (Lwt_mutex.create())
(* replace Lwt.return below with code that uses Lwt_list.iter_p to
  print sender: msg on each output channel (excluding the sender's)*)
let rec send_all sender msg =
  Lwt_list.iter_p (fun(person, output) -> match person with
                                          | person' -> if person' = sender then Lwt.return ()
                                          else Lwt_io.write_line output (sender^": "^msg)) (!sessions)

(* remove a session from the list of sessions: important so we don't try to
   write to a closed connection *)
let remove_session nn =
  sessions := List.remove_assoc nn !sessions;
  send_all nn "<left chat>" >>= fun () ->
  Lwt.return ()

(* Modify to handle the "Quit" case separately, closing the channels before removing the session *)
let handle_error e nn inp outp = remove_session nn

(* modify sessions to remove (nn,outp) association, add (new_nn,outp) association.
   also notify other participants of new nickname *)
let change_nn nn outp new_nn = send_all !nn ("<changed nick to "^new_nn^">") >>=
  fun () -> Lwt_mutex.lock sm >>= fun () -> sessions := List.remove_assoc !nn !sessions;
  nn := new_nn; sessions := (new_nn, outp)::(!sessions); Lwt_mutex.unlock sm; Lwt.return ()


(*  + obtain initial nick(name),
    + add (nick,outp) to !sessions, and
    + announce join to other users *)
let handle_login nr (inp,outp) =
  Lwt_io.write_line outp "Enter initial nick:" >>= fun () -> (Lwt_io.read_line inp) >>=
  fun s -> nr := s; Lwt_mutex.lock sm >>= fun () -> sessions := (s,outp)::(!sessions); Lwt_mutex.unlock sm;
  send_all s "<joined>"

(* modify handle_input below to detect /q, /n, and /l commands *)
let handle_input nr outp l =
  if String.length l < 2 then (send_all !nr l)
  else match (Str.string_before l 2) with
    | "/q" -> Lwt.fail Quit
    | "/n" -> change_nn nr outp (String.trim (Str.string_after l 2))
    | "/l" -> Lwt_list.iter_s (fun (n,c) -> Lwt_io.write_line outp n) !sessions
    | _ -> send_all !nr l

let chat_server _ (inp,outp) =
  let nick = ref "" in
  (* replace () below with call to handle_login *)
  let _ = handle_login nick (inp,outp) in (* check this*)
  let rec main_loop () =
	  Lwt_io.read_line inp >>= handle_input nick outp >>= main_loop in
  Lwt.catch main_loop (fun e -> handle_error e !nick inp outp)
