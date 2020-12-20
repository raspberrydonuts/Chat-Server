(* This file adds timestamps to messages and color-codes different users' messages; *)
(* Requires the Unix library *)
(* #load "unix.cma";; *)
open Unix
open Lwt.Infix

(* a list associating user nicknames to the output channels that write to their connections *)
(* Once we fix the other functions this should change to []*)
(* let sessions = ref [("",Lwt_io.null)] *)
let sessions = ref []
exception Quit

(* returns decimal color codes for ocaml unix console *)
let get_color color = match (String.trim color) with
  | "red" -> "\027[31m"
  | "green" -> "\027[32m"
  | "yellow" -> "\027[33m"
  | "blue" -> "\027[36m"
  | _ -> "You did not follow the directions. Your computer will blow up. Rip"

(* a mutex *)
let sm = (Lwt_mutex.create())

(* pads time with a zero in front if it's a single digit integer *)
let expand n = if String.length n = 1 then "0"^n else n

(* Gets current time for a timestamp *)
let get_time bl = if bl = true then
  let t = Unix.localtime (Unix.time ()) in
  expand (string_of_int t.tm_hour)^":"^ expand (string_of_int t.tm_min)^":"^ expand (string_of_int t.tm_sec)
  else ""

(* replace Lwt.return below with code that uses Lwt_list.iter_p to
  print sender: msg on each output channel (excluding the sender's)*)
let rec send_all sender color msg =
  Lwt_list.iter_p (fun(person, output) -> match person with
    | person' -> if person' = sender then Lwt.return ()
    else Lwt_io.write_line output (color^" "^(get_time true)^" - "^sender^": "^msg)) (!sessions)

(* remove a session from the list of sessions: important so we don't try to
   write to a closed connection *)
let remove_session nn color =
  sessions := List.remove_assoc nn !sessions;
  send_all nn color "<left chat>" >>= fun () ->
  Lwt.return ()

(* Modify to handle the "Quit" case separately, closing the channels before removing the session *)
let handle_error e nn inp outp color = remove_session nn color

(* modify sessions to remove (nn,outp) association, add (new_nn,outp) association.
   also notify other participants of new nickname *)
let change_nn nn outp new_nn = send_all (fst !nn) (snd !nn) ("<changed nick to "^new_nn^">") >>=
  fun () -> Lwt_mutex.lock sm >>= fun () -> sessions := List.remove_assoc (fst !nn) !sessions;
  nn := (new_nn, (snd !nn)); sessions := (new_nn, outp)::(!sessions); Lwt_mutex.unlock sm; Lwt.return ()


(*  + obtain initial nick(name),
    + add (nick,outp) to !sessions, and
    + announce join to other users *)
let handle_login nr (inp,outp) =
  Lwt_io.write_line outp "Enter initial nick:" >>=
  fun () -> (Lwt_io.read_line inp) >>=
  fun s -> nr := (s, "\027[31m");
  Lwt_io.write_line outp "Enter a color (blue, green, red, or yellow).
  If you do not pick one of these colors, your computer will blow up: " >>=
  fun () -> (Lwt_io.read_line inp) >>=
  fun t -> let c = get_color t in nr := (s, c);
  Lwt_mutex.lock sm >>=
  fun () -> sessions := (s,outp)::(!sessions); Lwt_mutex.unlock sm;
  send_all s (snd !nr) "<joined>"

(* modify handle_input below to detect /q, /n, and /l commands *)
let handle_input nr outp l =
  if String.length l < 2 then (send_all (fst !nr) (snd !nr) l)
  else match (Str.string_before l 2) with
    | "/q" -> Lwt.fail Quit
    | "/n" -> change_nn nr outp (String.trim (Str.string_after l 2))
    | "/l" -> Lwt_list.iter_s (fun (n,c) -> Lwt_io.write_line outp n) !sessions
    | _ -> send_all (fst !nr) (snd !nr) l

let chat_server _ (inp,outp) =
  let nick = ref ("","") in
  (* replace () below with call to handle_login *)
  let t = handle_login nick (inp,outp) in (* check this*)
  let rec main_loop () =
	  Lwt_io.read_line inp >>= handle_input nick outp >>= main_loop in
  Lwt.catch (fun () -> t >>= main_loop) (fun e -> handle_error e (fst !nick) inp outp (snd !nick))
