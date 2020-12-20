# Ocaml Chat Server

A _server_ that allows people to (text) chat with
each other over the internet.  The protocol is similar to
[Internet Relay Chat (IRC)](https://en.wikipedia.org/wiki/Internet_Relay_Chat)
with a little less functionality. I use the command-line program `nc` for a _client_ instead of
developing a fancier interface.  The file `chatServer.ml` contains a
skeleton for the server we'll develop, and the code to run `chatServer` in in `srvmain.ml`.

Run it using:
```
% ocamlfind ocamlc -package lwt -package lwt.unix -linkpkg -o server str.cma chatServer.ml srvmain.ml
% ./server
```

# Lwt
`Lwt` is a library for structuring programs to run in concurrent
threads

This project uses Lwt concurrency library to achieve concurrency.