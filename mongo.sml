(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
signature MONGO =
sig
    datatype mongo_value =
        Document of (string * mongo_value) list
      | Array of mongo_value list
      | Bool of bool
      | Int of IntInf.int
      | Float of real
      | String of string
    type mongo_document = (string * mongo_value) list
    type connection
    exception ConnectError of string
    val connect: string -> int -> int -> connection
    val getValue: mongo_document -> string -> mongo_value option
end;

structure Mongo :> MONGO =
struct
    datatype mongo_value =
        Document of (string * mongo_value) list
      | Array of mongo_value list
      | Bool of bool
      | Int of IntInf.int
      | Float of real
      | String of string
    type mongo_document = (string * mongo_value) list
    type connection = Socket.active INetSock.stream_sock
    exception ConnectError of string
    fun connect remote_host remote_port local_port =
        let
            val host_address = case NetHostDB.getByName remote_host of
                                   NONE => raise ConnectError ("No net host db entry found for name '" ^ remote_host ^ "'.")
                                 | SOME en => NetHostDB.addr en
            val remote_address = INetSock.toAddr (host_address, remote_port)
            val local_address = INetSock.any local_port
            val socket = INetSock.TCP.socket ();
        in
            Socket.bind (socket, local_address) handle SysErr => raise ConnectError ("Could not bind socket to port " ^ Int.toString local_port ^ ".");
            Socket.connect (socket, remote_address) handle SysErr => raise ConnectError ("Could not connect to mongo database at '" ^ remote_host ^ ":" ^ Int.toString remote_port ^ ".");
            socket
        end
    fun getValue (document: mongo_document) key =
        let
            val value = List.find (fn (s, _) => s = key) document
        in
            if Option.isSome value then
                let
                    val (_, result) = Option.valOf value
                in
                    SOME(result)
                end
            else
                NONE
        end
end;
