(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
signature MONGO =
sig
    type connection
    exception ConnectError of string
    val connect: string -> int -> int -> connection
end;

structure Mongo :> MONGO =
struct
    (* TODO tests *)
    type connection = Socket.active INetSock.stream_sock
    exception ConnectError of string
    exception InternalError
    exception UnimplementedError
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
end;
