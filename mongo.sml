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
    val toBSON: mongo_document -> Word8Vector.vector
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
    exception InternalError
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
    fun dedup document =
        let
            fun contains list (elem:string) =
                case list of
                    hd::tl => if hd = elem then true else contains tl elem
                  | nil => false
            fun dedup_helper (document:mongo_document) seen =
                case document of
                    hd::tl => if contains seen (#1 hd) then dedup_helper tl seen else hd::dedup_helper tl seen
                  | nil => nil
        in
            dedup_helper document nil
        end
    fun elementTypeFromName typeName =
        case typeName of
            "EOO" => Word8.fromInt 0
          | "NUMBER" => Word8.fromInt 1
          | "STRING" => Word8.fromInt 2
          | "OBJECT" => Word8.fromInt 3
          | "ARRAY" => Word8.fromInt 4
          | "BINARY" => Word8.fromInt 5
          | "UNDEFINED" => Word8.fromInt 6
          | "OID" => Word8.fromInt 7
          | "BOOLEAN" => Word8.fromInt 8
          | "DATE" => Word8.fromInt 9
          | "NULL" => Word8.fromInt 10
          | "REGEX" => Word8.fromInt 11
          | "REF" => Word8.fromInt 12
          | "CODE" => Word8.fromInt 13
          | _ => raise InternalError
    fun elementType element =
        case element of
            Document _ => elementTypeFromName "OBJECT"
          | Array _ => elementTypeFromName "ARRAY"
          | Bool _ => elementTypeFromName "BOOLEAN"
          | Int _ => elementTypeFromName "NUMBER"
          | Float _ => elementTypeFromName "NUMBER"
          | String _ => elementTypeFromName "STRING"
    fun toCString s =
        let
            val s' = List.map (Word8.fromInt o ord) (explode s)
        in
            List.concat [s', [Word8.fromInt 0]]
        end
    fun elementToBSON (name, element) =
        let
            val tp = elementType element
            val name = toCString name
        in
            [Word8.fromInt 5]
        end
    fun makeList count element =
        if count = 0 then
            nil
        else
            element::(makeList (count - 1) element)
    fun padLeft list count padding =
        let
            val len = length list
        in
            if len >= count then
                List.take(list, count)
            else
                (makeList (count - len) padding) @ list
        end
    fun intToWord8List i =
        let
            fun helper i l =
                if i = 0 then
                    l
                else
                    let
                        val w = Word8.fromInt (IntInf.toInt i)
                        val rem = IntInf.~>> (i, Word.fromInt 8)
                    in
                        helper rem (w::l)
                    end
            val l = helper (Int.toLarge i) nil
        in
            padLeft l 4 (Word8.fromInt 0)
        end
    val eoo = Word8.fromInt 0
    fun toBSON document =
        let
            val document' = dedup document
            val objectData = List.concat(List.map elementToBSON document')
            (* overhead for the size bytes and eoo.
             * TODO should this include the eoo byte or not? *)
            val overhead = 5
            val size = intToWord8List (length objectData + overhead)
        in
            Word8Vector.fromList (List.concat [size, objectData, [eoo]])
        end
end;
