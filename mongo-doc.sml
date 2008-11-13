(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
signature MONGO_DOC =
sig
    datatype value =
             Document of (string * value) list
           | Array of value list
           | Bool of bool
           | Int of int
           | Float of real
           | String of string
    type document
    val valueForKey: document -> string -> value option
    val fromList: (string * value) list -> document
    val toList: document -> (string * value) list
    val print: document -> unit
end

structure MongoDoc :> MONGO_DOC =
struct
    datatype value =
             Document of (string * value) list
           | Array of value list
           | Bool of bool
           | Int of int
           | Float of real
           | String of string
    type document = (string * value) list
    exception UnimplementedError
    fun valueForKey (document: document) key =
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
    fun fromList l = l
    fun toList d = d
    fun print document = (print "hello world")
end
