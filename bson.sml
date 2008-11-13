(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
signature BSON =
sig
    type value
    val print: value -> unit
    val fromDocument: MongoDoc.document -> value
    val toDocument: value -> MongoDoc.document
end
