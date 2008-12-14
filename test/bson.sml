(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestBSON =
struct
    open QCheck infix ==>

    (* TODO actually test these predicates *)
    fun toThenFromDocument bson = BSON.fromDocument (BSON.toDocument bson) = bson
    fun fromThenToDocument document = MongoDoc.equal (BSON.toDocument (BSON.fromDocument document)) document
end
