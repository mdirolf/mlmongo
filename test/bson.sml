(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestBSON =
struct
    open QCheck infix ==>

    (* TODO actually test these predicates *)
    fun toThenFromDocument bson = BSON.fromDocument (BSON.toDocument bson) = bson
    fun fromThenToDocument document = MongoDoc.equal (BSON.toDocument (BSON.fromDocument document)) document

    fun showDocumentHex (document, hex) = (MongoDoc.toString document) ^ " : '" ^ hex ^ "'"
    val documentHexList = (List.getItem, SOME showDocumentHex)
    fun documentEqualsHex (document, hex) = BSON.toString (BSON.fromDocument document) = hex
    val tests = [(MongoDoc.fromList [], "   0:  05 00 00 00 00\n")]
    val _ = check documentHexList ("documents are converted to BSON correctly", pred documentEqualsHex) tests
end
