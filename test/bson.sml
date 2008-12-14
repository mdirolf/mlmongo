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
    val tests = [(MongoDoc.fromList [],
                  "   0:  05 00 00 00 00\n"),
                 (MongoDoc.fromList [("test", MongoDoc.String "hello world")],
                  "   0:  1B 00 00 00 02 74 65 73\n   8:  74 00 0C 00 00 00 68 65\n  16:  6C 6C 6F 20 77 6F 72 6C\n  24:  64 00 00\n"),
                 (MongoDoc.fromList [("mike", MongoDoc.Int 100)],
                  "   0:  0F 00 00 00 10 6D 69 6B\n   8:  65 00 64 00 00 00 00\n"),
                 (MongoDoc.fromList [("testAnInt", MongoDoc.Int ~16000)],
                  "   0:  14 00 00 00 10 74 65 73\n   8:  74 41 6E 49 6E 74 00 80\n  16:  C1 FF FF 00\n")]
    val _ = check documentHexList ("documents are converted to BSON correctly", pred documentEqualsHex) tests
end
