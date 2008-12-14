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
                  "   0:  14 00 00 00 10 74 65 73\n   8:  74 41 6E 49 6E 74 00 80\n  16:  C1 FF FF 00\n"),
                 (MongoDoc.fromList [("hello", MongoDoc.Float 1.5)],
                  "   0:  14 00 00 00 01 68 65 6C\n   8:  6C 6F 00 00 00 00 00 00\n  16:  00 F8 3F 00\n"),
                 (MongoDoc.fromList [("c", MongoDoc.Float ~0.00000005)],
                  "   0:  10 00 00 00 01 63 00 48\n   8:  AF BC 9A F2 D7 6A BE 00\n"),
                 (MongoDoc.fromList [("true", MongoDoc.Bool true)],
                  "   0:  0C 00 00 00 08 74 72 75\n   8:  65 00 01 00\n"),
                 (MongoDoc.fromList [("false", MongoDoc.Bool false)],
                  "   0:  0D 00 00 00 08 66 61 6C\n   8:  73 65 00 00 00\n")]
    val _ = check documentHexList ("documents are converted to BSON correctly", pred documentEqualsHex) tests
end
