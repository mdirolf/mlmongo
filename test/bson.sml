(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestBSON =
struct
    open QCheck infix ==>

    (* TODO actually test this predicate *)
    fun toThenFromDocument bson = BSON.fromDocument (BSON.toDocument bson) = bson

    fun fromThenToDocument document = MongoDoc.equal (BSON.toDocument (BSON.fromDocument document)) document
    val _ = checkGen TestUtils.document ("toDocument o fromDocument = identity", pred fromThenToDocument)

    fun fromBytesRaises vector = (let
                                      val _ = BSON.fromBytes vector
                                  in
                                      false
                                  end) handle BSON.InvalidBSON => true
    val _ = checkGen TestUtils.word8Vector ("random bytes cannot be converted to BSON", pred fromBytesRaises)

    fun showDocumentHex (document, hex) = (MongoDoc.toString document) ^ " : '" ^ hex ^ "'"
    val documentHexList = (List.getItem, SOME showDocumentHex)
    fun documentEqualsHex (document, hex) = BSON.toString (BSON.fromDocument document) = hex
    val doc = MongoDoc.fromList [("int", MongoDoc.Int 105),
                                 ("array", MongoDoc.Array [MongoDoc.String "test string",
                                                           MongoDoc.Int ~1592003,
                                                           MongoDoc.Float 1.5324E~157,
                                                           MongoDoc.Bool false]),
                                 ("string", MongoDoc.String "what?")]
    val doc' = MongoDoc.setBinding doc ("doc", MongoDoc.Document doc)
    val doc'' = MongoDoc.setBinding doc' ("another document", MongoDoc.Document doc')
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
                  "   0:  0D 00 00 00 08 66 61 6C\n   8:  73 65 00 00 00\n"),
                 (MongoDoc.fromList [("empty", MongoDoc.Array [])],
                  "   0:  11 00 00 00 04 65 6D 70\n   8:  74 79 00 05 00 00 00 00\n  16:  00\n"),
                 (MongoDoc.fromList [("full", MongoDoc.Array [MongoDoc.Int 5, MongoDoc.String "huh", MongoDoc.Bool true])],
                  "   0:  26 00 00 00 04 66 75 6C\n   8:  6C 00 1B 00 00 00 10 30\n  16:  00 05 00 00 00 02 31 00\n  24:  04 00 00 00 68 75 68 00\n  32:  08 32 00 01 00 00\n"),
                 (MongoDoc.fromList [("none", MongoDoc.Document (MongoDoc.fromList []))],
                  "   0:  10 00 00 00 03 6E 6F 6E\n   8:  65 00 05 00 00 00 00 00\n"),
                 (MongoDoc.fromList [("some", MongoDoc.Document (MongoDoc.fromList [("full", MongoDoc.Array [MongoDoc.Int 5, MongoDoc.String "huh", MongoDoc.Bool true])]))],
                  "   0:  31 00 00 00 03 73 6F 6D\n   8:  65 00 26 00 00 00 04 66\n  16:  75 6C 6C 00 1B 00 00 00\n  24:  10 30 00 05 00 00 00 02\n  32:  31 00 04 00 00 00 68 75\n  40:  68 00 08 32 00 01 00 00\n  48:  00\n"),
                 (doc'',
                  "   0:  70 01 00 00 10 69 6E 74\n   8:  00 69 00 00 00 04 61 72\n  16:  72 61 79 00 2E 00 00 00\n  24:  02 30 00 0C 00 00 00 74\n  32:  65 73 74 20 73 74 72 69\n  40:  6E 67 00 10 31 00 3D B5\n  48:  E7 FF 01 32 00 E5 74 07\n  56:  AE D5 D4 60 1F 08 33 00\n  64:  00 00 02 73 74 72 69 6E\n  72:  67 00 06 00 00 00 77 68\n  80:  61 74 3F 00 03 64 6F 63\n  88:  00 55 00 00 00 10 69 6E\n  96:  74 00 69 00 00 00 04 61\n 104:  72 72 61 79 00 2E 00 00\n 112:  00 02 30 00 0C 00 00 00\n 120:  74 65 73 74 20 73 74 72\n 128:  69 6E 67 00 10 31 00 3D\n 136:  B5 E7 FF 01 32 00 E5 74\n 144:  07 AE D5 D4 60 1F 08 33\n 152:  00 00 00 02 73 74 72 69\n 160:  6E 67 00 06 00 00 00 77\n 168:  68 61 74 3F 00 00 03 61\n 176:  6E 6F 74 68 65 72 20 64\n 184:  6F 63 75 6D 65 6E 74 00\n 192:  AF 00 00 00 10 69 6E 74\n 200:  00 69 00 00 00 04 61 72\n 208:  72 61 79 00 2E 00 00 00\n 216:  02 30 00 0C 00 00 00 74\n 224:  65 73 74 20 73 74 72 69\n 232:  6E 67 00 10 31 00 3D B5\n 240:  E7 FF 01 32 00 E5 74 07\n 248:  AE D5 D4 60 1F 08 33 00\n 256:  00 00 02 73 74 72 69 6E\n 264:  67 00 06 00 00 00 77 68\n 272:  61 74 3F 00 03 64 6F 63\n 280:  00 55 00 00 00 10 69 6E\n 288:  74 00 69 00 00 00 04 61\n 296:  72 72 61 79 00 2E 00 00\n 304:  00 02 30 00 0C 00 00 00\n 312:  74 65 73 74 20 73 74 72\n 320:  69 6E 67 00 10 31 00 3D\n 328:  B5 E7 FF 01 32 00 E5 74\n 336:  07 AE D5 D4 60 1F 08 33\n 344:  00 00 00 02 73 74 72 69\n 352:  6E 67 00 06 00 00 00 77\n 360:  68 61 74 3F 00 00 00 00\n")]
    val _ = check documentHexList ("documents are converted to BSON correctly", pred documentEqualsHex) tests
end
