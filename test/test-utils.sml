(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)

(**
 * Utilities used for testing.
 *
 * Mostly test specifications for QCheck.
 *)
signature TEST_UTILS =
sig
    type 'a spec = 'a QCheck.Gen.gen * 'a QCheck.rep
    val document: MongoDoc.document spec
    val repDocumentPair: (MongoDoc.document * MongoDoc.document) QCheck.rep
    val documentPair: (MongoDoc.document * MongoDoc.document) spec
    val documentAndBinding: (MongoDoc.document * (string * MongoDoc.value)) spec
    val documentAndKey: (MongoDoc.document * string) spec
    val keyValueList: ((string * MongoDoc.value) list) spec
    val word8Vector: Word8Vector.vector spec
end
structure TestUtils :> TEST_UTILS =
struct
    open QCheck infix ==>
    val _ = Settings.set (Settings.column_width, 45) (* Set the QCheck column width *)
    type 'a spec = 'a QCheck.Gen.gen * 'a QCheck.rep
    val genString = Gen.choose #[Gen.lift "test",
                                 Gen.string (Gen.range (0, 20), Gen.charRange (#"a", #"z"))]
    val genFlatValue = Gen.choose #[Gen.map MongoDoc.Bool Gen.flip,
                                    Gen.map MongoDoc.Int Gen.Int.int,
                                    Gen.map MongoDoc.Float Gen.Real.real,
                                    Gen.map MongoDoc.String genString
                                  ]
    fun genValue 0 = genFlatValue
      | genValue n = Gen.choose' #[(4, genFlatValue),
                                   (1, Gen.map MongoDoc.Array (Gen.list Gen.flip (genValue (n - 1)))),
                                   (1, Gen.map MongoDoc.Document (genDocument (n - 1)))]
    and genKeyValueList n = Gen.list Gen.flip (Gen.zip (genString, genValue n))
    and genDocument n = Gen.map MongoDoc.fromList (genKeyValueList n)
    val genWord8Vector = Gen.vector Word8Vector.tabulate (Gen.range (0, 100), Gen.Word8.word)
    val word8VectorToString = Word8Vector.foldl (fn (x, y) => y ^ (Word8.toString x) ^ ", ") ""
    val document = (genDocument 5, SOME MongoDoc.toString)
    val repDocumentPair = SOME (fn (x,y) => MongoDoc.toString x ^ ", " ^ MongoDoc.toString y)
    val documentPair = (Gen.zip (genDocument 5, genDocument 5), repDocumentPair)
    val documentAndBinding = (Gen.zip (genDocument 5, Gen.zip (genString, genValue 5)), SOME (fn (x, (y: string, z: MongoDoc.value)) => MongoDoc.toString x ^ ", " ^ y))
    val documentAndKey = (Gen.zip (genDocument 5, genString), SOME (fn (x, y) => MongoDoc.toString x ^ ", " ^ y))
    (* TODO actually print the list, instead of converting it to a document first (which removes duplicates) *)
    val keyValueList = (genKeyValueList 5, SOME (MongoDoc.toString o MongoDoc.fromList))
    val word8Vector = (genWord8Vector, SOME word8VectorToString)
end
