(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestUtils =
struct
    open QCheck infix ==>

    (* generators *)
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

    (* test specs *)
    val document = (genDocument 5, SOME MongoDoc.toString)
    val documentPair = (Gen.zip (genDocument 5, genDocument 5), SOME (fn (x,y) => MongoDoc.toString x ^ ", " ^ MongoDoc.toString y))
    val documentAndBinding = (Gen.zip (genDocument 5, Gen.zip (genString, genValue 5)), SOME (fn (x, (y: string, z: MongoDoc.value)) => MongoDoc.toString x ^ ", " ^ y))
    val documentAndKey = (Gen.zip (genDocument 5, genString), SOME (fn (x, y) => MongoDoc.toString x ^ ", " ^ y))
    (* TODO actually print the list, instead of converting it to a document first (which removes duplicates) *)
    val keyValueList = (genKeyValueList 5, SOME (MongoDoc.toString o MongoDoc.fromList))
end
