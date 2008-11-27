(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestMongoDoc =
struct
    open QCheck
    (* TODO parameterize genString (for length)? *)
    val genString = Gen.string (Gen.range (0, 40), Gen.char)
    val genFlatValue = Gen.choose #[Gen.map MongoDoc.Bool Gen.flip,
                                    Gen.map MongoDoc.Int Gen.Int.int,
                                    Gen.map MongoDoc.Float Gen.Real.real,
                                    Gen.map MongoDoc.String genString
                                  ]
    fun genThickValue 0 = genFlatValue
      | genThickValue n = Gen.choose' #[(4, genFlatValue),
                                        (1, Gen.map MongoDoc.Array (Gen.list Gen.flip (genThickValue (n - 1)))),
                                        (1, Gen.map MongoDoc.Document (genDoc (n - 1)))]
    and genDocAsList n = Gen.list Gen.flip (Gen.zip (genString, genThickValue n))
    (* TODO what about generating documents that have repeats? is this even reasonable to do? *)
    and genDoc n = Gen.map MongoDoc.fromList (genDocAsList n)
end
