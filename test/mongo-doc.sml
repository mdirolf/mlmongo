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
                                        (1, genThickValue (n - 1))]
    (* TODO why did I choose 5. it is completely random. do something smarter. *)
    val genDocAsList = Gen.list Gen.flip (Gen.zip (genString, genThickValue 5))
    (* TODO figure out how repeats are being handled here... *)
    val genDoc = Gen.map MongoDoc.fromList genDocAsList
end
