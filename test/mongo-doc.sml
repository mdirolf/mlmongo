(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestMongoDoc =
struct
    open QCheck
    val genString = Gen.string (Gen.range (0, 40), Gen.char)
    val genFlatValue = Gen.choose #[Gen.map MongoDoc.Bool Gen.flip,
                                    Gen.map MongoDoc.Int Gen.Int.int,
                                    Gen.map MongoDoc.Float Gen.Real.real,
                                    Gen.map MongoDoc.String genString
                                  ]
(*    fun genDocAsList 0 = genFlatDoc
      | genDocAsList n =
    val genDoc *)
end
