(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestMongoDoc =
struct
    open QCheck infix ==>

    (* generators *)
    (* TODO parameterize genString (for length)? *)
    val genString = Gen.string (Gen.range (1, 20), Gen.charRange (#"a", #"z"))
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

    (* test cases *)
    fun closeToSelf document = MongoDoc.close document document
    fun notBothEmpty (x, y) = Bool.not (MongoDoc.isEmpty x)
                              orelse Bool.not (MongoDoc.isEmpty y)
    fun notClose (document1, document2) = Bool.not (MongoDoc.close document1 document2)
    (* NOTE this isn't ALWAYS true. but things are random enough that it should be, unless both documents are empty. *)
    val notCloseToRandom = notBothEmpty ==> notClose
    fun toThenFromList document = MongoDoc.close (MongoDoc.fromList (MongoDoc.toList document)) document

    (* document test specs *)
    val doc = (genDoc 5, SOME MongoDoc.toString)
    val docPair = (Gen.zip (genDoc 5, genDoc 5), SOME (fn (x,y) => MongoDoc.toString x ^ ", " ^ MongoDoc.toString y))

    (* run the tests *)
    val _ = checkGen doc ("a document is close to itself", pred closeToSelf)
    val _ = checkGen docPair ("two random documents are not close", notCloseToRandom)
    val _ = checkGen doc ("toList then fromList == identity", pred toThenFromList)
end


