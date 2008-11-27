(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestMongoDoc =
struct
    open QCheck infix ==>

    (* generators *)
    (* TODO parameterize genString (for length)? *)
    val genString = Gen.choose #[Gen.lift "test",
                                 Gen.string (Gen.range (0, 20), Gen.charRange (#"a", #"z"))]
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
    fun equalToSelf document = MongoDoc.equal document document
    fun notBothEmpty (x, y) = Bool.not (MongoDoc.isEmpty x)
                              orelse Bool.not (MongoDoc.isEmpty y)
    fun notEqual (document1, document2) = Bool.not (MongoDoc.equal document1 document2)
    (* NOTE this isn't ALWAYS true. but things are random enough that it should be, unless both documents are empty. *)
    val notEqualToRandom = notBothEmpty ==> notEqual
    fun toThenFromList document = MongoDoc.equal (MongoDoc.fromList (MongoDoc.toList document)) document
    fun contains list (elem:string) =
        case list of
            hd::tl => if hd = elem then true else contains tl elem
          | nil => false
    fun noDupsInList toCheck seen = case toCheck of
                                        nil => true
                                      | (key, value)::tl => ((case value of
                                                                  MongoDoc.Document d => noDups d
                                                                | _ => true)
                                                             andalso Bool.not (contains seen key))
                                                            andalso noDupsInList tl (key::seen)
    and noDups document = noDupsInList (MongoDoc.toList document) nil

    (* document test specs *)
    val doc = (genDoc 5, SOME MongoDoc.toString)
    val docPair = (Gen.zip (genDoc 5, genDoc 5), SOME (fn (x,y) => MongoDoc.toString x ^ ", " ^ MongoDoc.toString y))

    (* run the tests *)
    val _ = checkGen doc ("a document is equal to itself", pred equalToSelf)
    val _ = checkGen docPair ("two random documents are not equal", notEqualToRandom)
    val _ = checkGen doc ("fromList o toList == identity", pred toThenFromList)
    val _ = checkGen doc ("toList contains no duplicates", pred noDups)
end


