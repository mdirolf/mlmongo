(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
structure TestMongoDoc =
struct
    open QCheck infix ==>

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
    fun noDupsHelper toCheck seen = case toCheck of
                                        nil => true
                                      | (key, value)::tl => ((case value of
                                                                  MongoDoc.Document d => noDups d
                                                                | _ => true)
                                                             andalso Bool.not (contains seen key))
                                                            andalso noDupsHelper tl (key::seen)
    and noDups document = noDupsHelper (MongoDoc.toList document) nil
    fun listNoDups list = noDupsHelper list nil
    fun notAlreadyThere (document, (key, value)) = Bool.not (MongoDoc.hasKey document key)
    fun setThenRemove (document, (key, value)) =
        let
            val document' = MongoDoc.removeKey (MongoDoc.setBinding document (key, value)) key
        in
            MongoDoc.equal document document'
        end
    fun setThenCheck (document, (key, value)) =
        let
            val document' = MongoDoc.setBinding document (key, value)
            val value' = Option.valOf (MongoDoc.valueForKey document' key)
        in
            MongoDoc.valueEqual value value'
        end
    fun removeThenCheck (document, key) =
        let
            val document' = MongoDoc.removeKey document key
        in
            Bool.not (MongoDoc.hasKey document' key)
        end
    fun equalList nil nil  = true
      | equalList _ nil = false
      | equalList nil _ = false
      | equalList ((key1: string, value1)::tail1) ((key2, value2)::tail2) = key1 = key2
                                                                            andalso MongoDoc.valueEqual value1 value2
                                                                            andalso equalList tail1 tail2
    fun fromThenToList list = equalList (MongoDoc.toList (MongoDoc.fromList list)) list

    (* run the tests *)
    val _ = checkGen TestUtils.document ("a document is equal to itself", pred equalToSelf)
    val _ = checkGen TestUtils.documentPair ("two random documents are not equal", notEqualToRandom)
    val _ = checkGen TestUtils.document ("fromList o toList == identity", pred toThenFromList)
    val _ = checkGen TestUtils.document ("toList contains no duplicates", pred noDups)
    val _ = checkGen TestUtils.documentAndBinding ("removeKey o setBinding == identity", notAlreadyThere ==> setThenRemove)
    val _ = checkGen TestUtils.documentAndBinding ("setBinding then get value", pred setThenCheck)
    val _ = checkGen TestUtils.documentAndKey ("removeKey then check it's gone", trivial (fn (x, y) => Bool.not (MongoDoc.hasKey x y)) (pred removeThenCheck))
    val _ = checkGen TestUtils.keyValueList ("toList o fromList == identity (if no dups)", listNoDups ==> fromThenToList)
end


