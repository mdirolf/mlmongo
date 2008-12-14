(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)

(**
 * Utilities for dealing with the BSON data format.
 *)
signature BSON =
sig
    (**
     * A BSON "object".
     *)
    eqtype bson
    (*
     * Create a "hex dump" representation of a bson object.
     *
     * @param bson a bson document
     * @return a string representation of the document
     *)
    val toString: bson -> string
    (*
     * Convert a Mongo document to a bson object.
     *
     * @param document a Mongo document
     * @return a bson object corresponding to that document
     *)
    val fromDocument: MongoDoc.document -> bson
    (*
     * Convert a bson object to a Mongo document.
     *
     * @param bson a bson object
     * @return a Mongo document corresponding to that object
     *)
    val toDocument: bson -> MongoDoc.document
end

structure BSON :> BSON =
struct
    structure MD = MongoDoc

    type bson = Word8.word list
    exception InternalError
    exception UnimplementedError
    val zeroByte = Word8.fromInt 0
    fun makeList count element =
        if count = 0 then
            nil
        else
            element::(makeList (count - 1) element)
    fun padLeft list count padding =
        let
            val len = length list
        in
            if len >= count then
                List.take(list, count)
            else
                (makeList (count - len) padding) @ list
        end
    fun toString bson =
        let
            fun padStringLeft string count char =
                String.implode (padLeft (String.explode string) count char)
            fun printHelper lineNumber bson =
                case bson of
                    nil => "\n"
                  | hd::tl =>
                    let
                        val start =
                            if lineNumber mod 8 = 0 then
                                (if lineNumber <> 0 then "\n" else "") ^
                                ((padStringLeft (Int.toString lineNumber) 4 #" ") ^ ":  ")
                            else
                                " "
                    in
                        start ^
                        padStringLeft (Word8.toString hd) 2 #"0" ^
                        printHelper (lineNumber + 1) tl
                    end
        in
            printHelper 0 bson
        end
    fun elementTypeFromName typeName =
        case typeName of
            "EOO" => Word8.fromInt 0
          | "NUMBER" => Word8.fromInt 1
          | "STRING" => Word8.fromInt 2
          | "OBJECT" => Word8.fromInt 3
          | "ARRAY" => Word8.fromInt 4
          | "BINARY" => Word8.fromInt 5
          | "UNDEFINED" => Word8.fromInt 6
          | "OID" => Word8.fromInt 7
          | "BOOLEAN" => Word8.fromInt 8
          | "DATE" => Word8.fromInt 9
          | "NULL" => Word8.fromInt 10
          | "REGEX" => Word8.fromInt 11
          | "REF" => Word8.fromInt 12
          | "CODE" => Word8.fromInt 13
          | "SYMBOL" => Word8.fromInt 14
          | "CODE_W_SCOPE" => Word8.fromInt 15
          | "NUMBER_INT" => Word8.fromInt 16
          | _ => raise InternalError
    fun elementType element =
        case element of
            MD.Document _ => elementTypeFromName "OBJECT"
          | MD.Array _ => elementTypeFromName "ARRAY"
          | MD.Bool _ => elementTypeFromName "BOOLEAN"
          | MD.Int _ => elementTypeFromName "NUMBER_INT"
          | MD.Float _ => elementTypeFromName "NUMBER"
          | MD.String _ => elementTypeFromName "STRING"
    (* TODO this ought to be UTF-8 encoded *)
    fun toCString s =
        let
            val s' = List.map (Word8.fromInt o ord) (explode s)
        in
            List.concat [s', [zeroByte]]
        end
    fun intToWord8List int =
        let
            fun helper int count =
                if count = 0 then
                    nil
                else
                    if int = 0 then
                        zeroByte::(helper 0 (count - 1))
                    else
                        let
                            val word = Word8.fromInt (IntInf.toInt int)
                            val int' = IntInf.~>> (int, Word.fromInt 8)
                        in
                            word::(helper int' (count - 1))
                        end
        in
            helper (Int.toLarge int) 4
        end
    fun elementToBSON (name, element) =
        let
            val tp = elementType element
            val name = toCString name
            fun listAsArray list =
                let
                    fun helper l index =
                        case l of
                            nil => nil
                          | hd::tl => (Int.toString index, hd)::(helper tl (index + 1))
                in
                    helper list 0
                end
            fun toList vec = Word8Vector.foldr (op ::) [] vec
            val element = case element of
                              MD.Document d => fromDocument d
                            | MD.Array a => fromDocument (MD.fromList (listAsArray a))
                            | MD.Bool b => if b then [Word8.fromInt 1] else [zeroByte]
                            | MD.Int i => intToWord8List i
                            | MD.Float f => toList (PackRealLittle.toBytes f)
                            | MD.String s =>
                              let
                                  val cs = toCString s
                              in
                                  intToWord8List (length cs) @ cs
                              end
        in
            (tp::name) @ element
        end
    and fromDocument document =
        let
            val document' = MD.toList document
            val objectData = List.concat(List.map elementToBSON document')
            (* overhead for the size bytes and eoo *)
            val overhead = 5
            val size = intToWord8List (length objectData + overhead)
        in
            List.concat [size, objectData, [zeroByte]]
        end
    fun toDocument bson = raise UnimplementedError
end
