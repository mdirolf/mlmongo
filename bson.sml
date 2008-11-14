(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)
signature BSON =
sig
    type value
    val print: value -> unit
    val fromDocument: MongoDoc.document -> value
    val toDocument: value -> MongoDoc.document
end

structure BSON :> BSON =
struct
    structure MD = MongoDoc

    type value = Word8.word list
    exception InternalError
    exception UnimplementedError
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
    val print = fn bson =>
        let
            fun padStringLeft string count char =
                String.implode (padLeft (String.explode string) count char)
            fun printHelper lineNumber bson =
                case bson of
                    nil => print "\n"
                  | hd::tl =>
                    (if lineNumber mod 8 = 0 then
                        print ("\n" ^ (padStringLeft (Int.toString lineNumber) 4 #" ") ^ ":  ")
                     else
                        print " ";
                     print (padStringLeft (Word8.toString hd) 2 #"0");
                     printHelper (lineNumber + 1) tl)
        in
            printHelper 0 bson
        end
    fun dedup document =
        let
            fun contains list (elem:string) =
                case list of
                    hd::tl => if hd = elem then true else contains tl elem
                  | nil => false
            fun dedup_helper (document: (string * MD.value) list) seen =
                case document of
                    hd::tl => if contains seen (#1 hd) then dedup_helper tl seen else hd::dedup_helper tl seen
                  | nil => nil
        in
            MD.fromList (dedup_helper (MD.toList document) nil)
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
            List.concat [s', [Word8.fromInt 0]]
        end
    (* TODO better name, and use this more places *)
    val eoo = Word8.fromInt 0
    fun intToWord8List int =
        let
            fun helper int count =
                if count = 0 then
                    nil
                else
                    if int = 0 then
                        eoo::(helper 0 (count - 1))
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
                              MD.Document d => fromDocument (MD.fromList d)
                            | MD.Array a => fromDocument (MD.fromList (listAsArray a))
                            | MD.Bool b => if b then [Word8.fromInt 1] else [Word8.fromInt 0]
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
            val document' = dedup document
            val document'' = MD.toList document'
            val objectData = List.concat(List.map elementToBSON document'')
            (* overhead for the size bytes and eoo.
             * TODO should this include the eoo byte or not? *)
            val overhead = 5
            val size = intToWord8List (length objectData + overhead)
        in
            List.concat [size, objectData, [eoo]]
        end
    fun toDocument value = raise UnimplementedError
end
