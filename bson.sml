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
    (*
     * Get the size (in bytes) of a bson object.
     *
     * @param bson a bson object
     * @return the size of the object in bytes
     *)
    val size: bson -> int
end

structure BSON :> BSON =
struct
    structure MD = MongoDoc

    type bson = Word8.word list
    exception InternalError
    exception NotImplementedError
    val zeroByte = Word8.fromInt 0
    fun makeList 0 element = nil
      | makeList n element = element::(makeList (n-1) element)
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

    (* Some constants. *)
    val EOO = Word8.fromInt 0
    val NUMBER = Word8.fromInt 1
    val STRING = Word8.fromInt 2
    val OBJECT = Word8.fromInt 3
    val ARRAY = Word8.fromInt 4
    val BINARY = Word8.fromInt 5
    val UNDEFINED = Word8.fromInt 6
    val OID = Word8.fromInt 7
    val BOOLEAN = Word8.fromInt 8
    val DATE = Word8.fromInt 9
    val NULL = Word8.fromInt 10
    val REGEX = Word8.fromInt 11
    val REF = Word8.fromInt 12
    val CODE = Word8.fromInt 13
    val SYMBOL = Word8.fromInt 14
    val CODE_W_SCOPE = Word8.fromInt 15
    val NUMBER_INT = Word8.fromInt 16

    fun elementType element =
        case element of
            MD.Document _ => OBJECT
          | MD.Array _ => ARRAY
          | MD.Bool _ => BOOLEAN
          | MD.Int _ => NUMBER_INT
          | MD.Float _ => NUMBER
          | MD.String _ => STRING
    (* TODO this ought to be UTF-8 encoded *)
    fun toCString s =
        let
            val s' = List.map Byte.charToByte (explode s)
        in
            List.concat [s', [zeroByte]]
        end
    fun toList vec = Word8Vector.foldr (op ::) [] vec
    fun intToWord8List int =
        let
            val array = Word8Array.array (4, zeroByte)
        in
            PackWord32Little.update (array, 0, (Word32.fromInt int));
            toList (Word8Array.vector array)
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
    fun assert bool = if bool then () else raise InternalError
    fun getInt bytes =
        let
            val firstFourBytes = List.take (bytes, 4) handle Subscript => raise InternalError
            val int = Word32.toIntX (PackWord32Little.subVecX (Word8Vector.fromList firstFourBytes, 0))
            val remainder = List.drop (bytes, 4) handle Subscript => raise InternalError
        in
            (int, remainder)
        end
    fun getByte bytes =
        let
            val byte = hd bytes handle Empty => raise InternalError
            val remainder = tl bytes handle Empty => raise InternalError
        in
            (byte, remainder)
        end
    (* TODO this NEEDS to handle UTF-8 *)
    fun getCString bytes =
        let
            val (byte, remainder) = getByte bytes
        in
            if byte = zeroByte then
                ("", remainder)
            else
                let
                    val (string, remainder') = getCString remainder
                in
                    (String.str (Byte.byteToChar byte) ^ string, remainder')
                end
        end
    fun getReal bytes =
        let
            val firstEightBytes = List.take (bytes, 8) handle Subscript => raise InternalError
            val real = PackRealLittle.fromBytes (Word8Vector.fromList firstEightBytes)
            val remainder = List.drop (bytes, 8) handle Subscript => raise InternalError
        in
            (real, remainder)
        end
    fun unwrapObject bson =
        let
            val (size, remainder) = getInt bson
            val elements = List.take (remainder, size - 5) handle Subscript => raise InternalError
            val remainder' = List.drop (remainder, size - 5) handle Subscript => raise InternalError
        in
            (assert (hd remainder' = zeroByte);
             (elements, tl remainder')) handle Empty => raise InternalError
        end
    fun arrayFromDocument document =
        let
            fun helper document index =
                let
                    val key = Int.toString index
                in
                    if MongoDoc.hasKey document key then
                        let
                            val value = valOf (MongoDoc.valueForKey document key)
                            val document' = MongoDoc.removeKey document key
                        in
                            value::helper document' (index + 1)
                        end
                    else
                        nil
                end
        in
            MongoDoc.Array (helper document 0)
        end
(* TODO this is hideous. couldn't get a case to work for some reason. must be something better than this... *)
    fun hydrateValue elementType bytes =
        if elementType = NUMBER_INT then
            let
                val (int, remainder) = getInt bytes
            in
                (MongoDoc.Int int, remainder)
            end
        else
            if elementType = BOOLEAN then
                let
                    val (bool, remainder) = getByte bytes
                in
                    if bool = zeroByte then
                        (MongoDoc.Bool false, remainder)
                    else
                        (MongoDoc.Bool true, remainder)
                end
            else
                if elementType = NUMBER then
                    let
                        val (real, remainder) = getReal bytes
                    in
                        (MongoDoc.Float real, remainder)
                    end
                else
                    if elementType = STRING then
                        let
                            val (size, remainder) = getInt bytes
                            val (string, remainder') = getCString remainder
                        in
                            assert (size = String.size string + 1);
                            (MongoDoc.String string, remainder')
                        end
                    else
                        if elementType = OBJECT then
                            let
                                val (document, remainder) = getDocument bytes
                            in
                                (MongoDoc.Document document, remainder)
                            end
                        else
                            if elementType = ARRAY then
                                let
                                    val (document, remainder) = getDocument bytes
                                    val array = arrayFromDocument document
                                in
                                    (array, remainder)
                                end
                            else
                                raise InternalError
    and hydrateElements bytes =
        case bytes of
            nil => nil
          | _ => (let
                      val (elementType, remainder) = getByte bytes
                      val (key, data) = getCString remainder
                      val (value, elements) = hydrateValue elementType data
                  in
                      (key, value)::(hydrateElements elements)
                  end)
    and getDocument bytes =
        let
            val (elements, remainder) = unwrapObject bytes
        in
            (MongoDoc.fromList (hydrateElements elements), remainder)
        end
    fun toDocument bson =
        let
            val (document, remainder) = getDocument bson
        in
            assert (length remainder = 0);
            document
        end
    fun size bson = length bson
end
