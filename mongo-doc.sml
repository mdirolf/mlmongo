(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)

(**
 * Format for documents stored or retrieved from a Mongo database.
 *
 * Defines the type for Mongo documents. Provides utilities for creating,
 * updating, getting data from, and printing Mongo documents.
 *)
signature MONGO_DOC =
sig
    (**
     * A Mongo document.
     *)
    type document
    (**
     * A value that can be stored in a Mongo document.
     *)
    datatype value =
             Document of document
           | Array of value list
           | Bool of bool
           | Int of int
           | Float of real
           | String of string
    (**
     * Set a (key, value) binding on a Mongo document.
     *
     * @param document document to add a binding to
     * @param binding (key, value) pair specifying the binding to add
     * @return the resulting document
     *)
    val setBinding: document -> string * value -> document
    (**
     * Extract a value from a Mongo document.
     *
     * @param document the document to extract a value from
     * @param key the key to look up
     * @return the value for the given key (NONE if no value exists for key)
     *)
    val valueForKey: document -> string -> value option
    (**
     * Indicates whether or not a Mongo document contains a key.
     *
     * @param document the document to inspect
     * @param key the key to look up
     * @return true if the document contains the key, false otherwise
     *)
    val hasKey: document -> string -> bool
    (**
     * Indicates whether or not a Mongo document is empty.
     *
     * @param document the document to check
     * @return true if the document is empty, false otherwise
     *)
    val isEmpty: document -> bool
    (**
     * Remove a key from a Mongo document.
     *
     * @param document the document to remove a key from
     * @param key the key to remove
     * @return a new document, with key removed if it was originally present
     *)
    (* TODO test for this *)
    val removeKey: document -> string -> document
    (**
     * Create a Mongo document for a list of (key, value) pairs.
     *
     * @param list a list of (key, value) pairs
     * @return a Mongo document containing those same pairs
     *)
    (* TODO test that [d has no duplicates ==> toList (fromList d) = d] *)
    val fromList: (string * value) list -> document
    (**
     * Create a list of (key, value) pairs from a Mongo document.
     *
     * Note: toList (fromList l) will not necessarily be identical to l.
     *       The result of toList is guaranteed to not contain more than
     *       one pair with a given key.
     * @param document a Mongo document
     * @return a list of the (key, value) pairs that make up the document
     *)
    val toList: document -> (string * value) list
    (**
     * Pretty print a Mongo document to a string.
     *
     * @param document a Mongo document
     * @return a string representation of the document
     *)
    val toString: document -> string
    (**
     * Check if two values are equal.
     *)
    val valueEqual: value -> value -> bool
    (**
     * Check if two documents are equal.
     *
     * Documents cannot be an equality type since they contain reals.
     * @param document1 a Mongo document
     * @param document2 a Mongo document
     * @return a bool indicating if the two documents are equal
     *)
    val equal: document -> document -> bool
end

structure MongoDoc :> MONGO_DOC =
struct
    datatype value =
             Document of (string * value) list
           | Array of value list
           | Bool of bool
           | Int of int
           | Float of real
           | String of string
    type document = (string * value) list
    fun valueForKey (document: document) key =
        let
            val value = List.find (fn (s, _) => s = key) document
        in
            if Option.isSome value then
                let
                    val (_, result) = Option.valOf value
                in
                    SOME(result)
                end
            else
                NONE
        end
    fun hasKey document key = Option.isSome (valueForKey document key)
    fun isEmpty document = List.null document
    fun removeKey (document: document) key = List.filter (fn (s, _) => s <> key) document
    fun setBinding document (key, value) = (key, value)::(removeKey document key)
    (* NOTE Nested documents are guaranteed to be already dedup-ed.
     * We know this since there is no way to create a document with any
     * duplicates in it. *)
    fun dedup document =
        let
            fun contains list (elem:string) =
                case list of
                    hd::tl => if hd = elem then true else contains tl elem
                  | nil => false
            fun dedup_helper (document: document) seen =
                case document of
                    hd::tl => if contains seen (#1 hd) then dedup_helper tl seen else hd::dedup_helper tl ((#1 hd)::seen)
                  | nil => nil
        in
            dedup_helper document nil
        end
    fun fromList list = dedup list
    fun toList document = document
    fun indent width =
        case width of
            0 => ""
          | n => " " ^ indent (n - 1)
    (* TODO change names to show that we aren't printing anymore. *)
    fun printValue indentation value =
        case value of
            Document d => printDocument indentation d
          | Array a => printArray indentation a
          | Bool b => Bool.toString b
          | Int i => Int.toString i
          | Float f => Real.toString f
          | String s => "\"" ^ s ^ "\""
    and printArrayValue indentation trail value = indent indentation ^ printValue indentation value ^ trail ^ "\n"
    and printArray indentation array =
        case array of
            nil => "[]"
          | _ => "[\n" ^
                 String.concat (List.map (printArrayValue (indentation + 4) ",") (List.take (array, List.length array - 1))) ^
                 printArrayValue (indentation + 4) "" (List.last array) ^
                 indent indentation ^
                 "]"
    and printBinding indentation trail (key, value) =
        indent indentation ^ key ^ ": " ^
        printValue indentation value ^ trail ^ "\n"
    and printDocument indentation document =
        case document of
            nil => "{}"
          | _ => "{\n" ^
                 String.concat (List.map (printBinding (indentation + 4) ",") (List.take (document, List.length document - 1))) ^
                 printBinding (indentation + 4) "" (List.last document) ^
                 indent indentation ^ "}"
    fun toString document = (printDocument 0 document) ^ "\n"
    fun valueEqual value1 value2 =
        case value1 of
            Document d1 => (case value2 of
                                Document d2 => equal d1 d2
                              | _ => false)
          | Array a1 => (case value2 of
                             Array a2 => List.all (fn (a,b) => valueEqual a b) (ListPair.zip (a1, a2))
                           | _ => false)
          | Bool b1 => (case value2 of
                            Bool b2 => b1 = b2
                          | _ => false)
          | Int i1 => (case value2 of
                           Int i2 => i1 = i2
                         | _ => false)
          | Float f1 => (case value2 of
                             Float f2 => Real.== (f1, f2)
                           | _ => false)
          | String s1 => (case value2 of
                              String s2 => s1 = s2
                            | _ => false)
    and bindingInDoc (key, value1) document =
        let
            val value2 = valueForKey document key
        in
            Option.isSome value2
            andalso valueEqual value1 (Option.valOf value2)
        end
    and equal document1 document2 =
        case document1 of
            nil => (case document2 of
                        nil => true
                      | _ => false)
          | (key, value)::tl => bindingInDoc (key, value) document2
                      andalso equal tl (removeKey document2 key)
end
