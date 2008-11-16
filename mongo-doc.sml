(* Copyright 2008 Michael Dirolf (mike@dirolf.com). All Rights Reserved. *)

(**
 * Format for documents stored or retrieved from a Mongo database.
 *
 * Defines the type for Mongo documents. Provides utilities for creating,
 * updating, getting data from, and printing Mongo documents.
 *)
(* TODO mongo docs should guarantee no duplicates *)
signature MONGO_DOC =
sig
    (**
     * A value that can be stored in a Mongo document.
     *)
    datatype value =
             Document of (string * value) list
           | Array of value list
           | Bool of bool
           | Int of int
           | Float of real
           | String of string
    (**
     * A Mongo document.
     *)
    type document
    (**
     * Extract a value from a Mongo document.
     *
     * @param document the document to extract a value from
     * @param key the key to look up
     * @return the value for the given key (NONE if no value exists for key)
     *)
    val valueForKey: document -> string -> value option
    (**
     * Create a Mongo document for a list of (key, value) pairs.
     *
     * @param list a list of (key, value) pairs
     * @return a Mongo document containing those same pairs
     *)
    val fromList: (string * value) list -> document
    (**
     * Create a list of (key, value) pairs from a Mongo document.
     *
     * Note: toList (fromList l) will not necessarily be identical to l.
     *       The result of toList is guaranteed to not contain more than
     *       one pair with a given key. (TODO this is not true!)
     * @param document a Mongo document
     * @return a list of the (key, value) pairs that make up the document
     *)
    val toList: document -> (string * value) list
    (**
     * Pretty print a Mongo document.
     *
     * @param document a Mongo document
     *)
    val print: document -> unit
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
    exception UnimplementedError
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
    fun fromList list = list
    fun toList document = document
    fun indent width =
        case width of
            0 => ()
          | n => (print " "; indent (n - 1))
    fun printValue value =
        case value of
            Document d => raise UnimplementedError
          | Array a => raise UnimplementedError
          | Bool b => raise UnimplementedError
          | Int i => print (Int.toString i)
          | Float f => raise UnimplementedError
          | String s => print ("\"" ^ s ^ "\"")
    fun printBinding indentation trail (key, value) =
        (indent indentation;
         print (key ^ ": ");
         printValue value;
         print (trail ^ "\n"))
    fun printDocument indentation document =
        case document of
            nil => print "{}\n"
          | _ => (print "{\n";
                  List.map (printBinding (indentation + 4) ",") (List.take (document, List.length document - 1));
                  printBinding (indentation + 4) "" (List.last document);
                  print "}\n")
    fun print document = printDocument 0 document
end
