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
(* TODO add a set function to set a (key, value) pair *)
    (**
     * Extract a value from a Mongo document.
     *
     * @param document the document to extract a value from
     * @param key the key to look up
     * @return the value for the given key (NONE if no value exists for key)
     *)
(* TODO test that valueForKey returns NONE when document doesn't contain key *)
(* TODO test that valueForKey returns SOME when document does contain key *)
    val valueForKey: document -> string -> value option
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
(* TODO test that [fromList (toList d) = d] *)
(* TODO test that [toList d] contains no duplicates *)
    val toList: document -> (string * value) list
    (**
     * Pretty print a Mongo document.
     *
     * @param document a Mongo document
     *)
(* TODO test pretty printing, somehow... *)
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
            0 => ()
          | n => (print " "; indent (n - 1))
    fun printValue indentation value =
        case value of
            Document d => printDocument indentation d
          | Array a => printArray indentation a
          | Bool b => print (Bool.toString b)
          | Int i => print (Int.toString i)
          | Float f => print (Real.toString f)
          | String s => print ("\"" ^ s ^ "\"")
    and printArrayValue indentation trail value =
        (indent indentation;
         printValue indentation value;
         print (trail ^ "\n"))
    and printArray indentation array =
        case array of
            nil => print "[]\n"
          | _ => (print "[\n";
                  List.map (printArrayValue (indentation + 4) ",") (List.take (array, List.length array - 1));
                  printArrayValue (indentation + 4) "" (List.last array);
                  indent indentation;
                  print "]")
    and printBinding indentation trail (key, value) =
        (indent indentation;
         print (key ^ ": ");
         printValue indentation value;
         print (trail ^ "\n"))
    and printDocument indentation document =
        case document of
            nil => print "{}\n"
          | _ => (print "{\n";
                  List.map (printBinding (indentation + 4) ",") (List.take (document, List.length document - 1));
                  printBinding (indentation + 4) "" (List.last document);
                  indent indentation;
                  print "}")
    val print = fn document => (printDocument 0 document; print "\n")
end
