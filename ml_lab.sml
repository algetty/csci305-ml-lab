(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Amelia Getty
* gettyamelia@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)

(* "warmup" function *)
fun f [] = [] (* a - defines function f of input and output type of list*)
  | f (x::xs) = (x + 1) :: (f xs); (* b - adds 1 to the element x of the list
                                      and inserts it into the list xs*)

(****** SETS DATATYPE ******)
(* constructed set datatype; can hold polymorphic type 'element and sets of 'element *)
datatype 'element set =
  Empty | Set of 'element * 'element set;

(* determines if an element e is part of the set, aSet. Returns true if e is a member. *)
fun isMember (e, Set(element, aSet)) =
  if e = element then true
  else if aSet = Empty then false
  else isMember(e, aSet);

(* converts a list into a set, not allowing duplicates *)
fun list2Set [] = Empty
  | list2Set (head::tail) = Set(head, list2Set(tail));

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9]; (* this needed a semicolon; added. *)

(* Question 5 *)
(* this didn't work without an extra set of parenthases and a comma *)
val quest5 = isMember("one", (list2Set["1", "2", "3", "4"]));
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* questions 9 and 10 also needed extra parenthases and commas to work with my functions *)
(* Question 9 *)
print "\nQuestion 9: ";
print_str (union ((list2Set ["green", "eggs", "and"]), (list2Set ["ham"])));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect ((list2Set ["stewed", "tomatoes", "and", "macaroni"]), (list2Set ["macaroni", "and", "cheese"])));
