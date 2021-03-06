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
datatype 'element set = Empty
  | Set of 'element * 'element set;

(* determines if an element e is part of the set, aSet. Returns true if e is a member. *)
fun isMember e Empty = false
  | isMember e (Set(element, aSet)) =
  if e = element then true
  else isMember e aSet;

(* converts a list into a set, not allowing duplicates *)
  fun list2Set [] = Empty
    | list2Set (element::tail) =
    let val a = list2Set tail
    in if isMember element a
    then a
    else Set(element, a)
    end;

(* UNION *)
(* takes the set union of set1 and set 2 and returns a set representing the
   mathematical union of two sets.*)
fun union (Set(element, set1)) set2 =
  (* don't add elements to the union set if they are already in it *)
  if isMember element set2
  then union set1 set2
  (* check for elements to add from set1 that still need adding; if empty, there
     are no more elements to add. *)
  else if set1 = Empty
  then Set(element, set2)
  (* add element from set1 to the union set and recurse*)
  else Set(element, union set1 set2);

(* INTERSECT *)
(* takes the intersection of set1 and set2 and returns the set representing the
  mathematical intersection of the two sets *)
fun intersect (Set(element, set1)) set2 =
  (* checks to see if set1 is not empty and if the element from set1 is also in
     set2, and if yes then add to intersection set *)
  if set1 = Empty andalso isMember element set2
  then Set(element, Empty)
  (* if end of set1 reached, don't add more to intersection set *)
  else if set1 = Empty
  then Empty
  (* if element is in both sets, add to intersection set *)
  else if isMember element set2
  then Set(element, intersect set1 set2)
  else intersect set1 set2;



(****************************** v given code v ********************************)
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
val quest5 = isMember "one" (list2Set["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
