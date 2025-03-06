- To handle the class hierarchy, I use a parent_map that maps from class name to parent name.

- My collect_features function, using a parent_map and the ast along with the cname, returns a feature list
  containing the methods and attributes for a particular class.

- For the dispatches, the way that I checked for the formals is by matching methods from collect_features,
  and extracting the strings for the types of the methods. i convert this string back to a static_type, to use the is_subtype function.
        in hindsight, it probably would have been better to use a Method environment as described in the CRM,
        collect_features is very inefficient because it is recomputing to get the same information over and over again.
- For case expressions, after typechecking the expressions, i loop through each branch and add its varaible to the object map
  and typecheck the body. i store the type of the body of each case branch in a list, and then do the lub function on the list to get the type.
  essentially recursively calling the lub (function that computes lub for two elements) throughout the list.

- to handle SELF_TYPE, i created a refernence variable that refers to the current class, throughout the program I updated this variable,
  such as when itering through the ast. when encountering a SELF_TYPE, i dereference this variable and treat it as that type.


I chose my good use case to ensure number of inherited features were correct for the implementation map.

for my bad use cases- i chose
wrong_type_selftype (bad1.cl)- It allowed me to ensure that SELF_TYPE was working correctly.
cyclic_inheritance (bad2.cl)- Ensuring that creating and traversing the parent_map is correct.
wrong_type_override (bad3.cl) - For typechecking inherited methods

