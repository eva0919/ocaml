(* 將string -> string list *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) ((Char.escaped s.[i]) :: l) in
  exp (String.length s - 1) []

(* 遇到＋、-時考慮有沒有被壓在下面且沒有用括號隔開的*、/，有則移到result的stack *)
let rec s1ToPlusOrMinus s = 
	if s = [] then [] 
    else match (List.hd s) with
	  "+" -> s
	| "-" -> s
	| ")" -> s
	| _ -> s1ToPlusOrMinus (List.tl s)
let resultToPlusOrMinus inputS1 inputResult = 
  let rec resultToPlusOrMinusDetail (s:string list) (result:string list) = 
    if s = [] then result
    else match (List.hd s) with
	  "+" -> result
	| "-" -> result
	| ")" -> result
	| _ -> resultToPlusOrMinusDetail (List.tl s) ((List.hd s)::result) in
(resultToPlusOrMinusDetail inputS1 [])@inputResult
(* ******************************************************************* *)

(* 遇到右括號直接放入stack，但遇到左括號的時候，要將operator stack（s1)中，在右括號之前的東西都pop出來 *)
let rec s1ToLeftParenthesis s = 
	if s = [] then [] 
    else match (List.hd s) with
	  ")" -> List.tl s
	| _ -> s1ToLeftParenthesis (List.tl s)
let resultToLeftParenthesis inputS1 inputResult = 
  let rec resultToLeftParenthesisDetail (s:string list) (result:string list) = 
	if s = [] then result
    else match (List.hd s) with
	  ")" -> result
	| _ -> resultToLeftParenthesisDetail (List.tl s) ((List.hd s)::result) in
(resultToLeftParenthesisDetail inputS1 [])@inputResult
(* **************************************************************************************** *)

(* 主要function，revstr為使用者輸入的字串轉為string list後reverse後的結果；s1為operator（暫存）；result為最後要輸出的結果 *)
(* stack遇到*/)時直接放入s1，遇到+-時要考慮是不是有在s1中的*/被壓在下面，遇到(時要將)之前的operator pop出來 *)
let rec infixToPrefixDetail (revstr:string list) (s1:string list) (result:string list) = 
	match revstr with
	  [] -> (List.rev s1)@result
	| x::y -> if x = "*" || x = "/" || x = ")" then infixToPrefixDetail y (x::s1) result
			  else if x = "+" || x = "-" then infixToPrefixDetail y (x::(s1ToPlusOrMinus s1)) (resultToPlusOrMinus s1 result)
			       else if x = "(" then infixToPrefixDetail y (s1ToLeftParenthesis s1) (resultToLeftParenthesis s1 result)
			       	    else infixToPrefixDetail y s1 (x::result)
			  								
(* 以 infixToPrefix "a*(b+c*(d+e))+f" 這樣的方式，可以將infix轉為prefix *)
let infixToPrefix input = infixToPrefixDetail (List.rev (explode input)) [] []






(* ****************************************** *)
(* hw8 start *)





type bitree = Leaf of string | Node of string * bitree * bitree | Empty

exception Oops

let left = function Node (x, y, z) -> y
let right = function Node (x, y, z) -> z
let getVal a = match a with 
	Node (x, y, z) -> x
	| Leaf (w) -> w
let getType a = match a with
	"+" -> "operator"
	| "-" -> "operator"
	| "*" -> "operator"
	| "/" -> "operator"
	| "x" -> "variable"
	| "y" -> "variable"
	| _ -> "number"
	(* 如果傳入z之類的也會回傳number *)
(* let addTreeValue aTree a = match a with *)
let rec isLeft aTree = match aTree with
	Empty -> true
	| Leaf (w) -> false
	| Node (x, y ,z) -> isLeft y || isRight y
	and isRight aTree = match aTree with
	Empty -> true
	| Leaf (w) -> false
	| Node (x, y ,z) -> isLeft z || isRight z
let rec insertOperator aTree x = match aTree with
	Empty -> Node(x, Empty, Empty)
	| Node(a, l, r) -> if isLeft aTree then Node(a, (insertOperator l x), r)
					   else if isRight aTree then Node(a, l, (insertOperator r x))
					   else raise Oops
	| Leaf(a) -> raise Oops
let rec insertNumber aTree x = match aTree with
	Empty -> Leaf(x)
	| Node(a, l, r) -> if isLeft aTree then Node(a, (insertNumber l x), r)
					   else if isRight aTree then Node(a, l, (insertNumber r x))
					   else raise Oops
	| Leaf(a) -> raise Oops
(* let rec insertOperator aTree x =
	if Empty then Node(x, Empty, Empty)
	else if isLeft aTree then insertOperator (left aTree) x
	else if isRight aTree then insertOperator (right aTree) x
let rec insertNumber aTree x =
	if Empty then Leaf(x)
	else if isLeft aTree then insertNumber (left aTree) x
	else if isRight aTree then insertNumber (right aTree) x *)
	
let rec formAbstractTreeDetail aList ansTree = match aList with 
	[] -> ansTree
	| x::y -> if getType x = "number" || getType x = "variable" then formAbstractTreeDetail y (insertNumber ansTree x)
			  else if getType x = "operator" then formAbstractTreeDetail y (insertOperator ansTree x)
			  else raise Oops

let formAbstractTree input = formAbstractTreeDetail (infixToPrefix input) Empty


(*Mike code*)
let isZeroLeaf l =
	match l with
	| Leaf (a) -> if a = "0" then true
	              else false
	| _ -> false

let plusSimpling aTree =
	match aTree with
	| Node (o, l,r )-> if (isZeroLeaf l) then r 
	                   else if (isZeroLeaf r) then l
	                   else Node (o, l, r)
	| _ -> raise Oops


let rec plusSimple aTree =
	match aTree with
	Empty -> raise Oops
	| Node(o, l,r ) -> if o = "+" then plusSimpling (Node (o,(plusSimple l),(plusSimple r)) )
	                   else Node (o,(plusSimple l),(plusSimple r))
	| Leaf (a) -> Leaf (a)
