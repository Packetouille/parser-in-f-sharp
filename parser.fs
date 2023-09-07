(*
Assignment: A parser in F#

Name: Londono, Andres
*)

// GRAMMER
// program ⟶ stmt_list 
// stmt_list ⟶ stmt stmt_list | ε 
// stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt 
// expr ⟶ term term_tail 
// term_tail ⟶ add_op term term_tail | ε 
// term ⟶ factor factor_tail
// factor_tail ⟶ mult_op factor factor_tail | ε 
// factor ⟶ ( expr ) | id 
// add_op ⟶ - | + 
// mult_op ⟶ * | / 
// rel_oper ⟶ > | < | == 
// cond ⟶ expr rel_oper expr 
// assignment ⟶ id := expr 
// read_stmt ⟶ read id 
// write_stmt ⟶ write expr 
// if_stmt ⟶ if cond then stmt_list else_stmt 
// else_stmt ⟶ else stmt_list fi | fi 
// for_loop ⟶ for id = id to id step_stmt do stmt_list done 
// step_stmt ⟶ step id | ε 
// id ⟶ <any lexeme/token that's not already expressed as a terminal above>


// Tokens
type Token =
    | ID of string
    | EQUALS of string
    | IF of string
    | ELSE of string
    | END of string
    | FOR of string
    | DO of string
    | READ of string
    | WRITE of string
    | ADD_OP of string
    | MULT_OP of string
    | RELATIONAL of string
    | OPER of string
    | PARENTHESIS of string
    | FAILED of string
    | REASON of string

    with static member tokenFromLexeme str = // Function to get a token from a lexeme (String)
            match str with
            | ":=" -> EQUALS str
            | "if" | "then" -> IF str
            | "fi" | "done" -> END str
            | "else" -> ELSE str
            | "for" | "to" | "step" -> FOR str
            | "do" | "until" -> DO str
            | "read" -> READ str
            | "write" -> WRITE str
            | "+" | "-" -> ADD_OP str
            | "*" | "/" -> MULT_OP str
            | ">" | "<" | "==" -> RELATIONAL str
            | "=" -> OPER str
            | "(" | ")" -> PARENTHESIS str
            | "failure" -> FAILED str
            | x -> ID str



let parseResult parsedList str =
    match parsedList with
        | [] -> printfn $"The Sentence '{str}' follows the grammar."
        | _ -> failwith $"The Sentence '{str}' is incorrect because {parsedList.[1]}" // parsedList comes in as [FAILED failedToken; REASON reasonStr]. output REASON at index 1


let rec parse theList str = program theList str


// program ::= stmt_list
and program lst str = 
    lst |> stmt_list |> parseResult <| str


// stmt_list ::= stmt stmt_list | <empty>
and stmt_list = function
    | [] -> []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: [] 
    | ELSE elseToken :: xs as t -> t
    | END fiToken :: xs as t -> t
    | xs -> xs |> stmt |> stmt_list


// stmt ::= assignment | read_stmt | write_stmt | for_loop | if_stmt
 and stmt = function
    | [] -> []
    | ID idToken :: EQUALS equalToken :: xs as t -> t |> assignment
    | READ readToken :: xs as t -> t |> read_stmt
    | WRITE writeToken :: xs as t-> t |> write_stmt
    | FOR forToken :: xs as t -> t |> for_loop
    | IF ifToken :: xs as t -> t |> if_stmt
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"invalid start of string. If 'id' it should be followed by ':=' but received {xs}" :: []


// assignment ⟶ id := expr 
and assignment = function
    | ID idToken :: xs -> xs |> assignment
    | EQUALS eqToken :: xs -> xs |> expr 
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"unexpected end of assignment. received {xs}" :: [] 


// read_stmt ::= read id
and read_stmt = function
    | READ readToken :: xs -> xs |> function
            | ID id :: xs -> xs 
            | xs -> FAILED "failure" :: REASON $"expecting ID but instead received {xs}" :: []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"could not match read token. Received {xs}" :: [] 


// write_stmt ::= write expr
and write_stmt = function
    | WRITE writeToken :: xs -> xs |> expr 
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"unexpected end of write_stmt production. Received {xs}" :: [] 


// expr ::= term term_tail 
and expr = function
    | xs -> xs |> term |> term_tail


// term ::= factor factor_tail
and term = function
    | xs -> xs |> factor |> factor_tail


// term_tail ::= add_op term term_tail | ε  
and term_tail = function
    | ADD_OP addLabel :: xs as t -> t |> add_op |> term |> term_tail
    | xs -> xs


// factor ::= ( expr ) | id 
and factor = function
    | PARENTHESIS parToken :: xs -> xs |> expr |> function
            | PARENTHESIS parToken :: xs -> xs
            | xs -> FAILED "failure" :: REASON $"expected ')' but received {xs}" :: []
    | ID idToken :: xs -> xs
    | [] -> FAILED "failure" :: REASON $"expected either '(' or an id but list was empty." :: []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"expected either '(' or an id but received {xs}" :: []


// factor_tail ::= mult_op factor factor_tail | ε 
and factor_tail = function
    | MULT_OP multOpLabel :: xs as t -> t |> mult_op |> factor |> factor_tail
    | xs -> xs
 

// add_op ::= - | + 
and add_op = function
    | ADD_OP addOpToken :: xs -> xs
    | [] -> FAILED "failure" :: REASON $"expected either '+' or a '-' but list was empty." :: []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"expected + or - but received {xs}" :: []


// mult_op ::= * | / 
and mult_op = function
    | MULT_OP multOpToken :: xs -> xs
    | [] -> FAILED "failure" :: REASON $"expected either '*' or a '/' but list was empty." :: []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"expected * or / but received {xs}" :: []


// if_stmt ⟶ if cond then stmt_list else_stmt 
and if_stmt = function
    | IF ifToken :: xs -> xs |> cond |> function 
            | IF thenToken :: xs -> xs |> stmt_list |> else_stmt
            | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
            | xs -> FAILED "failure" :: REASON $"expected 'then' after the condition but received {xs}" :: []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"unexpected end of if statement. received {xs}" :: []


// else_stmt ⟶ else stmt_list fi | fi
and else_stmt = function
    | ELSE elseToken :: xs -> xs |> stmt_list |> function
            | END fiToken :: xs -> xs 
            | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
            | [] -> FAILED "failure" :: REASON $"expected 'fi' but received {xs}" :: []
            | xs -> FAILED "failure" :: REASON $"expected 'fi' but received {xs}" :: []
    | END fiToken :: xs -> xs
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"expected 'else' or 'fi' but received {xs}" :: []


// cond ⟶ expr oper expr
and cond = function
    | xs -> xs |> expr |> rel_oper |> expr


// rel_oper ⟶ > | < | == 
and rel_oper = function
    | RELATIONAL relToken :: xs -> xs
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"expected an operator < > or = but received {xs}" :: []

 
// for_loop ⟶ for id = id to id step_stmt do stmt_list done
and for_loop = function 
    | FOR forToken :: xs -> xs |> function
        | ID idToken :: xs -> xs |> function
            | OPER equalToken :: xs -> xs |> function
                | ID idToken :: xs -> xs |> function
                    | FOR toToken :: xs -> xs |> function
                        | ID idToken :: xs -> xs |> function
                            | xs -> xs |> step_stmt |> function
                                | DO doToken :: xs -> xs |> stmt_list |> function
                                    | END doneToken :: xs -> xs
                                    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
                                    | xs -> FAILED "failure" :: REASON $"expected 'done' but recevied {xs}" :: []
                                | FAILED failedToken :: xs as t -> t
                                | xs -> FAILED "failure" :: REASON $"expected 'do' but recevied {xs}" :: []
                        | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
                        | xs -> FAILED "failure" :: REASON $"expected another id but recevied {xs}" :: []
                    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
                    | xs -> FAILED "failure" :: REASON $"expected 'to' but received {xs}" :: []
                | xs -> FAILED "failure" :: REASON $"expected another id but received {xs}" :: []
            | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
            | xs -> FAILED "failure" :: REASON $"expected '=' but received {xs}" :: []
        | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
        | xs -> FAILED "failure" :: REASON $"expected an id but received {xs}" :: []
    | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
    | xs -> FAILED "failure" :: REASON $"expected token in for loop. received {xs.Head}" :: []


// step_stmt ⟶ step id | ε 
and step_stmt = function
    | FOR stepToken :: xs -> xs |> function
            | ID idToken :: xs -> xs
            | FAILED failureToken :: REASON reasonStr :: xs -> FAILED failureToken :: REASON reasonStr :: []
            | xs -> FAILED "failure" :: REASON $"expected id after step but recevied {xs}" :: []
    | xs -> xs


(* Begin Parsing Process *)
let startParsing (str:string) =
    let tokenList =
        str.Split ' ' |>
        Array.toList |>
        List.map Token.tokenFromLexeme

    printfn $"The initial String: %s{str}"
    printfn $"The initial List: %A{tokenList}\n"

    try
        let parsedList = (program tokenList str)
        in printfn $"The Final List:\n\t%A{parsedList}"
    with
        Failure msg -> printfn $"Error: %s{msg}";  System.Console.ReadLine () |> ignore


(* Get the user input and start parsing *)
let promptAndGo () =
    let userInput =
        printf "Enter String: ";
        
        System.Console.ReadLine () 

    in startParsing userInput


// RUN INTERACTIVELY AS A SCRIPT
promptAndGo ()


//Sample Input:

(*
sum := 0 
read number 

abs := n 

if n < 0 then 
    abs := 0 - abs 
    for i = 1 to max step i 
        do 
            sum := sum * ( 1 + n ) / 100 
            write sum + i 
        done
else 
    if n > 0 then 
        n := n + 1 
    fi 
fi 

write sum + count 

if sum > count then 
    sum := sum * count + ( 10 * n ) * 30 
fi


for i = 1 to max step i do sum := sum * ( 1 + n ) / 100 write sum + i done

for i = 1 to max do sum := sum * ( 1 + n ) / 100 write sum + i done

write sum + n sum := 0 read number abs := n if n < 0 then abs := 0 - abs for i = 1 to max step i do sum := sum * ( 1 + n ) / 100 write sum + i done else if n > 0 then n := n + 1 fi fi write sum + count if sum > count then sum := sum * count + ( 10 * n ) * 30 fi

if sum > count then read sum else read count fi
*)