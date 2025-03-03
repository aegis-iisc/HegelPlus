
(* 17 splitStr String -> Char -> [String] *)


(* Original *)
stack exec -- hplus --disable-filter=False --json='{"query": "String -> Char -> (String, String)", 
"inExamples": [],
"inArgNames": ["x", "y"]}'

(* E2 *)
stack exec -- hplus --disable-filter=False --json='{"query": "String -> Char -> (String, String)", 
"inExamples": [{ "inputs": ["\"abc\"", "\'b\'" ], 
                "output": "["\"ab\"", "\"c\"""},
                {"inputs": ["\"abcd\"", "\'c\'", 
                "output": "["\"abc\"", "\"d\""}],
"inArgNames": ["x", "y"]}' 




stack exec -- hplus --disable-filter=False --json='{"query": "String -> Char -> (String, String)", 
"inExamples": [{ "inputs": ["\"abc\"", "\'b\'" ], 
                "output": "["\"ab\"", "\"c\""},
                {"inputs": ["\"abcd\"", "\'c\'"], 
                "output": "["\"abc\"", "\"d\""}],
"inArgNames": ["x", "y"]}'


(* E6 *)
stack exec -- hplus --disable-filter=False --json='{"query": "String -> Char -> [String]", 
"inExamples": [{ "inputs": ["\"abc\"", "\'b\'" ], 
                "output": "["\"ab\"", "\"c\"" }],
"inArgNames": ["x", "y"]}'


(* <!-- Refinement types --> 
 Treast Strings as list of chars  
  *)
goal = (s : string) -> (c : char) -> { v : (String, String) | ?? }
