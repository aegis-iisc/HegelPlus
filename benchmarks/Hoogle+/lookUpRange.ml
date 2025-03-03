
(* <!-- 18 lookupC *)
(* Eq a => [(a,b)] -> a -> b --> *)
stack exec -- hplus --disable-filter=False --json='{"query": "Eq a => [(a,b)] -> a -> b", 
"inExamples": [],
"inArgNames": ["x", "y"]}'

stack exec -- hplus --disable-filter=False --json='{"query": "Eq a => [(a,a)] -> a -> a", 
"inExamples": [],
"inArgNames": ["x", "y"]}'


<!-- Examples -->
stack exec -- hplus --disable-filter=False --json='{"query": "Eq a => [(a,b)] -> a -> b", 
"inExamples": [{"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "2"], 
                "output": "3"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "3"], 
                "output": "4"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "5"], 
                "output": "6"}
                ],
"inArgNames": ["x", "y"]}'

<!-- Suceeds -->
stack exec -- hplus --disable-filter=False --json='{"query": "Eq a => [(a,a)] -> a -> a", 
"inExamples": [{"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "2"], 
                "output": "3"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "3"], 
                "output": "4"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "5"], 
                "output": "6"}
                ],
"inArgNames": ["x", "y"]}'


<!-- Fails -->
stack exec -- hplus --disable-filter=False --json='{"query": "Eq a => [(a,b)] -> a -> b", 
"inExamples": [{"inputs": ["[(1,2), (2,3), (3, 4), (5, 6)]", "3"], 
                "output": "2"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "2"], 
                "output": "1"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "6"], 
                "output": "5"}
                ],
"inArgNames": ["x", "y"]}'
<!-- Fails -->
stack exec -- hplus --disable-filter=False --json='{"query": "Eq a => [(a,a)] -> a -> a", 
"inExamples": [{"inputs": ["[(1,2), (2,3), (3, 4), (5, 6)]", "3"], 
                "output": "2"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "2"], 
                "output": "1"},
                {"inputs": ["[(1,2), (2,3), (3, 4), (5,6)]", "6"], 
                "output": "5"}
                ],
"inArgNames": ["x", "y"]}'

(* lookUpRange for a given function *)
goal = ??



