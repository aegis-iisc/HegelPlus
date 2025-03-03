Failing Queries
- x:Int -> y:Int -> z:[a] -> [a]
 0
 2
[1,3,5,6]
[1,3,5,6,1,5]

1
2
[1,3,5,6]
[1,3,5,6,3,5]


0
2
['a','b','c','d']
['a','b','c','d', 'a','c']
Required output: reverse (x!!2 : (x!!0 : (reverse x)))



- x : [a] -> [a]
[1,2]
[1,2,2,1]

[1,2,3,4]
[1,2,3,4,4,3,2,1]

['a','b']
['a','b','b','a']


Required output: x ++ reverse (x)

-Num a => x:[a] -> y:Int -> a

--encode the condition find the sum of first y elements of x--
[49, 62, 82, 54, 76]
2
111

[49, 62, 82, 54, 76]
3
193

[1,3,5]
2
4

Finds a solution: \x y. sum (take y x)

--change the query a bit and ask for the sum of last y elements of x--
[49, 62, 82, 54, 76]
2
130

[49, 62, 82, 54, 76]
3
212

[49, 62, 82, 54, 76]
1
76

[49, 62, 82, 54, 76]
4
274

[49, 62, 82, 54, 76]
5
323

Fails to find a solution even after a very detailed list of examples 
Required output: \x y. sum (take y (reverse x))
