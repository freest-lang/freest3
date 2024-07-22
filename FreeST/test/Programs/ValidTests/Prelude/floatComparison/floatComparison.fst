main : Bool
main = 
	let a = 1.0 <. 2.0 in
	let b = 1.0 <=. 2.0 in
	let c = 2.0 >. 1.0 in
	let d = 2.0 >=. 1.0 in
	a && b && c && d
