main : IO ()
main = processStdin 0
	(\ frequency, input_line => ( "" , frequency + the Int ( cast input_line ) ) )
	show