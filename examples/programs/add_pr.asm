# This is an address label, all instruction after it will be located relative to it
0x1:
	# This is a `PUSHC` instruction with the necessary constant following it
	PUSHC
	!0o7
	PUSHC
	!0b11
	ADD
20:
	HCF
