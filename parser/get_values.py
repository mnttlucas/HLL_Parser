# Little script to debug SMT with z3

# Put the streams you want to (get-value)
streams = ["s680", "s1870", "s1184", "s1423", "s2056", "s1002"]

for stream in streams:
	print("(get-value (", end = "")
	# Instead of range(10), put the same range as -stop when generating the SMT
	for i in range(15):
		print("(" + stream + " #x" + str(hex(i)[2:]) + ")", end = "")
	print("))")
