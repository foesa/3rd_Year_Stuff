from decimal import Decimal

excercise = [[[0.891,8],[0.009,8],[0.1,0]], [[0.18,0],[0.72,0],[0.1,0]], [[0,0],[0,0],[1,0]]]
relax = [[[0.693,10],[0.297,10],[0.01,0]], [[0,5],[0.99,5],[0.01,0]], [[0,0],[0,0],[1,0]]]

iterations = int(input("Please enter number of iterations: "))
disc_val = float(input("Please enter discounted value (0 < y < 1): "))
cur_state = input("Please enter state: ")

if cur_state == "fit" or cur_state == "Fit":
	cur_state = 0
elif cur_state == "unfit" or cur_state == "Unfit":
	cur_state = 1
else:
	cur_state = 2

def q0(s,a):
	if a == "excercise":
		part1 = excercise[s][0][0] * excercise[s][0][1]
		part2 = excercise[s][1][0] * excercise[s][1][1]
		part3 = part1 + part2
	else:
		part1 = relax[s][0][0] * relax[s][0][1]
		part2 = relax[s][1][0] * relax[s][1][1]
		part3 = part1 + part2
	return part3


def Vn(s,n):
	temp1 = qn(s, "excercise", n)
	temp2 = qn(s, "relax", n)
	return max(temp1, temp2)


def qn(s,a,n):

	if n == 0:
		return q0(s,a)

	temp = q0(s,a)

	if a == "excercise":
		part1 = excercise[s][0][0] * Vn(0, n-1)
		part2 = excercise[s][1][0] * Vn(1, n-1)
		part3 = disc_val * (part1 + part2)

	else:
		part1 = relax[s][0][0] * Vn(0, n-1)
		part2 = relax[s][1][0] * Vn(1, n-1)
		part3 = disc_val * (part1 + part2)

	part4 = temp + part3
	return part4

	
print("Excercise: " + str(qn(cur_state, "excercise", iterations)))
print("Relax: " + str(qn(cur_state, "relax", iterations)))
