# CustDist UX check
source("customDist.R")

kk = customDistParser(file = "UX_custDist_Input.txt")

# Now, let's break the function

# Case 0: Correct
case0 = customDistParser(file = "test.custDist.txt")

# Case I: Misspell: Distrabution...
debug(customDistParser)
case1 = customDistParser(file = "UX_custDist_Input.txt") # String Split Error

# Case II (A): No Distribution
case2.a = customDistParser(file = "UX_custDist_Input.txt")
# Same Issue as Case I

# Case II (B): No Function
case2.b = customDistParser(file = "UX_custDist_Input.txt")
# Same Issue as Case I


# Issue I: If distribution or function option does not exist, it will return error
# Resolved


# Case III(a): Lacking values in CustDist parameters's censor default
debug(customDistParser)
case3.a = customDistParser(file = "UX_custDist_Input.txt")
