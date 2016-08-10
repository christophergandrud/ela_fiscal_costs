# Find simplified formula for bank balance sheet change as a result of
# ELA collateral seizure
# Christopher Gandrud
# MIT License
################################################################################

# Import SymPy
import sympy as sp
from sympy.abc import eta, gamma

# Define symbols (eta and gamma imported)
A, AD = sp.symbols('A A_d')

print(sp.latex(lambdaA))

#######
# Expanded form expression for the change in non-performing loans after ELA
# collateral seizure
expr_npl1 = (gamma * A) / A
expr_npl2 = (gamma * A - eta * gamma * AD)/ (A - AD)

# Subtract and print simplified form as LaTeX
print(sp.latex((expr_npl1 - expr_npl2)))
