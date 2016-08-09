# Find simplified formula for bank balance sheet change as a result of
# ELA collateral seizure
# Christopher Gandrud
# MIT License
################################################################################

# Import SymPy
import sympy as sp
from sympy.abc import eta, gamma

# Define symbols (eta and gamma imported)
A, D = sp.symbols('A A_d')

#######
# Expanded form expression for non-performing loans
expr_npl1 = (gamma * A) / A
expr_npl2 = (gamma * A - eta * gamma * D)/ (A - D)

# Subtract and print to LaTeX`
print(sp.latex((expr_npl1 - expr_npl2)))
