from __future__ import division
from __future__ import absolute_import

import scipy as sp
import itertools as it
import functools as ft
import operator as op
import sys
# import sympy
import os

# Plotting
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.pyplot import subplots
from matplotlib.pyplot import show as pltshow


# and import some common functions into the global namespace
from scipy.linalg import norm, eig
from scipy import sin, cos, tan, log, pi, sqrt, exp, mean
from math import atan2, acos
# from sympy import Rational as rat
# from sympy import pretty as spretty

import oomphpy
import oomphpy.micromagnetics as mm
import oomphpy.tests
import oomphpy.matrices
from oomphpy.matrices import ascii2coo
from oomphpy.matrices import ascii2array
