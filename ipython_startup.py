from __future__ import division
from __future__ import absolute_import

import scipy as sp
import itertools as it
import functools as ft
import operator as op
import sys
import sympy


# Plotting
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.pyplot import subplots
from matplotlib.pyplot import show as pltshow


# and import some common functions into the global namespace
from scipy.linalg import norm
from scipy import sin, cos, tan, log, pi, sqrt, exp, mean
from math import atan2, acos
from sympy import Rational as sRat
from sympy import pretty as spretty
