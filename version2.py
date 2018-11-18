#Load packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as stats
from fancyimpute import KNN    
import statsmodels.api as sm
from sklearn.model_selection import train_test_split
from statsmodels.tsa.stattools import adfuller