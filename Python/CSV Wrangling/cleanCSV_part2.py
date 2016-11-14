from __future__ import division
import pandas as pd
import numpy as np
import random
from difflib import SequenceMatcher

df = pd.read_csv("C:\Users\dolejar\Documents\Incident Analysis\original_incident_file.csv", low_memory = False)