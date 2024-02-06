import pandas as pd
import json

f = open('../data/d_cases_short.json', 'r')
data = json.load(f)
type(data)

df = pd.DataFrame(data)
df.columns = ['id', 'date', 'county', 'cases']
df
df.to_csv('../data/cases_daily.csv', index=False)