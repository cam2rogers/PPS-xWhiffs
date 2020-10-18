# Scraping Statcast Data, 2015-2019

from baseball_scraper import statcast
import pandas as pd


sc_2019 = statcast(start_dt='2019-03-20', end_dt='2019-09-29')

df = pd.DataFrame(sc_2019)

df.to_csv("Statcast2019.csv", index = False, header=True)


sc_2018 = statcast(start_dt='2018-03-29', end_dt='2018-10-01')

df = pd.DataFrame(sc_2018)

df.to_csv("Statcast2018.csv", index = False, header=True)


sc_2017 = statcast(start_dt='2017-04-02', end_dt='2017-10-02')

df = pd.DataFrame(sc_2017)

df.to_csv("Statcast2017.csv", index = False, header=True)


sc_2016 = statcast(start_dt='2016-04-03', end_dt='2016-10-02')

df = pd.DataFrame(sc_2016)

df.to_csv("Statcast2016.csv", index = False, header=True)


sc_2015 = statcast(start_dt='2015-04-05', end_dt='2015-10-04')

df = pd.DataFrame(sc_2015)

df.to_csv("Statcast2015.csv", index = False, header=True)

