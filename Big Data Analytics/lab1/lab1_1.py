#exe1
from pyspark import SparkContext
sc = SparkContext(appName = "exe 1")
Temp=sc.textFile("BDA/input/temperature-readings.csv")
# Definition for calculating the maximum value
def max_temp(a,b):
    if a>=b:
        return a
    else:
        return b
# Definition for calculating the minimum value
def min_temp(a,b):
    if a<=b:
        return a
    else:
        return b

# Split features of the file separated by a ';'
lines = Temp.map(lambda line: line.split(";"))
# Map (year, temperature)
year_temperature = lines.map(lambda x: (x[1][0:4], float(x[3])))
# Filter the data
filter_year_temperature = year_temperature.filter(lambda x: int(x[0])>=1950 and int(x[0])<=2014)
# Find max and min temperature (each year)
max_temp_year = filter_year_temperature.reduceByKey(max_temp)
min_temp_year = filter_year_temperature.reduceByKey(min_temp)
# Sort the result by descending temperature
Max_temp_sorted = max_temp_year.sortBy(ascending = False, keyfunc=lambda k: k[1])
Min_temp_sorted = min_temp_year.sortBy(ascending = False, keyfunc=lambda k: k[1])

Max_temp_sorted.saveAsTextFile("BDA/output/max_temperature")
Min_temp_sorted.saveAsTextFile("BDA/output/min_temperature")
