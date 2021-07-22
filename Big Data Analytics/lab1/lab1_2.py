#exe2
from pyspark import SparkContext
sc = SparkContext(appName = "exercise 2")
Temp=sc.textFile("BDA/input/temperature-readings.csv")
lines = Temp.map(lambda line: line.split(";"))
#map(station, year, month, temperature)
year_temperature = lines.map(lambda x: (x[0], x[1][0:4], x[1][5:7], float(x[3])))

#filter 1950<year<2014 and temperature>10
filter_year = year_temperature.filter(lambda x: int(x[1])>=1950 and int(x[1])<=2014 and float(x[3])>10)
# map((year, month),1 )
filter_month = filter_year.map(lambda x: (( int(x[1]) , int(x[2])), 1))
# map((station, year, month),1) and return distint elements
filter_month_distinct = filter_year.map(lambda x: ((int(x[0]), int(x[1]), int(x[2])), 1)).distinct()
# Add a 1 to the value of each data point which have a temperature above 10 (distinct)
filter_month_distinct = filter_month_distinct.map(lambda x: ((x[0][1], x[0][2]), 1))
# Sum over all 1:s to count all instances
count_filter_month = filter_month.reduceByKey(lambda a,b: a+b)
count_filter_month_distinct = filter_month_distinct.reduceByKey(lambda a,b: a+b)
count_filter_month.saveAsTextFile("BDA/output/month_count")
count_filter_month_distinct.saveAsTextFile("BDA/output/month_count_distinct")
