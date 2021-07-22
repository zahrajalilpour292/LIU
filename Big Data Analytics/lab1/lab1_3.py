#exe3
from pyspark import SparkContext
sc = SparkContext(appName = "exercise 3")
Temp=sc.textFile("BDA/input/temperature-readings.csv")

lines = Temp.map(lambda line: line.split(";"))
# map(year, month, day, station, temperature)
year_temperature = lines.map(lambda x: (x[1][0:4], x[1][5:7], x[1][8:10], x[0], float(x[3])))
# filter 1960 < year < 2014
year_temperature = year_temperature.filter(lambda x: int(x[0])>=1960 and int(x[0])<=2014)
# map((year, month, day , station), temperature)
temp_day = year_temperature.map(lambda x: ((int(x[0]), int(x[1]), int(x[2]), int(x[3])), x[4]))
# Finding the max temperature for each station in a day
temp_max = temp_day.reduceByKey(max)
# Finding the min temperature for each station in a day
temp_min = temp_day.reduceByKey(min)
# create a value pair containing the max and min temperature 
temp_day_join = temp_max.join(temp_min)
# map((year, month, station),(temp_average, 1))
temp_month = temp_day_join.map(lambda x: ((x[0][0], x[0][1], x[0][3]), ((x[1][0]+x[1][1])/2, 1)))
# Summing all the measured daily averages and counting the number of days per month
average_temp_month = temp_month.reduceByKey(lambda a,b: (a[0]+b[0], a[1]+b[1]))
# Calculating the monthly averages
average_monthly_temperature = average_temp_month.mapValues(lambda x: x[0]/x[1])
#average_monthly_temperature.collect()
average_monthly_temperature.saveAsTextFile("BDA/output/average_monthly_temperature")
