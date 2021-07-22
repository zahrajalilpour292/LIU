# exe4
from pyspark import SparkContext
sc = SparkContext(appName = "exe 4")
Temp=sc.textFile("BDA/input/temperature-readings.csv")
Precipitation=sc.textFile("BDA/input/precipitation-readings.csv")

# split features of precipitation
lines_precipitation = Precipitation.map(lambda line: line.split(";"))
# map(station, temperature)
temperature = lines.map(lambda x: (x[0], float(x[3])))
# map(station, precipitation)
precipitation = lines_precipitation.map(lambda x: (x[0], float(x[3])))
# Finding the max temp and max precipitation for each station
maximum_temp = temperature.reduceByKey(max)
maximum_precipitation = precipitation.reduceByKey(max)
# Create a value pair containing the max temperature and max precipitation in each station
joined_maximum = maximum_temp.join(maximum_precipitation)
# Filter 25<temp<30 and 100<precipitation<200
final_station = joined_maximum.filter(lambda x: float(x[1][0])>=25 and float(x[1][0])<=30 and float(x[1][1])>=100 and float(x[1][1])<=200)
#final_station.collect()
final_station.saveAsTextFile("BDA/output/final_station")
