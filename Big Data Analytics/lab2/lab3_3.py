from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext(appName = "exe 3")
sqlContext = SQLContext(sc)
Temp=sc.textFile("BDA/input/temperature-readings.csv")
lines = Temp.map(lambda line: line.split(";"))
tempReadings = lines.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))

tempReadingsString = ["station", "date", "year", "month", "time", "value", "quality"]
schemaTempReadings = sqlContext.createDataFrame(tempReadings, tempReadingsString)
schemaTempReadings.registerTempTable("tempReadingsTable")
filtered_years = schemaTempReadings.where('year >= 1960 and year <= 2014')
temp_max = filtered_years.groupBy(['year', 'month', 'station', 'date']).agg(F.max('value').alias('max'))
temp_min = filtered_years.groupBy(['year', 'month', 'station', 'date']).agg(F.min('value').alias('min'))
temp_max_min = temp_max.join(temp_min, ['year', 'month', 'station', 'date'])
temp_max_min = temp_max_min.select('year', 'month', 'station', 'date', ((temp_max_min.max+temp_max_min.min)/2).alias('maxmin'))
ave_monthly_temp = temp_max_min.groupBy(['year', 'month', 'station']).agg(F.avg('maxmin').alias('avgMonthlyTemperature'))
ave_monthly_temp.orderBy('avgMonthlyTemperature', ascending=0).rdd.saveAsTextFile("BDA/output/ave_monthly_temp")
