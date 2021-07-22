from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext(appName = "exe 2")
sqlContext = SQLContext(sc)
Temp=sc.textFile("BDA/input/temperature-readings.csv")
# Split  by a ';'
lines = Temp.map(lambda line: line.split(";"))
tempReadings = lines.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))

tempReadingsString = ["station", "date", "year", "month", "time", "value","quality"]
schemaTempReadings = sqlContext.createDataFrame(tempReadings,tempReadingsString)
schemaTempReadings.registerTempTable("tempReadingsTable")
filtered_years = schemaTempReadings.where('year >= 1950 and year <= 2014 and value > 10')
month_over_10 = filtered_years.groupBy(['year', 'month']).agg(F.count('value').alias('value'))
month_over_10_distinct = filtered_years.groupBy(['year', 'month', 'station']).agg(F.countDistinct('year', 'month','station').alias('value'))
month_10_distinct = month_over_10_distinct.groupBy(['year', 'month']).agg(F.count('value').alias('value'))
sorted_over_10 = month_over_10.orderBy('value', ascending=0).rdd.saveAsTextFile("BDA/output/month_10")
sorted_distinct_over_10 = month_10_distinct.orderBy('value', ascending=0).rdd.saveAsTextFile("BDA/output/distinct_over_10")
