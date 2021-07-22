from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext(appName = "exe 1")
sqlContext = SQLContext(sc)
Temp=sc.textFile("BDA/input/temperature-readings.csv")
# Split by a ';'
lines = Temp.map(lambda line: line.split(";"))
# Map key as year and value as temperature
tempReadings = lines.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))

tempReadingsString = ["station", "date", "year", "month", "time", "value","quality"]
schemaTempReadings = sqlContext.createDataFrame(tempReadings,tempReadingsString)
schemaTempReadings.registerTempTable("tempReadingsTable")
filtered_years = schemaTempReadings.where('year >= 1950 and year <= 2014')
max_temp = filtered_years.groupBy('year').agg(F.max('value').alias('value'))
min_temp = filtered_years.groupBy('year').agg(F.min('value').alias('value'))
max_temp = max_temp.join(filtered_years, ['year', 'value']).select('year', 'station', 'value').orderBy(['value'], ascending=[0])
min_temp = min_temp.join(filtered_years, ['year', 'value']).select('year', 'station', 'value').orderBy(['value'], ascending=[0])
max_temp.withColumnRenamed('value', 'yearlyMax').rdd.saveAsTextFile("BDA/output/max_temperature")
min_temp.withColumnRenamed('value', 'yearlyMin').rdd.saveAsTextFile("BDA/output/min_temperature")
