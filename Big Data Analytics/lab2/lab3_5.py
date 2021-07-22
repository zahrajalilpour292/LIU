from pyspark import SparkContext
from pyspark.sql import SQLContext, Row
from pyspark.sql import functions as F

sc = SparkContext(appName = "exercise 5")
sqlContext = SQLContext(sc)

Precipitation=sc.textFile("BDA/input/precipitation-readings.csv")
Station=sc.textFile("BDA/input/stations-Ostergotland.csv")

lines_precipitation = Precipitation.map(lambda line: line.split(";"))
lines_stations = Station.map(lambda line: line.split(";"))
precReadings = lines_precipitation.map(lambda p: (p[0], p[1], int(p[1].split("-")[0]),
int(p[1].split("-")[1]), p[2], float(p[3]), p[4] ))
stationReadings = lines_stations.map(lambda x: (x[0], x[1]))

stationReadingsString = ["station", "name"]
precReadingsString = ["station", "date", "year", "month", "time", "prec", "quality"]

schemaStationReadings = sqlContext.createDataFrame(stationReadings, stationReadingsString)
schemaPrecReadings = sqlContext.createDataFrame(precReadings, precReadingsString)

schemaStationReadings.registerTempTable("stationReadingsTable")
schemaPrecReadings.registerTempTable("precReadingsTable")

prec_province = schemaPrecReadings.join(schemaStationReadings, 'station').where("year>=1993 and year<=2016")
monthly_prec = prec_province.groupBy(['station', 'year', 'month']).agg(F.sum('prec').alias('prec'))
prec_month_avg = monthly_prec.groupBy(['year', 'month']).agg(F.avg('prec').alias('avgMonthlyPrec'))
prec_month_avg.orderBy(['year', 'month'], ascending=[0,0]).rdd.saveAsTextFile("BDA/output/average_,onthly_precipitation")
