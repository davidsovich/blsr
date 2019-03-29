
#install.packages("sparklyr")

#library(sparklyr)
#spark_install(version = "2.1.0")

#install.packages(c("nycflights13", "Lahman"))
#library(dplyr)
#iris_tbl <- copy_to(sc, iris)
#flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
#batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
#src_tbls(sc)
#a = ml_decision_tree(iris_tbl,
#                 response = "Species",
#features = c("Petal_Length" , "Petal_Width"))
#alpha = predict(a, iris_tbl)

#dbGetQuery(sc, "SELECT COUNT(*) FROM iris")


#a = readr::read_csv(file="./data-raw/seasonal_adjustment_definitions.csv")

#data_feed_local = spark_read_csv(sc, name="data_feed",
                                 #path="C:/Users/dsovich/Dropbox/Programming/Packages/blsr/data-raw/seasonal_adjustment_definitions.csv", header=TRUE, delimiter=",")
