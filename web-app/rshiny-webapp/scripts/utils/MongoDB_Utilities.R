options(mongodb = list(
  "host" = "xxxx.xxx.com:55958",
  "username" = "xxx",
  "password" = "xxxx"
))


databaseName <- "mongodb-test"
collectionName <- "test1"

saveData_MongoDB <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
  data <- as.data.frame(t(data))
  db$insert(data)
}

loadData_MongoDB <- function(collection_) {
  # Connect to the database
  db <- mongo(collection = collection_,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find()
  data
}

