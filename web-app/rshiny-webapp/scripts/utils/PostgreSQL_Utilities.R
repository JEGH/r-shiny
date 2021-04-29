#____________________________________________________________________
#conect Database----
#Function that creates a connection with DB
#input variables:
#     db_name: DB name
#     db_host: url of postgres server
#     db_port: port of postgres server
#     db_user: name of DB user
#     db_pass: password for db_user
#returns:
#     variable with connection to DB
#____________________________________________________________________
connect_DataBase <-
  function(db_name = 'xxxxxx',
           db_host = "xx.xx.x.xxx",
           db_port = 5432,
           db_user = "xxx",
           db_password = "xxx") {

    # create a connection
    # save the password that we can "hide" it as best as we can by collapsing it
    pw <- {
      "postgres" #"postgres123"
    }
    
    # loads the PostgreSQL driver
    drv <- dbDriver(drvName = "PostgreSQL")
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    con <- dbConnect(
      drv,
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = db_user,
      password = db_password
    )
    rm(pw) # removes the password
    con
  }
#_______________________________________________________________
#insert_table_bd----
# Insert dataframe in DB rearm4thewin
# input variables:
#       table_name: dataframe name string
#       df_: variable with actual dataframe
# output/return:
#       res: variable with result from DB
#_______________________________________________________________
insert_table_bd <- function(table_name, df_) {
  con_ <-  connect_DataBase()
  res <- dbWriteTable(
    con_,
    table_name,
    value = df_,
    append = TRUE,
    row.names = FALSE
  )
  
  if (dbDisconnect(con_))
    print("close connection to Database")
  res
}
#________________________________________________
#get_query_bd ----
#Send query to DB rearm4thewin
#input variables:
#       query_: string with query
# output:
#       table from DB
#________________________________________________
get_query_bd <- function(query_) {
  con_ <-  connect_DataBase()
  res  <-  dbGetQuery(con_, query_)
  if (dbDisconnect(con_))
    print("close connection")
  res
}
#________________________________________________
#send_query_bd ----
#Send query to DB rearm4thewin, used for queries that dont return tables
#input variables:
#       query_: string with query
# output:
#       anwser of the DB server
#________________________________________________
send_query_bd <- function(query_) {
  con_ <-  connect_DataBase()
  res  <-  dbSendQuery(con_, query_)
  # commit the change
  res = dbCommit(con_)
  
  if (dbDisconnect(con_))
    print("close connection")
  res
}


#_________________________________________________________
#disable_html_elements-----
# enable or disable html tags with some id
# input:
#     dis: boolean variable that determines whether to enable or disable elements
#     elements: is a string array with the id of the html tags we want to change
#__________________________________________________________
disable_html_elements <- function(dis, elements) {
  if (dis) {
    for (i in 1:length(elements)) {
      shinyjs::disable(elements[i])
    }
  }
  else{
    for (i in 1:length(elements)) {
      shinyjs::enable(elements[i])
    }
  }
}
#____________________________________________________________________
#all_elements_not_empty-----
# Check in all elements in an array are null or na or ""
# inputs:
#       elements: array with variables we want to check if are empty or not
# output:
#       return true if all elements are different than null, na or ""
#       return false otherwise
#______________________________________________________________________
all_elements_not_empty <- function(elements) {
  count <- 0
  for (i in 1:length(elements)) {

    if (is.na(elements[i]) == TRUE || is.null(elements[i]) == TRUE) {
      print('e NA ou null')
    }
    else{
      if (class(elements[i]) == "character") {
        if (elements[i] != "") {
          count <- count + 1
        }
      }
      if (class(elements[i]) == "numeric") {

        if (!is.na(elements[i] == TRUE)) {
          count <- count + 1
        }
      }
    }
    
  }
  #if all elements are not null
  if (count == length(elements)) {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
#____________________________________________________________________________
#parse_remove_motor_input----
#Gets motor id from string like: id:2 ;model: "SRCIM"
#input:
#     received_input: string like explained previously
#output:
#     motor id
#___________________________________________________________________________
parse_remove_motor_input <- function(received_input) {
  bs <- strsplit(received_input, split = ':')[[1]][2]
  motor_id <- as.integer(strsplit(bs, split = ';')[[1]][1])
  return(motor_id)
}
#_______________________________________________________________
# get_room_ids_and_names-------
# returns array with strings like: room_id -> room_name 
# this functions gets from DB all room names and ids 
# and creates an array with strings like previously mentioned     
#_____________________________________________________________
get_room_ids_and_names <- function() {
  rooms_names <-
    get_query_bd("SELECT \"Name\",\"ID_Room\" from \"Rooms\"  ")
  rooms_id_names <- c()
  for (i in 1:length(rooms_names[['Name']])) {
    final_str <-
      concate_n_strings(c(rooms_names[['ID_Room']][i], '->', rooms_names[['Name']][i]))
    rooms_id_names <- c(rooms_id_names, final_str)
  }
  return(rooms_id_names)
}
# ______________________________________________________________________________
#get_motors_id_and_model---------
#gets array with all motors id's and models from the DB
#input: name of DB
# output: array with motor ids and models in strings
#__________________________________________________________________________________
get_motors_id_and_model <- function(db_name = "rearm4thewin") {
  if (db_name == "rearm4thewin") {
    motors_id_model <-
      get_query_bd("SELECT \"ID_Motor\",\"Model\" from \"Motors\"  ")
  } else if (db_name == "rearm-db") {
    motors_id_model <-
      Query_REARM_DB("SELECT \"ID_Motor\",\"Model\" from \"App\".\"Motors\"  ")
  } else{
    print("DB not created!")
    return()
    
  }
  output <- c()
  for (i in 1:length(motors_id_model[['ID_Motor']])) {
    f_str <-
      concate_n_strings(c('ID: ', motors_id_model[['ID_Motor']][i], ' ; ', 'Model: ', motors_id_model[['Model']][i]))
    output <- c(output, f_str)
  }
  return(output)
}


#___________________________________________________________________________
#select_companies------
# returns array with all companies ids and respective names in strings, like:
# c("1->Altran","2->OptiSigma",...)
#___________________________________________________________________________
select_companies <- function() {
  companies <- get_query_bd("SELECT * from \"Company\"  ")
  companies_id_names <- c()
  for (i in 1:length(companies[['Name']])) {
    final_str <-concate_n_strings(c(companies[['ID_Company']][i], '->', companies[['Name']][i]))
    companies_id_names <- c(companies_id_names, final_str)
  }
  return(companies_id_names)
}

#________________________________________________________________________________
#select_usernames-----
# returns dataframe with all users in DB
#
#________________________________________________________________________________
select_usernames <- function() {
  usernames <- get_query_bd("SELECT \"Name\" from \"Users\" ")
  return(usernames)
}
#________________________________________________________________________________
#select_usernames-----
# returns array with user types and respective description, like: 
# c("1->Admin","2->Developer","...")
#
#________________________________________________________________________________
select_type_of_users <- function() {
  types <-get_query_bd("SELECT \"ID_UserType\",\"Description\" from \"UserType\" ")
  types_id_des <- c()
  for (i in 1:length(types[['ID_UserType']])) {
    final_str <-concate_n_strings(c(types[['ID_UserType']][i], '->', types[['Description']][i]))
    types_id_des <- c(types_id_des, final_str)
  }
  return(types_id_des)
}

#__________________________________________________________________________________________________
#user_already_exists--------
#input: string of username to check if exists or not
# output:
#     returns boolean if user already exists or not
#_______________________________________________________________________________________________
user_already_exists <- function(username) {
  output <-get_query_bd(paste0("select \"Name\" from \"Users\" where \"Name\"='",username,"' "))
  
  if (is.null(output[['Name']])) {
    return(FALSE)
  }
  if (output[['Name']] != "") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
  
}
#_______________________________________________________________________________
#PrintByParts------
# from a dataframe prints iteratively n_rows
#input:
#     df: dataframe to print
#     rows_per_turn: number_of rows to print per iteration
#
#     
#_______________________
PrintByParts <- function(df, rows_per_turn = 50) {
  nrows <- length(df[, 1])
  n_iterations <- round(nrows / rows_per_turn, digits = 0)
  n <- n_iterations
  withProgress(message = 'Printing Data', value = 0, {
    for (i in 1:n_iterations) {
      start_index <- (i - 1) * rows_per_turn + 1
      final_index <- rows_per_turn * i

      print(df[start_index:final_index, ])

      # Increment the progress bar, and update the detail text.
      incProgress(1 / n,
                  detail = paste0("Printing: ", i, " from ", n_iterations, " parts."))
    }
  })
}

#__________________________________________________________________________
#Query_REARM_DB------
# send query to REARM_DB and returns what postgres sever returns from query
# input:
#     query_:string with query
# output:
#     returns what postgres server returns from query
#_________________________________________________________
Query_REARM_DB <-
  function(query_ = 'select * from "Data"."Raw" limit 10000') {
    con_ <-
      connect_DataBase(
        db_name = 'REARM_DB',
        db_host = "10.12.2.162",
        db_port = 5432,
        db_user = "postgres",
        db_password = "postgres"
      )

    res  <-  dbGetQuery(con_, query_)
    if (dbDisconnect(con_)) {
      print("close connection")
    }
    
    if (!is.null(res)) {
      return(res)
    }
    
  }
#____________________________________________________________________
#CompareDataFrames--------
#input:
#     two dataframes
#output:
#     returns if both dataframes are equal (TRUE) or not (FALSE)
#___________________________________________________________________
CompareDataFrames <- function(df1, df2) {
  comparison <- compare(df1, df2, allowAll = TRUE)
  return(comparison)
}
