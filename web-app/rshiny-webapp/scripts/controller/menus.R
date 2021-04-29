output$messageMenu <- renderMenu({
  if (USER$Logged == TRUE) {
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    messageData = data.frame(from = "Admin John", message = "Warnning! Motor 90L88 requires maintenance!")
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "messages", .list = msgs)
  }
})

output$notificationsMenu <- renderMenu({
  if (USER$Logged == TRUE) {
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    messageData = data.frame(
      text = c("Motor 90L88"),
      status = c("warning")
    )
    msgs <- apply(messageData, 1, function(row) {
      notificationItem(text = row[["text"]], status = row[["status"]])
    })
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = msgs)
  }
})
