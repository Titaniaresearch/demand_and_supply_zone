current_time = Sys.time()
print(current_time)
msg <- glue::glue("This is test message")
cat(msg,file="test.txt")