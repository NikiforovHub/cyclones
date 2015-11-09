
  fileConn <- file("track_log.csv")
  write.table(format(m, digits=2), file = "track_log.csv", 
              append = TRUE, sep='\t',quote = FALSE)
  a = " "
  write.table(a, file = "track_log.csv", 
              append = TRUE, sep = " ", quote = FALSE)
  close(fileConn)
