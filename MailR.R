library(mailR)
sender <- "b.park.kw@juntendo.ac.jp"
recipients <- c("clista77@gmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "Test mail from R",
          body = "Test email body",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "b.park.kw@juntendo.ac.jp",            
                      passwd = "Ujiheart1023", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)