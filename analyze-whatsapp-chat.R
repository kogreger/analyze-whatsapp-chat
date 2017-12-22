## load necessary libraries
library(dplyr)
library(readr)

## function to parse the information from WhatsApp chat export 
## (cf. https://stat.ethz.ch/R-manual/R-devel/library/base/html/grep.html)
parse.whatsapp <- function(res, 
                           result) {
  m <- do.call(rbind, 
               lapply(seq_along(res), 
                      function(i) {
                        if(result[i] == -1) return("")
                        st <- attr(result, 
                                   "capture.start")[i, ]
                        substring(res[i], 
                                  st, 
                                  st + attr(result, 
                                            "capture.length")[i, ] - 1)
                      }))
  colnames(m) <- attr(result, 
                      "capture.names")
  m
}



## load and parse WhatsApp chat export text file
whatsapp <- read_file("C:/Users/kgreger/Downloads/WhatsApp Chat - Test/_chat.txt") %>% 
  strsplit("\r\n") %>% 
  unlist() %>% 
  gsub("\\n", 
       "", 
       .) %>% 
  parse.whatsapp(regexpr("(?<date>\\d{2}\\.\\d{2}\\.\\d{2}), (?<time>\\d{2}:\\d{2}:\\d{2}): (?<sender>.+?): (?<text>.*)", 
                         ., 
                         perl = TRUE)) %>% 
  as_tibble() %>% 
  mutate(timestamp = as.POSIXct(strptime(paste(date, time), 
                                         format ="%d.%m.%y %H:%M:%S"))) %>% 
  select(timestamp, sender, text) %>% 
  filter(!is.na(timestamp))


## export line-by-line version of WhatsApp chat
write_csv(whatsapp, 
          "C:/Users/kgreger/Downloads/WhatsApp Chat - Test/chat.csv")