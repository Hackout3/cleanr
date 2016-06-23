d <- read.csv("countries.csv", stringsAsFactors=FALSE)
rules <- recode_read("countries.yml")
res <- recode(d, rules)
