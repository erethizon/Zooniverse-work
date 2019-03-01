###Information from Ivan for trying to parse the Subjects column
#jjj <- purrr::map(erika$subject_data, jsonlite::fromJSON, flatten = T)
#sapply(jjj, function(x) x[[1]]$Habitat, simplify = T) # Gives just the 75 habitats

library(jsonlite)
library(purrr)
library(readr)

DF<- read_csv("test-workflow-classifications.csv")
subjects<-purrr::map(DF$subject_data, jsonlite::fromJSON, flatten = T)
#this brought in the subject_data as a list of 75 items
habitat<-sapply(subjects, function(x)x[[1]]$Habitat, simplify = T)

#HOORAY THIS WORKED and should give me an idea of how to tap into other info in the subjects and/or metadata columns.

