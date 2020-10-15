#
#Author: César Garro-Marín
#Date: October 3rd, 19
#Scans pdf files from professor evaluations

#output: finalDataV2

#loading libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(tesseract)
library(sjmisc)
library(eeptools)
library(taRifx)
library(memisc)

#I set the working directory
setwd("C:/Users/thecs/Dropbox/boston_university/10-DeptRA/evaluations")

eng <- tesseract("eng")


file <-         "./input/springEvaluations.pdf"

#Converting the files to 

ver <- pdftools::pdf_text(file)
page_lines <- strsplit(ver[1],"\n")[[1]]
page_lines <- strsplit(ver[1],"\n")[[1]]

tesseract::ocr(file,engine=eng)

total_page <- 674

#Preparing output database
database <- as.data.frame(1:total_page)
colnames(database) <- c("page")

database <- database %>%
  add_column(class="") %>%
  add_column(name="") %>%
  add_column(last_name="") %>%
  add_column(respondents="") %>%
  add_column(prof_rating="") %>%
  add_column(course_rating="") %>%
  add_column(concepts="") %>% 
  add_column(interest="") %>% 
  add_column(participation="") %>%
  add_column(grading="") %>%
  add_column(assignments="") %>%
  add_column(feedback="") %>%
  add_column(availability="")%>%
  add_column(enrolles="")%>%
  add_column(section="")


pdf_text <- pdftools::pdf_text(file)



for (page in 1:9) {
    progress_line <- paste("Scanning page ",page," out of ",total_page,"\n",sep="")
    cat(progress_line)
 
    
    page_text <- pdf_text[page]%>%
      tolower()
    page_lines <- strsplit(page_text,"\r\n")[[1]]
    
    
    #Extracting class name
    class_pos <- match(TRUE,!is.na(str_locate(page_lines,"ec")))
    class_line<- str_split(trimws(page_lines[class_pos]),"[ ]+")[[1]]
    class <- paste(class_line[1],class_line[2],sep="")

    
    
    #Extracting professor name
    name_pos <- match(TRUE,!is.na(str_locate(page_lines,"spring20")))
    name_line<- str_split(trimws(page_lines[name_pos]),"[ ]+")[[1]]
    
    page_name <-  paste("./input/springEvaluations_",page,".png",sep="")
    alternate_page_text <-   tesseract::ocr(page_name,engine=eng)%>%
      tolower()
    
    alternate_page_lines <- strsplit(alternate_page_text,"\n")[[1]]
    
    alt_class_pos <- match(TRUE,!is.na(str_locate(alternate_page_lines,"ec")))
    alt_class_line<- str_split(trimws(alternate_page_lines[alt_class_pos]),"[ ]+")[[1]]
    section <- alt_class_line[3]
    
    if(!is.na(name_pos)){  
      database$class[page] <- class
      database$name[page] <- name_line[3]
      database$last_name[page] <- name_line[4]
      database$section[page] <- gsub("[[:punct:]]","",section)
      
      #Extraction students enrolled
      enrollement_pos <- match(TRUE,!is.na(str_locate(page_lines,"enrol")))
      enrollment_line<- str_split(trimws(page_lines[enrollement_pos]),"[ ]+")[[1]]
      enrollment_index <- match(TRUE,grepl("[0-9]+",enrollment_line))
      database$enrolles[page]<- enrollement_line[enrollment_index]
      
      #Extraction professor rating
      rating_pos <- match(TRUE,!is.na(str_locate(page_lines,"rating of instructor")))
      rating_line<- str_split(page_lines[rating_pos],"[ ]+")[[1]]
      database$prof_rating[page]<- rating_line[length(rating_line)]
      
      course_rating_pos <- match(TRUE,!is.na(str_locate(page_lines,"course rating")))
      course_rating_line<- str_split(page_lines[course_rating_pos],"[ ]+")[[1]]
      database$course_rating[page]<- course_rating_line[length(course_rating_line)]
      database$respondents[page]<- course_rating_line[length(course_rating_line)-3]
      
      concepts_pos <- match(TRUE,!is.na(str_locate(page_lines,"concepts")))
      concepts_line<- str_split(page_lines[concepts_pos],"[ ]+")[[1]]
      database$concepts[page]<- concepts_line[length(concepts_line)]
      
      
      interest_pos <- match(TRUE,!is.na(str_locate(page_lines,"subject")))
      interest_line<- str_split(page_lines[interest_pos],"[ ]+")[[1]]
      database$interest[page]<- interest_line[length(interest_line)]
      
      participation_pos <- match(TRUE,!is.na(str_locate(page_lines,"class participation")))
      participation_line<- str_split(page_lines[participation_pos],"[ ]+")[[1]]
      database$participation[page]<- participation_line[length(participation_line)]
      
      assignments_pos <- match(TRUE,!is.na(str_locate(page_lines,"returning assignments")))
      assignments_line<- str_split(page_lines[assignments_pos],"[ ]+")[[1]]
      database$assignments[page]<- assignments_line[length(assignments_line)]
      
      
      grading_pos <-match(TRUE,!is.na(str_locate(page_lines,"in grading")))
      grading_line<- str_split(page_lines[grading_pos],"[ ]+")[[1]]
      database$grading[page]<- grading_line[length(grading_line)]
      
      feedback_pos <- match(TRUE,!is.na(str_locate(page_lines,"feedback to students")))
      feedback_line<- str_split(page_lines[feedback_pos],"[ ]+")[[1]]
      database$feedback[page]<- feedback_line[length(feedback_line)]
      
      availability_pos <- match(TRUE,!is.na(str_locate(page_lines,"outside|outslde")))
      availability_line<- str_split(page_lines[availability_pos],"[ ]+")[[1]]
      database$availability[page]<- availability_line[length(availability_line)]
    }
}

print("Scanning has finished")

output_database <- database%>%
  filter(class!="")

output_database[,2:4] <- output_database[,2:4]%>%
  mutate_all(toupper)

output_database[,6:14] <- output_database[,6:14]%>%
  mutate_all(destring)

output_database$respondents <-as.numeric(output_database$respondents)

output_database <- output_database%>%
  mutate(complete=complete.cases(.))

output_database$respondents <- ifelse(output_database$respondents==0,NA,output_database$respondents)

output_database[,6:14] <- output_database[,6:14]%>%
  mutate_all(funs(ifelse(.>100,./100,.)))%>%
  mutate_all(funs(ifelse(.>10,./10,.)))


#Prompting for missing data
incomplete <- which(is.na(output_database),arr.ind = TRUE)

return_prompt <- function(column, page){
  suppressWarnings(
    prompt_b <- cases(
      "number of respondents"=column==5,
      "professor rating"=column==6,
      "course rating"=column==7,
      "effectiveness in explaining concepts rating"=column==8,
      "stimulate interest rating"=column==9,
      "class partipation rating"=column==10,
      "fairness in grading rating"=column==11,
      "returning assignments rating"=column==12,
      "quality of feedback rating"=column==13,
      "availability outside class rating"=column==14
    )
  )
  prompt <- paste("Please input the", prompt_b,"in page",page,">",sep=" ")
  return(prompt)
} 

n_incomplete <- nrow(incomplete)
for (row in 1:n_incomplete){
  line <- incomplete[row,1]
  column <- incomplete[row,2]
  page <- output_database$page[line]
  prompt <- return_prompt(column,page)
  new_data <- destring(readline(prompt))
  
  output_database[line,column] <- new_data
}


n_incomplete <- nrow(incomplete)
for (row in 1:n_incomplete){
  line <- incomplete[row,1]
  column <- incomplete[row,2]
  page <- output_database$page[line]
  prompt <- return_prompt(column,page)
  new_data <- destring(readline(prompt))
  
  output_database[line,column] <- new_data
}

mistake_matrix <- output_database
mistake_matrix[,] <- FALSE
mistake_matrix[,6:14] <- output_database[,6:14]%>%
  mutate_all(funs(ifelse(.>5,TRUE,FALSE)))


#correcting errors I can catch
errors <- which(mistake_matrix==TRUE,arr.ind = TRUE)



n_errors <- nrow(errors)
for (row in 1:n_errors){
  line <- errors[row,1]
  column <- errors[row,2]
  page <- output_database$page[line]
  prompt <- return_prompt(column,page)
  new_data <- destring(readline(prompt))
  
  output_database[line,column] <- new_data
}

write.csv(output_database,"final_database.csv",row.names = FALSE)

