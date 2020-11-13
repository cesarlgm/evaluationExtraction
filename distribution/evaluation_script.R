#!/usr/bin/env r

if (!require("tidyverse", quietly=TRUE)) install.packages("tidyverse",quiet=TRUE)
if (!require("stringr", quietly=TRUE)) install.packages("stringr",quiet=TRUE)
if (!require("dplyr", quietly=TRUE)) install.packages("dplyr",quiet=TRUE)
if (!require("tesseract", quietly=TRUE))  install.packages("tesseract",quiet=TRUE)
if (!require("sjmisc", quietly=TRUE))   install.packages("sjmisc",quiet=TRUE)
if (!require("eeptools", quietly=TRUE))  install.packages("eeptools",quiet=TRUE)
if (!require("taRifx", quietly=TRUE)) install.packages("taRifx",quiet=TRUE)
if (!require("memisc", quietly=TRUE)) install.packages("memisc",quiet=TRUE)
if (!require("pdftools", quietly=TRUE)) install.packages("pdftools",quiet=TRUE)


#this function prompts for the name of the file to scan
get_file_name <- function(file_name=""){
  file_prompt <- "Please input the name of the pdf file to scan. Do not include the file extension or any quotes in your input: "
  if (file_name=="") {
    file_name <- readline(file_prompt)
  }
  return(file_name)
}


#this function prompts for the semester to scan
get_semester_to_scan <- function(semester=""){ 
  semester_prompt <- "Please input the semester to scan. Make sure not to include any spaces in your input: "
  if (semester=="") {
    semester <- readline(semester_prompt)
  }
  return(tolower(semester))
}

do_empty_database <- function(n_pages){
  #Preparing output database
  database <- as.data.frame(1:n_pages)
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
  return(database)  
}


return_page_name <- function(page,file_name){
  to_return <- paste("to_scan/",file_name,"_",page,".png",sep="")
  return(to_return)
}




extract_data_point <- function(string_to_identify,where_to,pos_to_return=1,number_type="trad"){
  #this function just receives scanned text and returns the desired information based on 
  #the stirng to idenidy and the position to return
  


  #This returns the line number where the information can be found
  data_position <-  match(TRUE,!is.na(str_locate(where_to,string_to_identify)))
  
  #this returns the line th
  data_line <- str_split(trimws(where_to[data_position]),"[ ]+")[[1]]
  
  if (is.na(data_position)){
    return(NA)
  } 
  else {
    if (number_type=="trad"){
      to_return <- data_line[pos_to_return]
      return(to_return)
    }
    else if (number_type=="enrollment") {
      #this outputs enrollment
      corrected_pos <-  match(TRUE,grepl("[0-9]+",data_line))
      to_return <-  data_line[corrected_pos]
    }
    else if (number_type=="end") {
      #this outputs prof rating
      corrected_pos <-  length(data_line)
      to_return <-  data_line[corrected_pos]
    }
    else if (number_type=="respondents") { 
      corrected_pos <-  length(data_line)-3
      to_return <-  data_line[corrected_pos]
    }
  }
}



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


extract_information <- function(convert=TRUE){
  
  file_to_scan <-     get_file_name()
  
  print(paste("Scanning file: ",file_to_scan, ".pdf",sep=""))
  
  semester_to_scan <- get_semester_to_scan()  
  
  #Extracting text from provided pdf
  pdf_text <- pdftools::pdf_text(paste("to_scan/", file_to_scan,".pdf",sep=""))

  print("Converting pdf file to png. This will take a while")

  #converting the pdf file to png
  if (convert) {
    setwd("to_scan/")
    pdftools::pdf_convert(paste(file_to_scan,".pdf",sep=""), format = 'png',  dpi = 600)
    setwd("./..")
  }

  #Defining the number of pages to scan
  total_page <- length(pdf_text)
  
  #First I create the empty database
  database <- do_empty_database(total_page)
  
  print("Starting the extracting of information")

  for (page in 1:total_page){
    print(paste("Extracting information from page",page))
    database <- extract_page_text(page,file_to_scan,semester_to_scan , database, pdf_text)
  }

  print("Scanning has finished. We will start filling up missing values")

  output_database <- clean_output_database(database)

  print("Extraction of the information is finished")
  

  write.csv(output_database,"output/final_database.csv",row.names = FALSE)
}




clean_output_database <- function(database) {
  #filtering empty lines
  output_database <-  database%>%
    filter(class!="")
  
  #making data uppercase
  output_database[,2:4] <- output_database[,2:4]%>%
    mutate_all(toupper)

  #destringing numbers
  output_database[,6:14] <- output_database[,6:14]%>%
    mutate_all(destring)

  #destringing the respondents
  output_database$respondents <-as.numeric(output_database$respondents)

  #dropping enrolles column
  output_database$enrolles <- NULL
  output_database <- output_database%>%
  mutate(complete=complete.cases(.))

  #making zero responses NA
  output_database$respondents <- ifelse(output_database$respondents==0,NA,output_database$respondents)

  output_database[,6:14] <- output_database[,6:14]%>%
    mutate_all(funs(ifelse(.>100,./100,.)))%>%
    mutate_all(funs(ifelse(.>10,./10,.)))

  output_database$section <- output_database$section%>%
    toupper()

  #Prompting for missing data
  incomplete <- which(is.na(output_database),arr.ind = TRUE)
  
  if (TRUE) {
    n_incomplete <- nrow(incomplete)
    if (n_incomplete>=1) {
      for (row in 1:n_incomplete){
        line <- incomplete[row,1]
        column <- incomplete[row,2]
        page <- output_database$page[line]
        prompt <- return_prompt(column,page)
        new_data <- destring(readline(prompt))
        
        output_database[line,column] <- new_data
      }
    }
  
      
    mistake_matrix <- output_database
    mistake_matrix[,] <- FALSE
    mistake_matrix[,6:14] <- output_database[,6:14]%>%
      mutate_all(funs(ifelse(.>5,TRUE,FALSE)))
  
  
    
    #correcting errors I can catch
    errors <- which(mistake_matrix==TRUE,arr.ind = TRUE)
    n_errors <- nrow(errors)
    if (n_errors>=1) {
      for (row in 1:n_errors){
          line <- errors[row,1]
          column <- errors[row,2]
          page <- output_database$page[line]
          prompt <- return_prompt(column,page)
          new_data <- destring(readline(prompt))
          
          output_database[line,column] <- new_data
      }
    }
  }
  return(output_database)
}


extract_page_text <- function(page, file,semester_to_scan, database, pdf_text){
  
  #First I transform all the text into lower caps
  page_text <- pdf_text[page]%>%
    tolower()
  


  #page to scan using tesseract
  eng <- tesseract("eng")
  page_name <- return_page_name(page, file)
  alternate_page_text <-   tesseract::ocr(page_name,engine=eng)%>%
      tolower()
  
  #Then I split the text from the page. Note that the \r\n only appears with the acrobat text
  page_lines <- strsplit(page_text,"\r\n")[[1]]
  alternate_page_lines <- strsplit(alternate_page_text,"\n")[[1]]
    

  #I start by extracting the professor name. Only the pages with the numbers have professor name.
  first_name <- extract_data_point(semester_to_scan,page_lines,3)
  last_name <- extract_data_point(semester_to_scan,page_lines,4)
  

  #so if I observe the professor name, then I start with the extraction of all the information
  if (!is.na(first_name)){ 
    #adding name and last name
     database$name[page] <-       first_name
     database$last_name[page] <- last_name

    #filling up class information
    class <-  paste(extract_data_point("ec",page_lines,1),extract_data_point("ec",page_lines,2), sep="")
    database$class[page] <- class

    prof_rating <-  extract_data_point("rating of instructor",page_lines,number_type = "end")
    database$prof_rating[page] <- prof_rating

    course_rating <- extract_data_point("course rating",page_lines,number_type = "end")
    database$course_rating[page] <- course_rating

    concepts <- extract_data_point("concepts",page_lines,number_type = "end")
    database$concepts[page] <- course_rating

    interest <- extract_data_point("subject",page_lines,number_type = "end")
    database$interest[page] <- course_rating

    respondents <- extract_data_point("course rating",page_lines,number_type = "respondents")
    database$respondents[page] <- respondents

    participation <- extract_data_point("class participation",page_lines,number_type = "end")
    database$participation[page] <- participation

    assignments <- extract_data_point("returning assignments",page_lines,number_type = "end")
    database$assignments[page] <- assignments

    grading <- extract_data_point("in grading",page_lines,number_type = "end")
    database$grading[page] <- grading

    feedback <- extract_data_point("feedback to students",page_lines,number_type = "end")
    database$feedback[page] <- feedback

    availability <- extract_data_point("outside|outslde",page_lines,number_type = "end")
    database$availability[page] <- availability 

    section <- extract_data_point("ec",alternate_page_lines,3)
    database$section[page] <- gsub("[[:punct:]]","",section)
  }
  return(database)
}
print("All functions have been imported. Ready for scanning")