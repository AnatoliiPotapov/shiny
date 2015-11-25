library(stringr)

read_DT<-function(name,file_path){
  
  if (length(grep(".r48", x = name)) > 0)  {num = 48}
  else if (length(grep(".r96", x = name)) > 0) {num = 96}
  else return(NULL)
  
  all_data = readLines(file_path)
  output <-  list()
    output$version = get_version(all_data) 
    output$date = get_date(all_data) 
    output$operator = get_operator(all_data) 
    output$program = get_program(all_data) 
    output$optic = get_optic_curves(all_data, num) 
    output$labels = get_sample_labels(all_data, num)
  return (output)
}

get_sample_labels <- function(all_data, num){
  start_index = grep("\\$Information about tubes:\\$", all_data) 
  labels <- list()
  for (i in 1:num){
    slice_data = all_data[start_index + i]
    labels[[i]] <- gsub("[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+ +[^ ]+(.+)","\\1",slice_data)
  }
  return(labels)
}

get_program<-function(all_data){
  program_data<-list()
  start_index = grep("\\$Program of amplification:\\$", all_data)
  end_index =  start_index - 1 + grep("\\$(.*)\\$", all_data[-(1:start_index)])[1]
  amplification_data = grep("\\$Program of amplification:\\$|\tXCYC|\tXLEV",all_data[start_index:end_index],value=TRUE)
  amplification_data = sub("\\$Program of amplification:\\$", "Программа амплификации:", amplification_data)
  amplification_data = gsub("\tXCYC", "Колличество циклов:", amplification_data)
  amplification_data = gsub("\tXLEV +([0-9][0-9])([0-9][0-9]) +([0-9]+).*", "Температура: \\1.\\2 С, продолжительность: \\3 секунд", amplification_data, perl=TRUE)
  return(amplification_data) 
}

get_operator<-function(all_data){
  data_index = grep("\\$Operator:\\$", all_data)
  operator_data = sub("\\$Operator:\\$", "Оператор:", all_data[data_index])
  return(operator_data)
}

get_version<-function(all_data){
  data_index = grep("\\$Version:\\$", all_data)
  version_data = sub("\\$Version:\\$", "Версия:", all_data[data_index])
  return(version_data)
}

get_date<-function(all_data){
  data_index = grep("\\$Data:\\$", all_data)
  date_data = sub("\\$Data:\\$", "Дата:", all_data[data_index])
  return(date_data)
}

get_optic_curves<-function(all_data, num){
  # находим начало оптических измерений
  data_index = grep("\\$Results of optical measurements:\\$", all_data)
  # создаем таблицу оптических измерений
  optical_data = all_data[-(1:data_index)]
  
  output <- list()
  
  optic_channels=list("FAM_1", "FAM_2", "HEX_1", "HEX_2", "ROX_1", "ROX_2", "Cy5_1", "Cy5_2", "Cy5.5_1", "Cy5.5_2")
  for ( index in 1:10 )  {
    
    mt <- matrix(nrow = length(optical_data)/10, ncol = num)
    for ( i in 1:(length(optical_data)/10) ) {
      mt[i,] <- extract_optic_data(optical_data[(((i-1) * 10) + index)])[-(1:6)]
    }
    
    output[[optic_channels[[index]]]] <- mt
  }

  return(output)
}

extract_optic_data<-function(data){
  result = as.numeric( unlist(str_extract_all(data, "( )+[0-9]+")))
  return(result)
}


