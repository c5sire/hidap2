
new_program_table <- function(){
  crop_id   <- c("SP", "SP", "PT") # must be two upper case letters
  program_id <- c("OF", "VR", "DT")
  program_name <- c("orange-fleshed", "virus-resistant", "drought-resistant")
  res <- as.data.frame(cbind(crop_id, program_id, program_name),
                stringsAsFactors = FALSE)
  crops <- get_crop_table()
  res$crop_id <- as.factor(res$crop_id)
  levels(res$crop_id) <- sort(unique(crops$crop_id))

  res
}

get_program_table <- function(){

  if(!file.exists(fname_programs)) {
    table_programs <- new_program_table()
    save(table_programs, file = fname_programs)
  }
  load(fname_programs)
  table_programs
}

post_program_table <- function(table_programs){
  save(table_programs, file = fname_programs)
}

