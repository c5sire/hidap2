get_fieldbook_list <- function(crop){
  fp <- file.path(fname_fieldbook, crop)
  list.files(fp)
}

get_fieldbook_table <-function(crop, fieldbook){
  fp <- file.path(fname_fieldbook, crop, fieldbook)
  load(fp)
  fieldbook
}
