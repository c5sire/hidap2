
new_crop_table <- function(){
  crop_name <- c("potato", "sweetpotato")
  crop_id   <- c("PT", "SP") # must be two upper case letters
  crop_species <- c("Solanum tuberosum", "Ipomoea batatas")
  crop_type <- c("clonal", "clonal")

  as.data.frame(cbind(crop_name, crop_id, crop_species, crop_type),
                stringsAsFactors = FALSE)
}

get_crop_table <- function(){

  if(!file.exists(fname_crops)) {
    table_crops <- new_crop_table()
    save(table_crops, file = fname_crops)
  }
  load(fname_crops)
  table_crops
}

post_crop_table <- function(table_crops){
  save(table_crops, file = fname_crops)
}

