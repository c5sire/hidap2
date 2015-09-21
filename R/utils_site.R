
new_site_table <- function(){
  n = 3
  id <- 1:n
  shortn <- paste("ID_",id)
  altern <- rep("", n)
  fulln <-  rep("", n)
  local <-  rep("", n)
  latd <-  rep(0, n)
  lond <- rep(0, n)
  elev <-  rep(0, n)
  crops <-  rep("", n)
  aez <-  rep("", n)
  cont<-  rep("", n)
  creg<-  rep("", n)
  cntry<-  rep("", n)
  adm4<-  rep("", n)
  adm3<-  rep("", n)
  adm2<-  rep("", n)
  adm1<-  rep("", n)
  comment<-  rep("", n)

  res <- as.data.frame(cbind(
    id, shortn, altern, fulln, local, latd, lond, elev, crops, aez,
    cont, creg, cntry, adm4, adm3, adm2, adm1,  comment,
                       stringsAsFactors = FALSE))
  res
}

get_site_table <- function(){

  if(!file.exists(fname_sites)) {
    table_sites <- new_site_table()
    save(table_sites, file = fname_sites)
  }
  load(fname_sites)
  table_sites
}

post_sites_table <- function(table_sites){
  save(table_sites, file = fname_sites)
}

