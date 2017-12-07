

#geocode address
geo.code.input.address <- function(address.input,geocode.source='google'){
  data.frame(geocode(address.input,source=geocode.source), address=address.input)
}
#geo.code.input.address<- memoize(geo.code.input.address)


#This function selects tracts based upon their location within the buffer zone.
#If method =within, function will extract those tracts whose centroid is withing the buffer zone
#if method = intersect, function will extact tracts what are at all within the buffer zone.
#method expect a character value of "within" or "intersect" - not case sensitive.
#tracts expects a spatial data frame with census tracts;
#centroids expect a spatial data frame for census tracts centroids.  Only needed when
#using "within" function.

select_tracts_from_buffer <- function(method,tracts,centroids,buffer.file){
  #identify the cetroids that are within the buffer file.
  #select those tracts have their centroids within the buffer file.'
  if(tolower(method)=='within'){
    tracts.pts.sel    <-  centroids[buffer.file,]
    tracts.shp.sel  <-  tracts[which(tracts$geoid %in% tracts.pts.sel$geoid),] #select those tracts have their centroids within the buffer file.
  }
  #identify those tracts that intersect the buffer file.
  if(tolower(method)=='intersect'){
    tracts.shp.sel <- tracts[buffer.file,]
  }
  return(tracts.shp.sel)
}

process_acs_data<- function(i,acs.data,acs.shell,group.var){
  acs.proportions <-divide.acs(acs.data[,i], acs.data[,1], method="proportion", verbose=T, output="result")
  confid.proportions <- data.frame(confint(acs.proportions))
  colnames(confid.proportions) <- c("low.share","high.share")

  confid.estimates <- data.frame(confint(acs.data[1,i]))
  colnames(confid.estimates) <- c("low.est","high.est")
  
  output <- data.frame(full_table =acs.shell[i,group.var], den=acs.shell[1,group.var],
                       est=estimate(acs.data)[1,i],std=standard.error(acs.data)[1,i],
                       share=estimate(acs.proportions)[1,1],share.std=standard.error(acs.proportions)[1,1], confid.proportions,confid.estimates)
  output <-  mutate(output,est.cv= std/est) %>%
    select(lep,low.est,est,high.est,est.cv,low.share,share,high.share, std)
  return(output)
}


aggregate.acs.data.across.columns <- function(i,denom, acs.data,acs.shell,group.values, group.var) {
  sel <-pull(acs.shell[,group.var] ) %in% group.values[i]
  table <- pull(acs.shell[sel,'table'][1,])
  b <- sum(apply(acs.data[,sel],1,sum))
  acs.proportions <-divide.acs(b[,1], denom, method="proportion", verbose=T, output="result")
  
  confid.proportions <- data.frame(confint(acs.proportions))
  colnames(confid.proportions) <- c("low.share","high.share")
  
  confid.estimates <- data.frame(confint(b))
  colnames(confid.estimates) <- c("low.est","high.est")
  
  output <- data.frame(full_table =group.values[i], den=acs.colnames(denom),
                       est=estimate(b)[1,1],std=standard.error(b)[1,1],
                       share=estimate(acs.proportions)[1,1],share.std=standard.error(acs.proportions)[1,1], confid.proportions,confid.estimates)
  output <-  mutate(output,est.cv= std/est) %>%
    select(category=full_table, low.est, est, high.est,est.cv,low.share, share, high.share, std )
  return(output)
}

geo.create <- function(county.count,county.list){
   geo.make(state="NY",county="*",tract='*') 
}

acs.fetch.m <- function(geo,endyear,table.number){
  a<-acs.fetch(geo=geo,endyear = endyear,span=5,table.number=table.number)
  a@geography$geoid<- with(a@geography,as.character(state*10^9 + county*10^6 + as.numeric(tract)))
  return(a)
}
#acs.fetch.m <- memoise(acs.fetch.m)


tracts_for_analysis_m <- function(state.shp,mpo, mpos.list){
  if(length(mpo)==1 & sum(mpo %in% "All")==1){state.shp}
  else if(length(mpo)==1 & sum(mpo %in% "All")==0) {state.shp[which(state.shp$mpo %in% mpo),]}
  else if(length(mpo)>1  & sum(mpo %in% "All")==0) {state.shp[which(state.shp$mpo %in% mpo),]}
  else{state.shp}
  }


#tracts_for_analysis_m<- memoise(tracts_for_analysis_m)


option2calculation <- function(acs.data,acs.shell,acs.group.var){
  group.value <- pull(unique(acs.shell[,acs.group.var]))
  group.value.n <- length(group.value)
  group.denom<- sum(apply(acs.data[,1],1,sum))
  a <-do.call('rbind', lapply(1:group.value.n,aggregate.acs.data.across.columns, group.values=group.value, denom=group.denom,  acs.data=acs.data, acs.shell=acs.shell,
                              group.var=acs.group.var))
  #full_table	den	est	std	share	share.std	low.share	high.share	low.est	high.est
}

format.data.table <- function(acs.data, method,buffer, address){
  datatable(acs.data,rownames=FALSE,caption = paste("This data is for tracts ",tolower(method), "a ", buffer, " mile radius of ", address),
            
            colnames = c('Category', 'Low Estimate', 'Estimate', 'High Estimate', 'CV','Low Share',"Share",'High Share','STD')) %>%
    formatPercentage(c('share','est.cv',"low.share","high.share"), 1) %>%    
    formatCurrency(c('est','low.est',"high.est",'std'), currency="",interval=3,mark=",",digits=0)  
}


