library(RCurl)
library(rjson)

setwd("git/Dcard_crawler")

dcard_excerpt_crawler = function(url){
  json = getURLContent(url)
  all_data = fromJSON(json)
  
  tmp = lapply(1:length(all_data), function(i) data.frame(id = all_data[[i]]$id, update = all_data[[i]]$updatedAt, 
                                                          title = all_data[[i]]$title, content = all_data[[i]]$excerpt,  
                                                          commentCount= all_data[[i]]$commentCount, 
                                                          tags = paste0(all_data[[i]]$tags, collapse = ", "),
                                                          gender = all_data[[i]]$gender,
                                                          school = all_data[[i]]$school,
                                                          stringsAsFactors = F
  ))
  tmp = do.call("rbind", tmp)
  return(tmp)
}

max_i = 100
for(i in 1:max_i){
  if(i==1){
    total_df = dcard_excerpt_crawler("https://www.dcard.tw/_api/forums/job/posts?popular=false")
  }else{
    url = paste0("https://www.dcard.tw/_api/forums/job/posts?popular=false&before=", tail(total_df$id,1))
    tmp = dcard_excerpt_crawler(url)
    total_df = rbind(total_df, tmp)
  }
  cat("\r", format(round(i/max_i*100, 2), nsmall=2), "%")
  Sys.sleep(runif(1, 10, 20))
}

# title
for(i in 1:nrow(total_df)){
  tryCatch({
    url = paste0("https://www.dcard.tw/_api/posts/", total_df$id[i])
    json = getURLContent(url)
    tmp = fromJSON(json)
    total_df$content[i] = tmp$content
    cat("\r", format(round(i/nrow(total_df)*100, 2), nsmall=2), "%")
    Sys.sleep(runif(1, 10, 20))
  }, error = function(e){
    Sys.sleep(runif(1, 60, 65))
    url = paste0("https://www.dcard.tw/_api/posts/", total_df$id[i])
    json = getURLContent(url)
    tmp = fromJSON(json)
    total_df$content[i] <<- tmp$content
    cat("\r", format(round(i/nrow(total_df)*100, 2), nsmall=2), "%")
  })
}

dir.create("output", showWarnings = F)
write.csv(total_df, paste0("output/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "dcard_jobs.csv"), row.names = F)
