library(plyr)
library(effsize)
library(tidyverse)
setwd("code-ownership/ownership_data")

datasets = list.files("commitOwnership/developers/")

results = data.frame()
for(f in datasets){
  print(f)

  project = gsub("_.*","",f)
  release = sub(".*_","",f)
  result = data.frame(project = project, release = release)

  data = read.csv(paste0("../defect_data_file/",release),stringsAsFactors = F)
  
  commits = read.csv(paste0("commitOwnership/developers/",f), stringsAsFactors = F)
  colnames(commits)[1] = 'File'
  colnames(commits)[2] = 'Author'
  colnames(commits)[3] = "c_own"
  commits = commits[commits$File %in% data$File,]


  l_name = gsub(".csv","_Line.csv",f)
  lines = read.csv(paste0("lineOwnership/developers/",l_name), stringsAsFactors = F)
  lines = lines[,c("Filename", "Author", "NumLines")]
  colnames(lines)[1] = 'File'
  lines = lines[lines$File %in% data$File,]



  totalLines = ddply(lines, .(File), .fun=function(x){return(data.frame(TotalLines = sum(x$NumLines)))})
  lines = merge(lines,totalLines, by = "File")
  lines$l_own = lines$NumLines/lines$TotalLines

  studied_files = intersect(intersect(lines$File, commits$File),data$File)
  lines = lines[lines$File %in% studied_files,]
  commits = commits[commits$File %in% studied_files,]
  data = data[data$File %in% studied_files,]

  
  ### Update Code Ownership to the dataset 
  
  
  
  mdat = merge(commits, lines, by=c('File','Author'), all.x=T, all.y = T)
  print(length(unique(mdat$Author)))
  
  ##### ANALYSIS #####
  mdat$c_major = mdat$c_own > 0.05
  mdat$l_major = mdat$l_own > 0.05

  # #A1: count contributor
  counter = ddply(mdat, .(File), .fun=function(x){
    c_only = x[is.na(x$l_own),]
    l_only = x[is.na(x$c_own),]
    both = x[!is.na(x$l_own) & !is.na(x$c_own),]

    output = data.frame(totalContributor = nrow(x),
                        onlyCommits = nrow(c_only)/nrow(x),
                        onlyLine = nrow(l_only)/nrow(x),
                        both=nrow(both)/nrow(x)
                        )
    return(output)
  });
  print(summary(counter))
  print(nrow(counter[counter$minor_commit == 0,])/nrow(counter))

  # #miss_by_commits = There are x% of files whose contributors were missing when using commit-based code ownership.
  result$miss_by_commit = nrow(counter[counter$onlyCommits + counter$both != 1,])/nrow(counter)
  #
  #
  l_only = mdat[is.na(mdat$c_own) & !is.na(mdat$l_own),]
  # #X% of contributers who were not identified by commit-based ownership are major contributors of line-based ownership.
  result$miss_major_by_commits = nrow(l_only[l_only$l_major,])/nrow(l_only)
  #
  # #miss_by_lines = There are x% of files whose contributors were missing when using line-based code ownership
  result$miss_by_lines = nrow(counter[counter$onlyLine + counter$both != 1,])/nrow(counter)
  c_only = mdat[is.na(mdat$l_own) & !is.na(mdat$c_own),]
  # #X% of contributers who were not identified by line-based ownership are major contributors of commit-based ownership
  result$miss_major_by_lines = nrow(c_only[c_only$c_major,])/nrow(c_only)
  #
  # #A2: Expertise level
  #
  both = mdat[!is.na(mdat$c_own) & !is.na(mdat$l_own),]
  # # X% of lines-based major developers are identified as minor developers by commit-based ownership
  result$inconsistent_major_commits = nrow(both[both$l_major & !both$c_major,])/nrow(both[both$l_major,])
  #
  # # X% of commits-based major developers are identified as minor developers by lines-based ownership
  result$inconsistent_major_lines = nrow(both[both$c_major & !both$l_major,])/nrow(both[both$c_major,])
  #
  counter = ddply(mdat, .(File), .fun=function(x){
    c_major = x[x$c_major,]
    l_major = x[x$l_major,]
    c_minor = x[!x$c_major,]
    l_minor = x[!x$l_major,]
    output = data.frame(c_major=nrow(c_major), c_minor = nrow(c_minor), l_major = nrow(l_major), l_minor=nrow(l_minor))
    return(output)
  })
  #
  # #In X% of files, the two heuristics produce a different number of major developers
  result$diff_major = nrow(counter[counter$c_major != counter$l_major, ])/nrow(counter)
  #
  # #In X% of files, the two approaches produce a different number of minor developers
  result$diff_minor = nrow(counter[counter$c_minor != counter$l_minor, ])/nrow(counter)
  #
  # #Correlation
  result$correlation = cor(both$c_own, both$l_own, method='spearman')
  w = wilcox.test(both$c_own, both$l_own, paired = T)
  result$wilcox = w$p.value
  d = cliff.delta(both$c_own, both$l_own, paired=TRUE)
  result$cliff_est = d$estimate
  result$cliff_mag = as.character(d$magnitude)
  #
  # #Owner
  owner = ddply(mdat, .(File), .fun=function(x){

    c_owner = x[!is.na(x$c_own),]
    c_owner = c_owner[order(-c_owner$c_own),][1,]

    l_owner = x[!is.na(x$l_own),]
    l_owner = l_owner[order(-l_owner$l_own),][1,]

    output = data.frame(TotalDevelopers = nrow(x),
                        c_owner = c_owner$Author,
                        max_c_own = c_owner$c_own,
                        l_owner = l_owner$Author,
                        max_l_own = l_owner$l_own,stringsAsFactors = F)
    return(output)
  })
  #
  # #In X% of files, the ownership heuristics identify a different devloper as an owner
  result$diff_owner = nrow(owner[owner$c_owner != owner$l_owner ,])/nrow(owner)



  results = rbind(results, result)
}

# write.csv(results,paste0("~/mining-ownership/analysis_results/RQ1-results.csv"), row.names = F)
