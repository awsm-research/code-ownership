setwd("~/Research/mining-ownership/Part1")

library(tidyverse)
releases = c("activemq-5.0.0", "activemq-5.1.0","activemq-5.2.0",
             "activemq-5.3.0","camel-1.4.0","camel-2.9.0","camel-2.10.0",
             "camel-2.11.0","groovy-1_5_7","groovy-1_6_BETA_1","groovy-1_6_BETA_2",
             "hbase-0.94.0","hbase-0.95.0","hbase-0.95.2","hive-0.9.0","hive-0.10.0","hive-0.12.0",
             "jruby-1.1","jruby-1.4.0","jruby-1.5.0","jruby-1.7.0.preview1","lucene-2.3.0","lucene-2.9.0",
             "lucene-3.0.0","lucene-3.1")

RQ1_results = data.frame()
ratio_bug = c()
ratio_del_only = c()
for(release in releases){
print(release)
##Open metric data to get only changed files
data = read.csv(paste0("../Part2/datasets/",release,".csv"),stringsAsFactors = F)

## Prepare commit authors data
commits = read.csv(paste0("defect_data.old/commitOwnership/developers/",release,".csv"), stringsAsFactors = F)
colnames(commits)[1] = 'File'
colnames(commits)[2] = 'Author'
colnames(commits)[3] = "c_own"
#Select only files that are in the metric data
commits = commits[commits$File %in% data$File,]

## Prepare line authors data
lines = read.csv(paste0("defect_data.old/lineOwnership/developers/",release,"_Line.csv"), stringsAsFactors = F)
lines = lines[,c("Filename", "Author", "NumLines")]
colnames(lines)[1] = 'File'
lines = lines[lines$File %in% data$File,]
#Select only files that are in the metric data
lines = lines %>% group_by(File) %>% mutate(l_own = NumLines/sum(NumLines))

ratio_bug = c(ratio_bug, nrow(data[data$COMM == 0 & data$RealBug == T, ])*100/nrow(data[data$COMM == 0, ]))
ratio_del_only = c(ratio_del_only, nrow(data[data$Added_lines == 0 & data$Del_lines,])/nrow(data[data$COMM > 0, ]))


### Analysis ###
authors = full_join(commits, lines)

#RQ1: Does a set of contributors differ when using the commit-based and line-based approaches?

#RQ1.1: How many contributors in each file identified by each approach?
#  - commit_Authors = #authors who made a commit
#  - line_Authors = #authors of a line
#  - commont_Authors = #authors who both made a commit and lines
#  - line_only_Authors = #authors who wrote lines but no commit in the current release
#  - commit_only_Authors = #authors who made a commit but no history in git blame (= write many commits but over-written by someone else)
num_authors = authors %>% group_by(File) %>% summarize(num_Authors = n(),
                                                       commit_Authors = sum(!is.na(c_own)), 
                                                       line_Authors = sum(!is.na(l_own)),
                                                       common_Authors = sum(!is.na(c_own) & !is.na(l_own))) %>%
                                              mutate(
                                              line_only_Authors = line_Authors-common_Authors,
                                              commit_only_Authors = commit_Authors - common_Authors,
                                              prop_common = round(common_Authors*100/num_Authors,digits=0),
                                              prop_line_only = round(line_only_Authors*100/num_Authors,digits=0),
                                              prop_commit_only = round(commit_only_Authors*100/num_Authors,digits=0))

RQ1_results = rbind(RQ1_results, data.frame("Release" = release, num_authors))
}

write.csv(RQ1_results,"analysis_results/RQ1-results-new.csv",row.names = F)
