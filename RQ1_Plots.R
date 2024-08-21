setwd("~/code-ownership")

library(ggplot2)
library(reshape2)
library(tidyverse)


data = read.csv(file = "./defect_data_file/activemq-5.1.0.csv",stringsAsFactors = F)
## RQ1 results
RQ1_results = read.csv("analysis_results/RQ1-results-new.csv",stringsAsFactors = F) 
RQ1_results$Project = gsub( "-.*","",RQ1_results$Release)


## Plot the number of authors in a file
ggplot(RQ1_results, aes(x=Project, y=num_Authors)) + 
  geom_boxplot() + scale_y_log10() + ylab("The number of authors in a file") + 
  xlab("Studied Systems") + scale_x_discrete(labels=c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene"))

## Provide more details about the distribution of the authors
RQ1_results %>% group_by(Project) %>% summarize(median=median(num_Authors), 
                                                greater_than_one_authors = sum(num_Authors > 1)/n(),
                                                greater_than_two_authors = sum(num_Authors > 2)/n())

ggsave("authors_distribution.pdf", width=5, height=2.5)

## Plot the proportion of authors in a file
m_data = melt(RQ1_results[,c('Project','Release','prop_common','prop_commit_only','prop_line_only')])

ggplot(m_data, aes(x=Project, fill=variable, y=value)) + geom_boxplot() + 
  ylab("A proportion of developers in a file in a release (%)") + xlab("Studied Systems")  + 
  scale_x_discrete(labels=c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene")) + theme_bw() + 
  scale_fill_manual(name="Developers", values=c("#ece7f2","#a6bddb","#2b8cbe"), labels=c("Common","Commit_only","Line_only")) + theme(legend.position = "top") 

ggsave("RQ1-authors_cnt.pdf",width=6, height=4)


#Statistic Summary of common authors
# median_common = the median value of the % common authors
# no_common_with_commit = how many files that have a commit history, but still do not have common authors
# no_commits = %files that do not have a commit history
# all_common = %files that have a proportion of common authors of 100%
RQ1_results %>% group_by(Project) %>% summarize(median_common = median(prop_common), 
                                                no_common = sum(prop_common == 0)/n(),
                                                no_common_with_commit  = sum(prop_common == 0 & commit_Authors > 0)*100/sum(commit_Authors > 0),
                                                no_commits = sum(commit_Authors == 0)/n(),
                                                all_common = sum(prop_common == 1)/n()) 

##RQ1.2 count how many files that have {}_only authors
sum_data = RQ1_results %>% group_by(Project) %>% summarize(median_commit_only = median(prop_commit_only),
                                                median_line_only = median(prop_line_only),
                                                has_commit_only = sum(prop_commit_only > 0)*100/n(),
                                                has_line_only = sum(prop_line_only > 0)*100/n())

## Plot a barplot
m_data = melt(sum_data[,c("Project","has_commit_only","has_line_only")])
ggplot(m_data, aes(x=Project, y=value, fill= variable)) + geom_bar(stat="identity", position=position_dodge()) +
  ylab("A proportion of files (%)") + scale_fill_manual(name="",values=c("#2b8cbe","#a6bddb"), labels=c("At least one commit_only authors","At least one line_only authors"))+
  scale_x_discrete(labels=c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene")) +
  theme_bw() + theme(legend.position = "top") + xlab("Studied Systems")

ggsave("RQ1-prop_missing.pdf",width=5, height=3)


##RQ1.2 Provide statistic summary for commit_only authors
RQ1_results %>% filter(prop_commit_only > 0) %>% ungroup() %>% 
  summarize(min(commit_only_Authors), max(commit_only_Authors), min(prop_commit_only), max(prop_commit_only))
##RQ1.2 Provide statistic summary for commit_only authors
RQ1_results %>% filter(prop_line_only > 0) %>% ungroup() %>% 
  summarize(min(line_only_Authors), max(line_only_Authors), min(prop_line_only), max(prop_line_only))


