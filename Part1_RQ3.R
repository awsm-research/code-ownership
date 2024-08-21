setwd("~/code-ownership")

library(tidyverse)
library(lsr)
releases = c("activemq-5.0.0", "activemq-5.1.0","activemq-5.2.0",
             "activemq-5.3.0","camel-1.4.0","camel-2.9.0","camel-2.10.0",
             "camel-2.11.0","groovy-1_5_7","groovy-1_6_BETA_1","groovy-1_6_BETA_2",
             "hbase-0.94.0","hbase-0.95.0","hbase-0.95.2","hive-0.9.0","hive-0.10.0","hive-0.12.0",
             "jruby-1.1","jruby-1.4.0","jruby-1.5.0","jruby-1.7.0.preview1","lucene-2.3.0","lucene-2.9.0",
             "lucene-3.0.0","lucene-3.1")

RQ3_results = data.frame()
own_data = data.frame()
library(effsize)
for(release  in releases){
  print(release)
  
  ##Open metric data to get only changed files
  data = read.csv(paste0("../defect_data_file/",release,".csv"),stringsAsFactors = F)
  
  ## Prepare commit authors data
  commits = read.csv(paste0("ownership_data/commitOwnership/developers/",release,".csv"), stringsAsFactors = F)
  colnames(commits)[1] = 'File'
  colnames(commits)[2] = 'Author'
  colnames(commits)[3] = "c_own"
  #Select only files that are in the metric data
  commits = commits[commits$File %in% data$File,]
  
  ## Prepare line authors data
  lines = read.csv(paste0("ownership_data/lineOwnership/developers/",release,"_Line.csv"), stringsAsFactors = F)
  lines = lines[,c("Filename", "Author", "NumLines")]
  colnames(lines)[1] = 'File'
  lines = lines[lines$File %in% data$File,]
  #Select only files that are in the metric data
  lines = lines %>% group_by(File) %>% mutate(l_own = NumLines/sum(NumLines))
  
  
  
  ### Analysis ###
  authors = full_join(commits, lines)
  
  commit_only = authors %>% filter(is.na(NumLines))
  line_only = authors %>% filter(is.na(Sum))
  
  own_data = rbind(own_data, data.frame(Release = release, variable="commit_own",value = commit_only$c_own))
  own_data = rbind(own_data, data.frame(Release = release, variable="line_own",value = line_only$l_own))
  
  c_output = cliff.delta(commit_only$c_own, line_only$l_own)
  w_output = wilcox.test(commit_only$c_own, line_only$l_own, alternative = "greater")
  
  rel_output = data.frame(Release = release, num_c_authors = nrow(commit_only), 
                          num_l_authors = nrow(line_only), 
                          c_own_median = median(commit_only$c_own), 
                          l_own_median = median(line_only$l_own), 
                          cliff_delta = c_output$estimate,
                          cliff_mag = c_output$magnitude,
                          p_value = round(w_output$p.value,digits=3))
  
  RQ3_results = rbind(RQ3_results, rel_output)
  
}

own_data$Project = gsub( "-.*","",own_data$Release)
library(ggplot2)
project_names = data.frame(Project= c("activemq","camel","groovy","hbase","hive","jruby","lucene"), Project_Name = c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene"))
own_data = inner_join(own_data, project_names)
ggplot(own_data, aes(x=Release, y=value, fill=variable)) + geom_boxplot() + facet_wrap(~ Project_Name, scale="free", ncol=7) +
  theme_bw() + theme(legend.position = "top",axis.text.x = element_text(angle = 30, hjust = 1)) + xlab("") + ylab("An ownership value") +
  scale_fill_manual(values=c("#2b8cbe","#a6bddb"), labels=c("Commit_only","Line_only"), name="")

ggsave("RQ3-ownership_values.pdf",width=10, height=3)

##Stat summary
own_data %>% group_by(Project,Release, variable) %>% summarize(med_val = median(value)) %>% group_by(variable) %>% summarize(min(med_val), max(med_val))

prop_major = own_data %>% group_by(Project_Name,Release,variable) %>% summarize(num_major = sum(value > 0.05)*100/n()) 

ggplot(prop_major, aes(x=Release, y=num_major, fill= variable)) + geom_bar(stat="identity", position=position_dodge(), col="#525252") + 
  facet_wrap(~ Project_Name, scale="free_x",ncol=7) + ylab("A proportion of major developers (%)") + xlab("") + 
  scale_fill_manual(name="",values=c("#2b8cbe","#a6bddb"), labels=c("Commit_only","Line_only")) + 
  theme_bw() +  theme(legend.position = "top",axis.text.x = element_text(angle = 30, hjust = 1)) 

  
  
ggsave("RQ3-major_contributors.pdf",width=10, height=3.2)

#Stat summary
prop_major %>% group_by(Project_Name,variable) %>% summarize(avg=mean(num_major))
