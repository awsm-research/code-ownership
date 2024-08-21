setwd("~/code-ownership")

library(tidyverse)
library(lsr)
releases = c("activemq-5.0.0", "activemq-5.1.0","activemq-5.2.0",
             "activemq-5.3.0","camel-1.4.0","camel-2.9.0","camel-2.10.0",
             "camel-2.11.0","groovy-1_5_7","groovy-1_6_BETA_1","groovy-1_6_BETA_2",
             "hbase-0.94.0","hbase-0.95.0","hbase-0.95.2","hive-0.9.0","hive-0.10.0","hive-0.12.0",
             "jruby-1.1","jruby-1.4.0","jruby-1.5.0","jruby-1.7.0.preview1","lucene-2.3.0","lucene-2.9.0",
             "lucene-3.0.0","lucene-3.1")

RQ2_results = data.frame()
for(release in releases){
  print(release)
  
  ##Open metric data to get only changed files
  data = read.csv(paste0("./defect_data_file/",release,".csv"),stringsAsFactors = F)
  
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
  
  common_authors = authors %>% filter(c_own > 0 & l_own > 0) %>% ungroup() %>% mutate(c_major = c_own > 0.05, l_major = l_own > 0.05)
  
  contigency = common_authors  %>% summarize(num_common_authors = n(), 
                                                  major_major = sum(c_major & l_major)*100/n(),
                                                  minor_minor = sum(!c_major & !l_major)*100/n(),
                                                  major_minor = sum(c_major & !l_major)*100/n(),
                                                  minor_major = sum(!c_major & l_major)*100/n())
  
  ## Get correlation 
  rho = cor(common_authors$c_own, common_authors$l_own, method = "spearman")
  
  rho_strength = "Small"
  if(rho >= 0.3){
    rho_strength = "Moderate"
  }else if(rho >= 0.7){
    rho_strength = "Strong"
  }
  
  #Build contingency table
  exp_tab = table(common_authors$c_major, common_authors$l_major)
  
  #Try to do fisher exact test
  if(nrow(exp_tab) == 2){
    # Example: exp_tab
    #         FALSE TRUE (l_own)
    # FALSE     0    1
    # TRUE    151 2110
    # (c_own)
    f_test = fisher.test(exp_tab)
    OR = f_test$estimate
    p_value = f_test$p.value
    
    #Save results
    RQ2_results = rbind(RQ2_results, data.frame(Project=gsub( "-.*","",release), Release=release, Spearman_Rho = rho, Rho_Strength=rho_strength, Fisher_p_value=p_value, Fisher_OR = OR, contigency))
  }else{
    RQ2_results = rbind(RQ2_results, data.frame(Project=gsub( "-.*","",release), Release=release, Spearman_Rho = rho, Rho_Strength=rho_strength, Fisher_p_value=NA, Fisher_OR = NA,contigency ))
  }
  
}

library(ggplot2)
library(reshape2)
##Plot Correlation
ggplot(RQ2_results, aes(y=Spearman_Rho, x=Project)) + geom_boxplot() + ylab(expression(paste("Spearman's correlation coefficients (", rho,")"))) +
  xlab("Studied Systems") + scale_x_discrete(labels=c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene")) + scale_y_continuous(limits=c(0,1)) + coord_flip()

ggsave("RQ2-correlation.pdf",width=5, height=2.5)

##Statistic summary
RQ2_results %>% group_by(Project) %>% summarize(median_rho = median(Spearman_Rho)) 

table(RQ2_results$Rho_Strength)

m_data = melt(RQ2_results[,c("Project","Release","major_major","minor_minor","major_minor","minor_major")])
m_data$group = "Inconsistent Expertise"
m_data[m_data$variable %in% c("major_major","minor_minor"),]$group = "Consistent Expertise"

# ggplot(m_data, aes(x=Project, y=value, fill=group)) + geom_boxplot()  +
#   scale_x_discrete(labels=c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene")) + theme_bw() +
#   ylab("A proportion of authors (%)") + theme(legend.position = "top") + xlab("Studied Systems") +
#   scale_fill_manual(labels = c("Consistent","Inconsistent"),
#                     values = c("#74a9cf","#bdc9e1"), name="Expertise Level") + guides(fill = guide_legend(ncol = 2,byrow = TRUE))
# 

m_data$variable = factor(m_data$variable, levels=c("minor_major","major_minor", "minor_minor","major_major"))

project_names = data.frame(Project= c("activemq","camel","groovy","hbase","hive","jruby","lucene"), Project_Name = c("ActiveMQ","Camel","Groovy","HBase","Hive","JRuby","Lucene"))
m_data = inner_join(m_data, project_names)
ggplot(m_data, aes(x=Release, y=value, fill=variable)) + geom_bar(stat="identity", col="#525252") + facet_wrap(~ Project_Name,scales = "free", ncol = 7) + 
  scale_fill_manual(name="",values=c("#f1eef6","#bdc9e1","#74a9cf","#0570b0"), labels=c("Minor_commit & Major_line","Major_commit & Minor_line","Minor_commit & Minor_line","Major_commit & Major_line")) +
  theme_bw() + theme(legend.position = "top",axis.text.x = element_text(angle = 30, hjust = 1)) + xlab("") + ylab("The proportion of authors (%)") 

# wilcox.test(RQ2_results$major_major+RQ2_results$minor_minor,RQ2_results$major_minor+RQ2_results$minor_major, paired = T, alternative = "greater" )
ggsave("RQ2-consistent_level.pdf",width=12, height=4)

average = m_data %>% group_by(Project,group,variable)%>% summarize(average=mean(value)) %>% group_by(Project, group) %>% summarise(sum=sum(average))


