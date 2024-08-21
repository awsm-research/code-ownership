library(ScottKnottESD)
library(ggplot2)
setwd("~/Research/mining-ownership/Part2")
# df_models <- read.csv("rq4_models.csv")
# boxplot(df_models[,'auc'])

df_features <- read.csv("rq4_features_varimp.csv")

check.ANOVA.assumptions(df_features)

for(version in c("p","np")){
  sk <- sk_esd(df_features, version=version)
  ranking <- sk[[2]]
  
  data <- melt(df_features)
  data$ranking <- ranking[data$variable]
  
  ggplot(data, aes(x=reorder(variable, -value, median), y=value)) + geom_boxplot() + facet_grid(~ranking, space="free", scale="free", drop=TRUE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))  + labs(y="The Importance Scores", x = "")
  ggsave(paste0("RQ4-varimp-",version,".pdf"), width = 10, height = 4)
}
