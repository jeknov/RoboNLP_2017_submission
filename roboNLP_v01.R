df <- read.csv("TRANSCRIPTS FILE URL", stringsAsFactors = F)

#df <- subset(df, subject_no %in% c(2,3,4,5,7,8,9,12,13,14,15))
df$condition <- as.factor(df$condition)


##### NLP #####

library(stringr)
library(stringi)
library(dplyr)
library(tidytext)
library(qdap)
library(ggplot2)

df$ref_clean <- sapply(df$utterance, # remove punctuation
                       function(x) str_replace_all(x, pattern = "[[:punct:]]", "")) 
df$ref_clean <- sapply(df$ref_clean, # trim white spaces
                       function(x) str_replace_all(x, pattern = "\\s+", " "))
df$ref_clean <- sapply(df$ref_clean, # convert to lower case
                       function(x) tolower(x))

df$utterance <- sapply(df$utterance, # add "." to the end if missed
                 function(x) ifelse(substr(x, nchar(x), nchar(x)) == "." | x == "", x, 
                                    paste(x,".",sep="")))
df$ref_coll <- sapply(df$utterance, # trim white spaces
                      function(x) str_replace_all(x, pattern = "\\s+", " "))
df$ref_coll <- sapply(df$ref_coll, # convert to lower case
                      function(x) tolower(x))

df$n.char <- sapply(df$ref_clean, 
                    function(x) stri_length(x) - stri_count_fixed(x, " ")) # w/o spaces
df$word_count <- sapply(df$ref_clean,
                        function(x) length(trimws(unlist(strsplit(x, "[^[:alnum:]]")))))
df$word_count_uniq <- sapply(df$ref_clean,
                             function(x) length(unique(trimws(unlist(strsplit(x, "[^[:alnum:]]"))))))

df$lexic_diversity <- df$word_count_uniq / df$word_count 

df$n.sent <- sapply(df$utterance, # number of sentences
                    function(x) str_count(x, "\\."))

df$wps <- df$word_count / df$n.sent
df$wps_unique <- df$word_count_uniq / df$n.sent

df.nlp = df %>%
  group_by(subject_ID, actor) %>%
  summarise(n.char.avg = mean(n.char, na.rm = TRUE),
            w.count.avg = mean(word_count, na.rm = TRUE),
            w.count.uniq.avg = mean(word_count_uniq, na.rm = TRUE),
            lex.div.avg = mean(lexic_diversity, na.rm = TRUE),
            n.sent.avg = mean(n.sent, na.rm = TRUE),
            wps.avg = mean(wps, na.rm = TRUE),
            wps.uniq.avg = mean(wps_unique, na.rm = TRUE))

df.all_nlp <- merge(x = df.all.scores, y = df.nlp, by.x = "subject", 
                       by.y = "subject_ID", all.x = TRUE)

#### Add non-linguistic dialogue features ####

# Prosodic features are calculated in sound_analysis_v01.R and analysed in prosody_analysis_v01.R



# Here, I will add them manually
write.csv(df.all_nlp, "df.all_nlp.csv", row.names = F)
df.all_nlp <- read.csv("df.all_nlp.csv")

##### Correlation #####

hum <- subset(df.all_nlp, actor == "Human")
rob <- subset(df.all_nlp, actor == "Robot")
cor.all.hum <- as.data.frame(cor(hum[,c(2:30, 33:39)], method = "spearman", use="complete.obs"))[c(8:29),c(1:7, 30:36)]
cor.all.rob <- as.data.frame(cor(rob[,c(2:30, 33:39)], method = "spearman", use="complete.obs"))[c(8:29),c(1:7, 30:36)]

cor.emotion <- as.data.frame(cor(hum[,c(2:4, 9:29)], method = "spearman", use="complete.obs"))[c(4:24), c(1:3)]
cor.prosody <- as.data.frame(cor(hum[,c(5,6,8, 9:29)], method = "spearman", use="complete.obs"))[c(4:24), c(1:3)]
cor.nonLing.h <- as.data.frame(cor(hum[,c(40:44, 9:29)], method = "spearman", use="complete.obs"))[c(6:26), c(1:5)]
cor.nonLing.r <- as.data.frame(cor(rob[,c(40:44, 9:29)], method = "spearman", use="complete.obs"))[c(6:26), c(1:5)]
cor.ling.h <- as.data.frame(cor(hum[,c(33:39, 9:29)], method = "spearman", use="complete.obs"))[c(8:28), c(1:7)]
cor.ling.r <- as.data.frame(cor(rob[,c(33:39, 9:29)], method = "spearman", use="complete.obs"))[c(8:28), c(1:7)]



##### Bigrams #####

df_bigram <- df %>%
  unnest_tokens(bigram, ref_clean, token = "ngrams", n = 2) %>%
  group_by(utterance) 
#df_bigram_b$bigram <- as.factor(df_bigram_b$bigram)

df_bigr2 <- df_bigram %>%
  group_by(actor, condition,bigram) %>%
  summarise(bigram_count = n()) %>%
  arrange(desc(bigram_count)) %>%
  head(50) %>%
  mutate(order = row_number(-bigram_count))

df_bigr3 <- df_bigram %>%
  group_by(actor, bigram) %>%
  summarise(bigram_count = n()) %>%
  arrange(desc(bigram_count)) %>%
  #head(50) %>%
  mutate(order = row_number(-bigram_count))
df_hum <- subset(df_bigr3, actor == "Human")
df_rob <- subset(df_bigr3, actor == "Robot")

length(subset(df_hum, bigram_count==1)[[2]]) # how many bigrams used just once
summary(subset(df_hum, bigram_count!=1)$bigram_count) # avg frequency of bigrams used more than once
sd(subset(df_hum, bigram_count!=1)$bigram_count)

length(subset(df_rob, bigram_count==1)[[2]]) # how many bigrams used just once
summary(subset(df_rob, bigram_count!=1)$bigram_count) # avg frequency of bigrams used more than once
sd(subset(df_rob, bigram_count!=1)$bigram_count)

t.test(subset(df_hum, bigram_count!=1)$bigram_count,
       subset(df_rob, bigram_count!=1)$bigram_count) # significant


ggplot(df_bigr2, aes(x=order, y=bigram_count))+
  geom_bar(stat="identity")+
  scale_x_continuous(
    breaks = df_bigr2$order,
    #labels = df_bigr2$bigram,
    expand = c(0,0))+
  xlab("Bigrams")+
  ylab("Frequency")+ 
  facet_wrap(~condition, ncol=2) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=17)) 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

by(df_hum$bigram_count, df_hum$condition, summary) # chat condition is more varied for bigrams

t.test(subset(df_hum, condition == "chat")$bigram_count,
       subset(df_hum, condition == "task")$bigram_count, p.adj = "none") # no sign diff

##### Linguistic statistics #####

t.test(subset(df, actor == "Human" & condition == "chat")$n.char,
       subset(df, actor == "Human" & condition == "task")$n.char, p.adj = "none") #no
t.test(subset(df, actor == "Human" & condition == "chat")$word_count,
       subset(df, actor == "Human" & condition == "task")$word_count, p.adj = "none") #no
t.test(subset(df, actor == "Human" & condition == "chat")$word_count_uniq,
       subset(df, actor == "Human" & condition == "task")$word_count_uniq, p.adj = "none") #no
t.test(subset(df, actor == "Human" & condition == "chat")$lexic_diversity,
       subset(df, actor == "Human" & condition == "task")$lexic_diversity, p.adj = "none") #no
t.test(subset(df, actor == "Human" & condition == "chat")$n.sent,
       subset(df, actor == "Human" & condition == "task")$n.sent, p.adj = "none") #no
t.test(subset(df, actor == "Human" & condition == "chat")$wps,
       subset(df, actor == "Human" & condition == "task")$wps, p.adj = "none") #no
t.test(subset(df, actor == "Human" & condition == "chat")$wps_unique,
       subset(df, actor == "Human" & condition == "task")$wps_unique, p.adj = "none") #no

##### Trigrams #####

df_trigram <- df %>%
  unnest_tokens(trigram, ref_clean, token = "ngrams", n = 3) %>%
  group_by(utterance) 

df_trigr2 <- df_trigram %>%
  group_by(actor,trigram) %>%
  summarise(trigram_count = n()) %>%
  arrange(desc(trigram_count)) %>%
  head(25) %>%
  mutate(order = row_number(-trigram_count))

df_trigr3 <- df_trigram %>%
  group_by(actor, trigram) %>%
  summarise(trigram_count = n()) %>%
  arrange(desc(trigram_count)) %>%
  #head(50) %>%
  mutate(order = row_number(-trigram_count))
df_rob2 <- subset(df_trigr3, actor == "Robot")
df_hum2 <- subset(df_trigr3, actor == "Human")

length(subset(df_hum2, trigram_count==1)[[2]]) # how many trigrams used just once
summary(subset(df_hum2, trigram_count!=1)$trigram_count) # avg frequency of trigrams used more than once
sd(subset(df_hum2, trigram_count!=1)$trigram_count)

length(subset(df_rob2, trigram_count==1)[[2]]) # how many trigrams used just once
summary(subset(df_rob2, trigram_count!=1)$trigram_count) # avg frequency of trigrams used more than once
sd(subset(df_rob2, trigram_count!=1)$trigram_count)

t.test(subset(df_rob2, trigram_count!=1)$trigram_count,
       subset(df_hum2, trigram_count!=1)$trigram_count)

df_top <- rbind(subset(df_hum2, order < 11), subset(df_rob2, order < 11))
df_top$order <- c(1:20)


a <- ggplot(subset(df_hum2, order < 11), aes(x=order, y=trigram_count))+
  geom_bar(stat="identity")+
  scale_x_continuous(
    breaks = df_top$order,
    labels = df_top$trigram,
    expand = c(0,0))+
  xlab("")+
  ylab("Frequency")+ 
  scale_y_continuous(limits = c(0,150)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=17)) +
  ggtitle("Human")
b <- ggplot(subset(df_rob2, order < 11), aes(x=order, y=trigram_count))+
    geom_bar(stat="identity")+
    scale_x_continuous(
      breaks = df_rob2$order,
      labels = df_rob2$trigram,
      expand = c(0,0))+
    xlab("")+
    ylab("Frequency")+ 
    #facet_wrap(~actor, ncol=2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=17)) +
  ggtitle("Robot")
grid.arrange(a,b,ncol=1)

by(df_hum2$trigram_count, df_hum2$condition, summary) # chat condition is more varied for bigrams
t.test(subset(df_hum2, condition == "chat")$trigram_count,
       subset(df_hum2, condition == "task")$trigram_count, p.adj = "none") # no sign diff

write.csv(subset(df, condition == "chat" & actor == "Human"), "chat_transcripts.csv", row.names = F)
write.csv(subset(df, condition == "task" & actor == "Human"), "task_transcripts.csv", row.names = F)


##### Descriptive statistics for the Table 1? #####

table1 <- as.data.frame(matrix(nrow=18, ncol = 2))
row.names(table1) <- names(hum)[c(2:6,8,33:44)]
names(table1) <- c("Human", "Robot")

for (i in 1:18){
  if (i<6) {
    table1[i,1] <- summary(hum[,i+1])[4]}
  if (i==6){
    table1[i,1] <- summary(hum[,i+2])[4]}
  if (i>6){
    table1[i,1] <- summary(hum[,i+26])[4]
    table1[i,2] <- summary(rob[,i+26])[4]}
}

write.csv(table1, "table1.csv")


t.test(hum$n.char.avg, rob$n.char.avg)
t.test(hum$w.count.avg , rob$w.count.avg)
t.test(hum$w.count.uniq.avg , rob$w.count.uniq.avg)
t.test(hum$lex.div.avg, rob$lex.div.avg)
t.test(hum$n.sent.avg, rob$n.sent.avg)
t.test(hum$wps.avg, rob$wps.avg)
t.test(hum$wps.uniq.avg, rob$wps.uniq.avg)
t.test(hum$dur.speech, rob$dur.speech)
t.test(hum$no.turns, rob$no.turns)
t.test(hum$no.self.repeat, rob$no.self.repeat)
t.test(hum$task.per.turn, rob$task.per.turn)

##### D-Levels Plot #####
require(data.table)
dat <- read.csv("dlevels.csv")[,2:3]
rownames(dat) <- c(0:7)
datm <- melt(cbind(dat, Level = rownames(dat)), id.vars = c('Level'))

library(scales)
require(ggsci)
#require(graphics)
#shadesOfGrey <- colorRampPalette(c("grey0", "grey76"))
#mypal <- shadesOfGrey(8)
ggplot(datm,aes(x = variable, y = value,fill = Level)) + 
  geom_bar(position = "fill",stat = "identity") + 
  scale_y_continuous(labels = percent_format()) +
  theme(text = element_text(size=27)) + 
  scale_x_discrete(name="") +
  scale_y_continuous(name="Percentage") +
  #theme_bw() +
  scale_fill_npg()
  
scale_fill_manual(values=mypal)

##### Predictive model ######

library(caret)
library(randomForest)

train_index <- createDataPartition(df.all_nlp$mean.rate, p = .7, list = FALSE)
trainset <- df.all_nlp[train_index, ]
testset <- df.all_nlp[-train_index, ]

pred.summary2 <- as.data.frame(matrix(nrow=22, ncol = 5)) # for a baseline
row.names(pred.summary2) <- names(trainset[,c(9:30)])
names(pred.summary2) <- c("Emotions only", "Prosody only", "Non-linguisstic only", "Linguisstic only", "All")

for (i in 21:21){

rf.all <- train(notunderstandable_understandable_avg ~ happ.avg+surpr.avg+sad.avg+
                  f0.avg+f0.max+f0.diff+
                  dur.speech+no.turns+no.tasks.compl+no.self.repeat+task.per.turn+
                  n.char.avg+w.count.avg+w.count.uniq.avg+lex.div.avg+n.sent.avg+wps.avg+wps.uniq.avg,
                 data = trainset,
                 method = "rf",
                 importance = TRUE,
                 ntree=100,
                 na.action=na.exclude,
                 trControl = trainControl(method= "cv", number=10, savePredictions = T))

# rf.emo <- train(fake_natural_avg ~ happ.avg+surpr.avg+sad.avg,
#                  data = trainset,
#                  method = "rf",
#                  importance = TRUE,
#                  ntree=100,
#                  na.action=na.exclude,
#                  trControl = trainControl(method= "cv", number=10, savePredictions = T))
# 
# rf.prosody <- train(fake_natural_avg ~ f0.avg+f0.max+f0.diff,
#                      data = trainset,
#                      method = "rf",
#                      importance = TRUE,
#                      ntree=100,
#                      na.action=na.exclude,
#                      trControl = trainControl(method= "cv", number=10, savePredictions = T))
# 
# rf.n_ling <- train(fake_natural_avg ~ 
#                       dur.speech+no.turns+no.tasks.compl+no.self.repeat+task.per.turn,
#                     data = trainset,
#                      method = "rf",
#                      ntree=100,
#                     na.action=na.exclude,
#                      trControl = trainControl(method= "cv", number=10, savePredictions = T))
# 
# rf.ling <- train(fake_natural_avg ~ 
#                     n.char.avg+w.count.avg+w.count.uniq.avg+lex.div.avg+n.sent.avg+wps.avg+wps.uniq.avg,
#                  data = trainset,
#                  method = "rf",
#                  importance = TRUE,
#                  ntree=100,
#                  na.action=na.exclude,
#                  trControl = trainControl(method= "cv", number=10, savePredictions = T))
# 
# pred.emo = round(predict(rf.emo, testset))
# pred.summary2[i,1] <- as.numeric(postResample(pred.emo, testset[,i+8])[1])
# pred.prosody = round(predict(rf.prosody, testset))
# pred.summary2[i,2] <- as.numeric(postResample(pred.prosody, testset[,i+8])[1])
# pred.n_ling = round(predict(rf.n_ling, testset))
# pred.summary2[i,3] <- as.numeric(postResample(pred.n_ling, testset[,i+8])[1])
# pred.ling = round(predict(rf.ling, testset))
# pred.summary2[i,4] <- as.numeric(postResample(pred.ling, testset[,i+8])[1])
# pred.all = round(predict(rf.all, testset))
pred.summary2[i,5] <- as.numeric(postResample(pred.all, testset[,i+8])[1])

#rm(rf.emo, rf.prosody, rf.n_ling, rf.ling, rf.all)

}


##### Plot cross-validation results ####

rf.n_ling <- train(obstructive_supportive ~ 
                     dur.speech+no.turns+no.tasks.compl+no.self.repeat+task.per.turn,
                   data = trainset,
                   method = "rf",
                   ntree=100,
                   na.action=na.exclude,
                   trControl = trainControl(method= "cv", number=10, savePredictions = T))

rf.ling_supp <- train(obstructive_supportive ~ 
                   n.char.avg+w.count.avg+w.count.uniq.avg+lex.div.avg+n.sent.avg+wps.avg+wps.uniq.avg,
                 data = trainset,
                 method = "rf",
                 importance = TRUE,
                 ntree=100,
                 na.action=na.exclude,
                 trControl = trainControl(method= "cv", number=10, savePredictions = T))
rf.ling_intel <- train(unintelligent_intelligent ~ 
                        n.char.avg+w.count.avg+w.count.uniq.avg+lex.div.avg+n.sent.avg+wps.avg+wps.uniq.avg,
                      data = trainset,
                      method = "rf",
                      importance = TRUE,
                      ntree=100,
                      na.action=na.exclude,
                      trControl = trainControl(method= "cv", number=10, savePredictions = T))

par(mfrow=c(2,1))
plot(rf.ling_supp)
par(new=TRUE)
plot(rf.ling_intel, col="red")


rf.dial <- train(foolish_sensible ~ 
                   dur.speech+no.turns+no.tasks.compl+no.self.repeat+task.per.turn+
                   n.char.avg+w.count.avg+w.count.uniq.avg+lex.div.avg+n.sent.avg+wps.avg+wps.uniq.avg,
                 data = trainset,
                 method = "rf",
                 importance = TRUE,
                 ntree=100,
                 na.action=na.exclude,
                 trControl = trainControl(method= "cv", number=10, savePredictions = T))
pred.ling = round(predict(rf.dial, testset))
postResample(pred.ling, testset$foolish_sensible)[1]
