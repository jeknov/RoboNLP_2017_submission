library(dplyr)
library(ggplot2)
library(sqldf)

full.data2 <- read.csv("ELAN ANNOTATIONS FILE URL")

pros <- read.csv("PROSODIC FEATURES FILE URL") # created using sound_analysis_v01.R

prosody = pros %>%
  select(subject, frames, f0, acf) %>%
  group_by(subject, frames) %>%
  summarise(f0 = mean(f0),
            acf = mean(acf))

# Now, I use a "faces" dataframe from face_annotations_analysis_v01.R:
faces$X <- NULL
faces$subejct <- as.character(faces$subejct)
prosody$subject <- as.character(prosody$subject)
prosody$frames <- as.integer(as.character(prosody$frames))

# library(sqldf)
# 
# full.data <- sqldf('select faces.*, prosody.f0, prosody.acf from 
#   faces left join outer prosody 
#   where faces.subejct = prosody.subject and faces.frame = prosody.frames')

full.data <- merge(x = faces, y = prosody, by.x = c("subejct", "frame"), 
                   by.y = c("subject", "frames"), all.x = TRUE)
full.data$f0 <- as.numeric(as.character(full.data$f0))
full.data$acf <- as.numeric(as.character(full.data$acf))

summary.by.actor = full.data %>%
  filter(!is.na(speaking_actor)) %>%
  group_by(speaking_actor, condition) %>%
  summarise(f0 = mean(f0, na.rm = TRUE),
            f0.sd = sd(f0, na.rm = TRUE),
            acf = mean(acf, na.rm = TRUE),
            acf.sd = sd(acf, na.rm = TRUE))
  
summary.by.actor <- as.data.frame(matrix(nrow=4, ncol = 8))
rownames(summary.by.actor)<-c("human speaking / chat", "human speaking / task",
                     "robot speaking / chat", "robot speaking / task")
names(summary.by.actor)<-c("happy.avg", "happy.sd", "surpr.avg", "surpr.sd", 
                  "f0.avg", "f0.sd", "acf.avg", "acf.sd")
summary.by.actor[1,1] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$happiness, na.rm=T)
summary.by.actor[1,2] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$happiness, na.rm=T)
summary.by.actor[1,3] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$surprise, na.rm=T)
summary.by.actor[1,4] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$surprise, na.rm=T)
summary.by.actor[1,5] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$f0, na.rm=T)
summary.by.actor[1,6] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$f0, na.rm=T)
summary.by.actor[1,7] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$acf, na.rm=T)
summary.by.actor[1,8] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "chat")$acf, na.rm=T)
summary.by.actor[2,1] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$happiness, na.rm=T)
summary.by.actor[2,2] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$happiness, na.rm=T)
summary.by.actor[2,3] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$surprise, na.rm=T)
summary.by.actor[2,4] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$surprise, na.rm=T)
summary.by.actor[2,5] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$f0, na.rm=T)
summary.by.actor[2,6] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$f0, na.rm=T)
summary.by.actor[2,7] <- mean(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$acf, na.rm=T)
summary.by.actor[2,8] <- sd(subset(full.data, speaking_actor=="human_speaking" & condition == "task")$acf, na.rm=T)
summary.by.actor[3,1] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$happiness, na.rm=T)
summary.by.actor[3,2] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$happiness, na.rm=T)
summary.by.actor[3,3] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$surprise, na.rm=T)
summary.by.actor[3,4] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$surprise, na.rm=T)
summary.by.actor[3,5] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$f0, na.rm=T)
summary.by.actor[3,6] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$f0, na.rm=T)
summary.by.actor[3,7] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$acf, na.rm=T)
summary.by.actor[3,8] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "chat")$acf, na.rm=T)
summary.by.actor[4,1] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$happiness, na.rm=T)
summary.by.actor[4,2] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$happiness, na.rm=T)
summary.by.actor[4,3] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$surprise, na.rm=T)
summary.by.actor[4,4] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$surprise, na.rm=T)
summary.by.actor[4,5] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$f0, na.rm=T)
summary.by.actor[4,6] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$f0, na.rm=T)
summary.by.actor[4,7] <- mean(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$acf, na.rm=T)
summary.by.actor[4,8] <- sd(subset(full.data, speaking_actor=="robot_speaking" & condition == "task")$acf, na.rm=T)

write.csv(full.data, "full_data.csv", row.names = F)

prosody_scores.by.subj = full.data2 %>%
  filter(!is.na(speaking_actor)) %>%
  group_by(subejct) %>%
  summarise(happ.avg = mean(happiness),
            surpise.avf = mean(surprise),
            f0.avg = mean(f0, na.rm = T),
            acf.avg = mean(acf, na.rm = T))


lm(ts(subset(full.data, subejct=="005_C" & speaking_actor=="human_speaking")$happiness, 
      frequency = 25*10)~time(ts(subset(full.data, subejct=="005_T" & speaking_actor=="human_speaking")$happiness, 
                                 frequency = 25*10)))$coefficients[2]  
lm(ts(subset(full.data, subejct=="005_C" & speaking_actor=="robot_speaking")$happiness, 
      frequency = 25*10)~time(ts(subset(full.data, subejct=="005_T" & speaking_actor=="robot_speaking")$happiness, 
                                 frequency = 25*10)))$coefficients[2]  
    

lm(ts(subset(full.data, subejct=="005_C")$happiness, 
      frequency = 25*60)~time(ts(subset(full.data, subejct=="005_C")$happiness, 
                                 frequency = 25*60)))$coefficients[2]  
lm(ts(subset(full.data, subejct=="005_T")$happiness, 
      frequency = 25*60)~time(ts(subset(full.data, subejct=="005_T")$happiness, 
                                 frequency = 25*60)))$coefficients[2]  

            #trend.happ = mk.test(ts(happiness))$pvalue,
            #slope.happ = lm(ts(happiness)~time(happiness))$coefficients[2],
            #poch.happ = pettitt.test(ts(happiness))$estimate,
            #trend.surp = mk.test(ts(surprise))$pvalue,
            #slope.surpr = lm(ts(surprise)~time(surprise))$coefficients[2],
            #poch.surpr = pettitt.test(ts(surprise))$estimate
            #)


##### Significance of differences #####
  
fit.hap <- aov(happiness ~ condition*speaking_actor, data=full.data) # 2-way repeated measures anova
summary(fit.hap) # both condition and actor influence happiness. chat and robot have higher happ.
t.test(subset(full.data, speaking_actor=="human_speaking")$happiness, subset(full.data, speaking_actor=="robot_speaking")$happiness)
t.test(subset(full.data, condition=="chat")$happiness, subset(full.data, condition=="task")$happiness)

fit.hap2 <- aov(happiness ~ condition*pauses, data=full.data) # 2-way repeated measures anova
summary(fit.hap2) # both condition and pauses influence happiness. chat and pause after robot - higher
t.test(subset(full.data, pauses=="pause")$happiness, subset(full.data, pauses=="pause after robot")$happiness)
t.test(subset(full.data, condition=="chat")$happiness, subset(full.data, condition=="task")$happiness)

fit.surpr <- aov(surprise ~ condition*speaking_actor, data=full.data) # 2-way repeated measures anova
summary(fit.surpr)  # only speaking actor influences surprise. human - higher values
t.test(subset(full.data, speaking_actor=="human_speaking")$surprise, subset(full.data, speaking_actor=="robot_speaking")$surprise)
t.test(subset(full.data, condition=="chat")$surprise, subset(full.data, condition=="task")$surprise)

fit.surpr2 <- aov(surprise ~ condition*pauses, data=full.data) # 2-way repeated measures anova
summary(fit.surpr2)  # both condition and pauses influences surprise. task and pause after robot - higher.
t.test(subset(full.data, pauses=="pause")$surprise, subset(full.data, pauses=="pause after robot")$surprise)
t.test(subset(full.data, condition=="chat")$surprise, subset(full.data, condition=="task")$surprise)

fit.f0 <- aov(f0 ~ condition*speaking_actor, data=full.data) # 2-way repeated measures anova
summary(fit.f0) # both condition and actor (human vs robot) influence f0. robot and task - higher f0
t.test(subset(full.data, speaking_actor=="human_speaking")$f0, subset(full.data, speaking_actor=="robot_speaking")$f0)
t.test(subset(full.data, condition=="chat")$f0, subset(full.data, condition=="task")$f0)


corrplot::corrplot.mixed(cor(subset(full.data, speaking_actor=="robot_speaking")[,c(10,16,22)], method = "spearman", use="complete.obs"),
                         lower = "circle", upper = "number", 
                         main = "How human speech correlates with perception of robot", mar=c(0,0,1,0),
                         tl.cex = .8, number.cex = 0.8, number.digits = 1,
                         col=colorRampPalette(c("red","grey45","blue"))(200))









