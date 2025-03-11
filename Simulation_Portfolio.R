Matrikelnummern = c(400421373,
                    400451856,
                    400928934,
                    400292045,
                    400453062,
                    400803412,
                    400789036,
                    400458177,
                    400828014,
                    400666957,
                    400433640,
                    400935642,
                    400420947,
                    400279771,
                    400900235)

roundallnumerics = function(df, digits){
  for(j in 1:ncol(df)){
    if(is.numeric(df[,j])){
      df[,j] = round(df[,j], digits)
    }
  }
  return(df)
}

n = 200

for (m in Matrikelnummern) {
  
  print(m)
  set.seed(m)
  
  data = data.frame(subject = rep(as.character(1:n), each = 5),
                    sex = NA,
                    age = NA,
                    time = rep(0:4, n),
                    group = c(rep(0, n/2 * 5), rep(1, n/2 * 5)))
  
  for (ch in unique(data$subject)) {
    data$sex[data$subject == ch] = sample(c(0,1), size = 1, prob = c(0.5, 0.5))
    data$age[data$subject == ch] = round(rnorm(1, 45, 15))
  }
  
  data$group = factor(data$group, levels = 0:1, labels = c("Warteliste", "KVT"))
  data$sex = factor(data$sex, levels = 0:1, labels = c("männlich", "weiblich"))
  data$age[data$age < 18] = 18
  
  data_wide = data[data$time == 0,]
  
  problemfokus = c(1, 6, 9)
  kontrollverlust = c(2, 4, 11)
  vergangenheitsorientierung = c(3, 5, 7, 8, 10)
  alltag = c(12, 13, 14)
  
  data_wide[, paste0("GNS", problemfokus[1])] = pmax(pmin(round(rnorm(n, mean = 1.1, sd = 1)), 5), 1) 
  data_wide[, paste0("GNS", kontrollverlust[1])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", problemfokus[1])], mu = 1.1, sd = 1, r = .45), 5), 1) 
  data_wide[, paste0("GNS", vergangenheitsorientierung[1])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", problemfokus[1])], mu = 1.1, sd = 1, r = .40), 5), 1) 
  data_wide[, paste0("GNS", alltag[1])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", problemfokus[1])], mu = 1.1, sd = 1, r = .41), 5), 1) 
  
  data_wide[, paste0("GNS", problemfokus[2])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", problemfokus[1])], mu = 1.1, sd = 1, r = .8), 5), 1) 
  data_wide[, paste0("GNS", problemfokus[3])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", problemfokus[1])], mu = 1.1, sd = 1, r = .85), 5), 1) 
  
  data_wide[, paste0("GNS", kontrollverlust[2])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", kontrollverlust[1])], mu = 1.1, sd = 1, r = .8), 5), 1) 
  data_wide[, paste0("GNS", kontrollverlust[3])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", kontrollverlust[1])], mu = 1.1, sd = 1, r = .91), 5), 1) 
  
  data_wide[, paste0("GNS", vergangenheitsorientierung[2])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", vergangenheitsorientierung[1])], mu = 1.1, sd = 1, r = .79), 5), 1) 
  data_wide[, paste0("GNS", vergangenheitsorientierung[3])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", vergangenheitsorientierung[1])], mu = 1.1, sd = 1, r = .82), 5), 1) 
  data_wide[, paste0("GNS", vergangenheitsorientierung[4])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", vergangenheitsorientierung[1])], mu = 1.1, sd = 1, r = .81), 5), 1) 
  data_wide[, paste0("GNS", vergangenheitsorientierung[5])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", vergangenheitsorientierung[1])], mu = 1.1, sd = 1, r = .93), 5), 1) 
  
  data_wide[, paste0("GNS", alltag[2])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", alltag[1])], mu = 1.1, sd = 1, r = .81), 5), 1) 
  data_wide[, paste0("GNS", alltag[3])] = pmax(pmin(faux::rnorm_pre(data_wide[, paste0("GNS", alltag[1])], mu = 1.1, sd = 1, r = .52), 5), 1) 
  
  data_wide = roundallnumerics(data_wide)
  
  data_wide$GNS = rowSums(data_wide[,grep("GNS[1-9]", names(data_wide))])
  
  for(ch in unique(data$subject[data$group == "Warteliste"])){
    data$GNS[data$subject == ch] = round(data_wide$GNS[data_wide$subject == ch]  - 1.5 * data$time[data$subject == ch] + rnorm(n = 5, mean = 0, sd = 4)) 
  }
  for(ch in unique(data$subject[data$group == "KVT"])){
    data$GNS[data$subject == ch] = round(data_wide$GNS[data_wide$subject == ch] - 2 * data$time[data$subject == ch] + rnorm(n = 5, mean = 0, sd = 4)) 
  }
  
  data$GNS[data$GNS < 0] = 0
  
  while (summary(lmer(GNS ~ time * group + (1|subject), data = data))$coefficients["time:groupKVT", "Pr(>|t|)"] >= .05) {
    for(ch in unique(data$subject[data$group == "Warteliste"])){
      data$GNS[data$subject == ch] = round(data_wide$GNS[data_wide$subject == ch] - 1.5 * data$time[data$subject == ch] + rnorm(n = 5, mean = 0, sd = 4)) 
    }
    for(ch in unique(data$subject[data$group == "KVT"])){
      data$GNS[data$subject == ch] = round(data_wide$GNS[data_wide$subject == ch] - 2 * data$time[data$subject == ch] + rnorm(n = 5, mean = 0, sd = 4)) 
    }
  }
  
  data$time = data$time * 2
  
  data_wide$change = NA
  data_wide$KFS20 = NA
  data_wide$SMI15 = NA
  data_wide$cluster = NA
  
  for (ch in unique(data_wide$subject)) {
    data_wide$change[data_wide$subject == ch] = data$GNS[data$subject == ch & data$time == 0] - data$GNS[data$subject == ch & data$time == 8]
  }
  
  data_wide$cluster = factor(sample(c(1:3), size = n, prob = c(1/3, 1/3, 1/3), replace = T))
  
  for (ch in unique(data_wide$subject)) {
    data$cluster[data$subject == ch] = as.character(data_wide$cluster[data_wide$subject == ch])
  }
  
  while(anova(lmer(GNS ~ time * cluster + (1|subject), data = data[data$group == "KVT",]))["time:cluster","Pr(>F)"] >= .05){
    data_wide$cluster = factor(sample(c(1:3), size = n, prob = c(1/3, 1/3, 1/3), replace = T))
    
    for (ch in unique(data_wide$subject)) {
      data$cluster[data$subject == ch] = as.character(data_wide$cluster[data_wide$subject == ch])
    }
  }
  
  emt = as.data.frame(emmeans::emtrends(lmer(GNS ~ time * cluster + (1|subject), data = data), pairwise ~ cluster, var = "time")$emtrends)
  
  highest = emt$cluster[order(emt$time.trend)[1]]
  med = emt$cluster[order(emt$time.trend)[2]]
  lowest = emt$cluster[order(emt$time.trend)[3]]
  
  data_wide$KFS20[data_wide$cluster == highest] = round(rnorm(n = length(data_wide$KFS20[data_wide$cluster == highest]), mean = 25, sd = 2))
  data_wide$SMI15[data_wide$cluster == highest] = round(rnorm(n = length(data_wide$SMI15[data_wide$cluster == highest]), mean = 20, sd = 2))
  
  data_wide$KFS20[data_wide$cluster == med] = round(rnorm(n = length(data_wide$KFS20[data_wide$cluster == med]), mean = 25, sd = 2))
  data_wide$SMI15[data_wide$cluster == med] = round(rnorm(n = length(data_wide$SMI15[data_wide$cluster == med]), mean = 6, sd = 2))
  
  data_wide$KFS20[data_wide$cluster == lowest] = round(rnorm(n = length(data_wide$KFS20[data_wide$cluster == lowest]), mean = 6, sd = 2))
  data_wide$SMI15[data_wide$cluster == lowest] = round(rnorm(n = length(data_wide$SMI15[data_wide$cluster == lowest]), mean = 6, sd = 2))
  
  data_wide$KFS20[data_wide$KFS20 < 0] = 0
  data_wide$SMI15[data_wide$SMI15 < 0] = 0
  
  data_wide$MVMS = NA
  data_wide$LQI25 = NA
  
  
  data_wide$MVMS <- round(10 + 0.35 * data_wide$SMI15 + rnorm(200, mean = 0, sd = 2))
  data_wide$LQI25 <- round(0.23 * data_wide$MVMS + rnorm(200, mean = 0, sd = 2))
  data_wide$MVMS[data_wide$MVMS < 0] = 0
  data_wide$LQI25[data_wide$LQI25 < 0] = 0
  
  pe = parameterestimates(sem(model, data = data_wide[data_wide$group == "KVT",], se = "bootstrap", bootstrap = 1000))
  
  while (pe$pvalue[pe$lhs == "ab"] >= .05 | pe$pvalue[pe$lhs == "total"] >= .05) {
    data_wide$MVMS <- round(10 + 0.35 * data_wide$SMI15 + rnorm(200, mean = 0, sd = 2))
    data_wide$LQI25 <- round(0.23 * data_wide$MVMS + rnorm(200, mean = 0, sd = 2))
    data_wide$MVMS[data_wide$MVMS < 0] = 0
    data_wide$LQI25[data_wide$LQI25 < 0] = 0
    
    pe = parameterestimates(sem(model, data = data_wide[data_wide$group == "KVT",], se = "bootstrap", bootstrap = 1000))
  }
  
  for (ch in data_wide$subject) {
    
    for (item in names(data_wide[,grep("GNS[1-9]", names(data_wide))])) {
      data[data$time == 0 & data$subject == ch, item] = data_wide[data_wide$subject == ch, item]
    }
    
    data$KFS20[data$time == 0 & data$subject == ch] = data_wide$KFS20[data_wide$subject == ch]
    data$SMI15[data$time == 0 & data$subject == ch] = data_wide$SMI15[data_wide$subject == ch]
    data$MVMS[data$time == 0 & data$subject == ch] = data_wide$MVMS[data_wide$subject == ch]
    data$LQI25[data$time == 8 & data$subject == ch] = data_wide$LQI25[data_wide$subject == ch]
  }
  
  data_wide = BBmisc::dropNamed(data_wide, drop = c("change", "cluster"))
  data = BBmisc::dropNamed(data, drop = c("cluster"))
  
  data = data[order(data$subject),]
  
  write.csv(data, paste0("/Users/stephangoerigk/Desktop/Universität/CFH/Lehre/Bachelor/Empirisch-wissenschaftliches Arbeiten/VO_Wissenschaftliches Arbeiten und Forschungsmethoden/WAF_Folien/Daten_MVV/", m, "_long.csv"))
  write.csv(data_wide, paste0("/Users/stephangoerigk/Desktop/Universität/CFH/Lehre/Bachelor/Empirisch-wissenschaftliches Arbeiten/VO_Wissenschaftliches Arbeiten und Forschungsmethoden/WAF_Folien/Daten_MVV/", m, "_wide.csv"))
}













##########################

summary(lmer(GNS ~ time * group + (1|subject), data = data))

ggplot(data, aes(y = GNS, x = time, colour = group)) +
  stat_summary()+
  stat_summary(geom = "line")

KMO(data_wide[,grep("GNS[1-9]", names(data_wide))])

fa(r = cor(data_wide[,grep("GNS[1-9]", names(data_wide))]), 
   nfactors = 4, 
   fm = "ml", 
   rotate = "promax")$loadings

mean(data_wide$change[data_wide$group == "KVT"])

ggplot(data_wide, aes(y = KFS20, x = SMI15, colour = cluster)) +
 geom_point()

d <- dist(data_wide[,c("KFS20", "SMI15")], method = "euclidean")
hc <- hclust(d = d, method = "ward.D2")
fviz_nbclust(x = data_wide[,c("KFS20", "SMI15")], 
             FUN = hcut, 
             method = c("silhouette"))


model <- ' # direct effect
             LQI25 ~ c*SMI15
           # mediator
             MVMS ~ a*SMI15
             LQI25 ~ b*MVMS
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
fit <- sem(model, data = data_wide[data_wide$group == "KVT",], se = "bootstrap", bootstrap = 100)
summary(fit)

data$GNS[data$time == 0] - data$GNS[data$time == 8] > data$GNS[data$time == 0] * 0.5



