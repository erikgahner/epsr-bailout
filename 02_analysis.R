###
##
## Article:   Bailout or bust? Government evaluations in the wake of a bailout
## 
##            European Political Science Review
##
##            Erik Gahner Larsen          Robert Klemmensen       Michael Baggesen Klitgaard
##            E.G.Larsen@kent.ac.uk       rkl@sam.sdu.dk          mbk@sam.sdu.dk
##
##        
## Data:      European Social Survey: http://www.europeansocialsurvey.org/
##            Longitudinal Internet Studies for the Social Sciences: https://lissdata.nl
##            Eurobarometer: http://ec.europa.eu/commfrontoffice/publicopinion/
##
###

library("tidyverse")
library("rio")
library("lme4")
library("stargazer")
library("interplot")
library("grid")
library("gridExtra")
library("Matching")
library("ebal")
library("scales")
library("interflex")

# No scientific notation in print output
options(scipen=999)

# Set select() to dplyr::select as default
select <- dplyr::select

# Set theme options
theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

set.seed(50)

coverage <- read.csv("data_coverage.csv")

# Figure 1: Draw and save media coverage figure
pdf('fig1-mediacoverage.pdf', height=4, width=4)
ggplot(coverage, aes(week, articles)) + 
  scale_y_continuous(name="Frequency of articles") +
  geom_line(linetype="dashed", col="#000000") +
  geom_line(aes(coverage$week[coverage$week_0==0],coverage$articles[coverage$week_0==0]), col="#000000") +
  geom_line(aes(coverage$week[coverage$week_1==1],coverage$articles[coverage$week_1==1]), col="#000000") +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) + 
  scale_x_continuous(name="Week", breaks = 36:44, labels=36:44) 
dev.off()

png("fig1-mediacoverage.tiff", height=1200, width=1200, res = 300)
ggplot(coverage, aes(week, articles)) + 
  scale_y_continuous(name="Frequency of articles") +
  geom_line(linetype="dashed", col="#000000") +
  geom_line(aes(coverage$week[coverage$week_0==0],coverage$articles[coverage$week_0==0]), col="#000000") +
  geom_line(aes(coverage$week[coverage$week_1==1],coverage$articles[coverage$week_1==1]), col="#000000") +
  geom_point(size = 6, colour = "white") + 
  geom_point(size = 3, shape = 1) + 
  scale_x_continuous(name="Week", breaks = 36:44, labels=36:44) 
dev.off()

# Load data sets
ess <- read.csv("data_ess.csv")
eb <- read.csv("data_eb.csv")
liss <- read.csv("data_liss.csv")

# Descriptive statistics reported in Table 1
## ESS
ess %>%
  group_by(bailout) %>%
  summarise(stfeco_mean = mean(stfeco, na.rm=TRUE),
            stfeco_sd = sd(stfeco, na.rm=TRUE),
            stfgov_mean = mean(stfgov, na.rm=TRUE),
            stfgov_sd = sd(stfgov, na.rm=TRUE)
            )


## LISS
mean(liss$cv08a043, na.rm=TRUE)
sd(liss$cv08a043, na.rm=TRUE)

mean(liss$cv09b043, na.rm=TRUE)
sd(liss$cv09b043, na.rm=TRUE)

mean(liss$cv08a030, na.rm=TRUE)
sd(liss$cv08a030, na.rm=TRUE)

mean(liss$cv09b030, na.rm=TRUE)
sd(liss$cv09b030, na.rm=TRUE)

liss.long.eco <- gather(liss, bailout, stfeco, c(cv08a043, cv09b043)) %>% filter(stfeco >= 0)
liss.long.gov <- gather(liss, bailout, stfgov, c(cv08a030, cv09b030)) %>% filter(stfgov >= 0)

liss.long.eco$bailout <- ifelse(liss.long.eco$bailout == "cv09b043", 1, 0)
liss.long.gov$bailout <- ifelse(liss.long.gov$bailout == "cv09b030", 1, 0)

res.d.df <- data.frame(
  data = c("ESS","ESS","LISS","LISS"),
  sat = c("Economy","Government","Economy","Government"),
  effect = c(
    mean(ess$stfeco[ess$bailout==1], na.rm=T) - mean(ess$stfeco[ess$bailout==0], na.rm=T),
    mean(ess$stfgov[ess$bailout==1], na.rm=T) - mean(ess$stfgov[ess$bailout==0], na.rm=T),
    mean(liss$cv09b043, na.rm=T) - mean(liss$cv08a043, na.rm=T),
    mean(liss$cv09b030, na.rm=T) - mean(liss$cv08a030, na.rm=T)   
  ),
  se = c(
    sqrt(sd(ess$stfeco[ess$bailout==1], na.rm=T)*sd(ess$stfeco[ess$bailout==1], na.rm=T)/NROW(na.omit(ess$stfeco[ess$bailout==1]))+sd(ess$stfeco[ess$bailout==0], na.rm=T)*sd(ess$stfeco[ess$bailout==0], na.rm=T)/NROW(na.omit(ess$stfeco[ess$bailout==0]))),
    sqrt(sd(ess$stfgov[ess$bailout==1], na.rm=T)*sd(ess$stfgov[ess$bailout==1], na.rm=T)/NROW(na.omit(ess$stfgov[ess$bailout==1]))+sd(ess$stfgov[ess$bailout==0], na.rm=T)*sd(ess$stfgov[ess$bailout==0], na.rm=T)/NROW(na.omit(ess$stfgov[ess$bailout==0]))),
    sqrt(sd(liss$cv09b043, na.rm=T)*sd(liss$cv09b043, na.rm=T)/NROW(na.omit(liss$cv09b043))+sd(liss$cv08a043, na.rm=T)*sd(liss$cv08a043, na.rm=T)/NROW(na.omit(liss$cv08a043))),
    sqrt(sd(liss$cv09b030, na.rm=T)*sd(liss$cv09b030, na.rm=T)/NROW(na.omit(liss$cv09b030))+sd(liss$cv08a030, na.rm=T)*sd(liss$cv08a030, na.rm=T)/NROW(na.omit(liss$cv08a030)))
  )
)

# Create figure with average treatment effects
pdf("fig3-ate.pdf", width=4, height=4)
ggplot(res.d.df, aes(x=sat, y=effect, group=data, shape=data)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=effect-1.645*se, ymax=effect+1.645*se), position=position_dodge(width=0.3), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se), position=position_dodge(width=0.3), colour="black", width=0) + 
  geom_point(position=position_dodge(width=0.3), size = 3, shape=16, colour = "white") + 
  geom_point(position=position_dodge(width=0.3), size = 2) + 
  ylim(-.85,.85) +
  scale_shape_manual(values=c(1, 2)) +
  ylab("Effect of bailout") +
  xlab("") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()

png("fig3-ate.tiff", height=1200, width=1200, res = 300)
ggplot(res.d.df, aes(x=sat, y=effect, group=data, shape=data)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=effect-1.645*se, ymax=effect+1.645*se), position=position_dodge(width=0.3), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se), position=position_dodge(width=0.3), colour="black", width=0) + 
  geom_point(position=position_dodge(width=0.3), size = 3, shape=16, colour = "white") + 
  geom_point(position=position_dodge(width=0.3), size = 2) + 
  ylim(-.85,.85) +
  scale_shape_manual(values=c(1, 2)) +
  ylab("Effect of bailout") +
  xlab("") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()

# Placebo tests

placebo_df <- data.frame(
  data = c("ESS","ESS","LISS", "LISS", "LISS", "LISS", "LISS", "LISS", "LISS", "LISS"),
  sat = c("Life","Democracy","Media", "Military", "Education\n system", "Healthcare", "Science", "Democracy", "Shops", "Internet\n shops"),
  effect = c(
    mean(ess$stflife[ess$bailout==1], na.rm=T) - mean(ess$stflife[ess$bailout==0], na.rm=T),
    mean(ess$stfdem[ess$bailout==1], na.rm=T) - mean(ess$stfdem[ess$bailout==0], na.rm=T),
    # Satisfaction: the media
    mean(liss$cv09b038, na.rm=T) - mean(liss$cv08a038, na.rm=T),
    # Satisfaction: the military
    mean(liss$cv09b039, na.rm=T) - mean(liss$cv08a039, na.rm=T),
    # Satisfaction: the education system
    mean(liss$cv09b040, na.rm=T) - mean(liss$cv08a040, na.rm=T),
    # Satisfaction: healthcare
    mean(liss$cv09b041, na.rm=T) - mean(liss$cv08a041, na.rm=T),
    # Satisfaction: science
    mean(liss$cv09b042, na.rm=T) - mean(liss$cv08a042, na.rm=T),
    # Satisfaction: democracy
    mean(liss$cv09b044, na.rm=T) - mean(liss$cv08a044, na.rm=T),
    # Satisfaction: shops/firms that you deal with personally (that you visit in person)
    mean(liss$cv09b045, na.rm=T) - mean(liss$cv08a045, na.rm=T),
    # Satisfaction: shops/firms on the Internet
    mean(liss$cv09b046, na.rm=T) - mean(liss$cv08a046, na.rm=T)
  ),
  se = c(
    sqrt(sd(ess$stflife[ess$bailout==1], na.rm=T)*sd(ess$stflife[ess$bailout==1], na.rm=T)/NROW(na.omit(ess$stflife[ess$bailout==1]))+sd(ess$stflife[ess$bailout==0], na.rm=T)*sd(ess$stflife[ess$bailout==0], na.rm=T)/NROW(na.omit(ess$stflife[ess$bailout==0]))),
    sqrt(sd(ess$stfdem[ess$bailout==1], na.rm=T)*sd(ess$stfdem[ess$bailout==1], na.rm=T)/NROW(na.omit(ess$stfdem[ess$bailout==1]))+sd(ess$stfdem[ess$bailout==0], na.rm=T)*sd(ess$stfdem[ess$bailout==0], na.rm=T)/NROW(na.omit(ess$stfdem[ess$bailout==0]))),
    sqrt(sd(liss$cv09b038, na.rm=T)*sd(liss$cv09b038, na.rm=T)/NROW(na.omit(liss$cv09b038))+sd(liss$cv08a038, na.rm=T)*sd(liss$cv08a038, na.rm=T)/NROW(na.omit(liss$cv08a038))),
    sqrt(sd(liss$cv09b039, na.rm=T)*sd(liss$cv09b039, na.rm=T)/NROW(na.omit(liss$cv09b039))+sd(liss$cv08a039, na.rm=T)*sd(liss$cv08a039, na.rm=T)/NROW(na.omit(liss$cv08a039))),
    sqrt(sd(liss$cv09b040, na.rm=T)*sd(liss$cv09b040, na.rm=T)/NROW(na.omit(liss$cv09b040))+sd(liss$cv08a040, na.rm=T)*sd(liss$cv08a040, na.rm=T)/NROW(na.omit(liss$cv08a040))),
    sqrt(sd(liss$cv09b041, na.rm=T)*sd(liss$cv09b041, na.rm=T)/NROW(na.omit(liss$cv09b041))+sd(liss$cv08a041, na.rm=T)*sd(liss$cv08a041, na.rm=T)/NROW(na.omit(liss$cv08a041))),
    sqrt(sd(liss$cv09b042, na.rm=T)*sd(liss$cv09b042, na.rm=T)/NROW(na.omit(liss$cv09b042))+sd(liss$cv08a042, na.rm=T)*sd(liss$cv08a042, na.rm=T)/NROW(na.omit(liss$cv08a042))),
    sqrt(sd(liss$cv09b044, na.rm=T)*sd(liss$cv09b044, na.rm=T)/NROW(na.omit(liss$cv09b044))+sd(liss$cv08a044, na.rm=T)*sd(liss$cv08a044, na.rm=T)/NROW(na.omit(liss$cv08a044))),
    sqrt(sd(liss$cv09b045, na.rm=T)*sd(liss$cv09b045, na.rm=T)/NROW(na.omit(liss$cv09b045))+sd(liss$cv08a045, na.rm=T)*sd(liss$cv08a045, na.rm=T)/NROW(na.omit(liss$cv08a045))),
    sqrt(sd(liss$cv09b046, na.rm=T)*sd(liss$cv09b046, na.rm=T)/NROW(na.omit(liss$cv09b046))+sd(liss$cv08a046, na.rm=T)*sd(liss$cv08a046, na.rm=T)/NROW(na.omit(liss$cv08a046)))
  )
)

# Create figure with placebo tests
pdf("fig6-placebo.pdf", width=7, height=5)
ggplot(placebo_df, aes(x=sat, y=effect, group=data, shape=data)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=effect-1.645*se, ymax=effect+1.645*se), position=position_dodge(width=0.3), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se), position=position_dodge(width=0.3), colour="black", width=0) + 
  geom_point(position=position_dodge(width=0.3), size = 3, shape=16, colour = "white") + 
  geom_point(position=position_dodge(width=0.3), size = 2) + 
  ylim(-.85,.85) +
  scale_shape_manual(values=c(1, 2)) +
  ylab("Effect of bailout") +
  xlab("") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()

png("fig6-placebo.tiff", width=2100, height=1500, res = 300)
ggplot(placebo_df, aes(x=sat, y=effect, group=data, shape=data)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=effect-1.645*se, ymax=effect+1.645*se), position=position_dodge(width=0.3), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se), position=position_dodge(width=0.3), colour="black", width=0) + 
  geom_point(position=position_dodge(width=0.3), size = 3, shape=16, colour = "white") + 
  geom_point(position=position_dodge(width=0.3), size = 2) + 
  ylim(-.85,.85) +
  scale_shape_manual(values=c(1, 2)) +
  ylab("Effect of bailout") +
  xlab("") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
dev.off()

# Income analysis

lm.income.eco.ess <- lm(stfeco ~ bailout*income, data=ess)
lm.income.gov.ess <- lm(stfgov ~ bailout*income, data=ess)
lm.income.eco.liss <- lm(eco ~ income, data=liss)
lm.income.gov.liss <- lm(gov ~ income, data=liss)

stargazer(lm.income.eco.ess, lm.income.gov.ess, lm.income.eco.liss, lm.income.gov.liss,
            covariate.labels = c("Bailout", "Income", "Bailout * Income"),
            column.labels=c("Economy", "Government", "Economy", "Government"),
            keep.stat = c("n","adj.rsq"),
            title="Heterogeneous response to bailout, income, OLS",
            model.names=FALSE,
            type="text", out="tab2-income.htm")

reg_income_eco_marg <- interplot(m = lm.income.eco.ess, var1 = "bailout", var2 = "income", plot=FALSE)

fig_income_eco <- ggplot(reg_income_eco_marg, aes(x = income)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  scale_y_continuous(name="Marginal effect of bailout", limits = c(-1.5,1.5)) +
  ggtitle("(A) Economic evaluations") 

reg_income_gov_marg <- interplot(m = lm.income.gov.ess, var1 = "bailout", var2 = "income", plot=FALSE)

fig_income_gov <- ggplot(reg_income_gov_marg, aes(x = income)) +
  geom_hline(yintercept=0, colour="gray80") +
  geom_line(aes(y = coef)) +
  geom_line(aes(y = ub), linetype=2) +
  geom_line(aes(y = lb), linetype=2) +
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  scale_y_continuous(name="Marginal effect of bailout", limits = c(-1.5,1.5)) +
  ggtitle("(B) Government evaluations")

pdf("fig4-income.pdf", width=8, height=4)
grid.arrange(fig_income_eco, fig_income_gov, ncol=2)
dev.off()

png("fig4-income.tiff", width=2400, height=1200, res = 300)
grid.arrange(fig_income_eco, fig_income_gov, ncol=2)
dev.off()


inter_raw_stfeco <- inter.raw(Y = "stfeco", D = "Bailout", X = "income", data = ess)$graph
inter_raw_stfgov <- inter.raw(Y = "stfgov", D = "Bailout", X = "income", data = ess)$graph

inter_raw_stfeco <- inter_raw_stfeco + 
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  labs(y = "Economic evaluations")

inter_raw_stfgov <- inter_raw_stfgov + 
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  labs(y = "Government evaluations")

pdf("figE-raw.pdf", width=8, height=5)
grid.arrange(inter_raw_stfeco, inter_raw_stfgov, ncol=2)
dev.off()


inter_bin_stfeco <- inter.binning(Y = "stfeco", D = "bailout", X = "income", data = ess, vartype = "robust", main = "Marginal Effects", na.rm=TRUE)$graph
inter_bin_stfgov <- inter.binning(Y = "stfgov", D = "bailout", X = "income", data = ess, vartype = "robust", main = "Marginal Effects", na.rm=TRUE)$graph

inter_bin_stfeco <- inter_bin_stfeco + 
  theme(plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15))) +
  geom_hline(yintercept=0, colour="gray60") +
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  labs(title = "(A) Economic evaluations",
       y = "Marginal effect of bailout") 
  
inter_bin_stfgov <- inter_bin_stfgov + 
  theme(plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15))) +
  geom_hline(yintercept=0, colour="gray60") +
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  labs(title = "(B) Government evaluations",
       y = "Marginal effect of bailout") 

pdf("figE-bin.pdf", width=8, height=5)
grid.arrange(inter_bin_stfeco, inter_bin_stfgov, ncol=2)
dev.off()

inter_kernel_stfeco <- inter.kernel(Y = "stfeco", D = "bailout", X = "income", data = ess, na.rm=TRUE)$graph
inter_kernel_stfgov <- inter.kernel(Y = "stfgov", D = "bailout", X = "income", data = ess, na.rm=TRUE)$graph

inter_kernel_stfeco <- inter_kernel_stfeco +  
  geom_hline(yintercept=0, colour="gray60") +
  scale_x_continuous(name="Moderator: Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  labs(title = "(A) Economic evaluations",
       y = "Marginal effect of bailout") 

inter_kernel_stfgov <- inter_kernel_stfgov +
  geom_hline(yintercept=0, colour="gray60") +
  scale_x_continuous(name="Income", breaks=1:10, labels=c("Low", rep("", 8), "High")) +
  labs(title = "(B) Government evaluations",
       y = "Marginal effect of bailout") 

pdf("figE-kernel.pdf", width=8, height=4)
grid.arrange(inter_kernel_stfeco, inter_kernel_stfgov, ncol=2)
dev.off()

# Party analysis
ess.eco.sup <- lm(stfeco~bailout,data=ess[ess$government == 1,])
ess.eco.opp <- lm(stfeco~bailout,data=ess[ess$government == 0,])
liss.eco.sup <- lmer(stfeco~bailout + (1 | nomem_encr), data=liss.long.eco[liss.long.eco$government == 1,])
liss.eco.opp <- lmer(stfeco~bailout + (1 | nomem_encr), data=liss.long.eco[liss.long.eco$government == 0,])
ess.gov.sup <- lm(stfgov~bailout,data=ess[ess$government == 1,])
ess.gov.opp <- lm(stfgov~bailout,data=ess[ess$government == 0,])
liss.gov.sup <- lmer(stfgov~bailout + (1 | nomem_encr), data=liss.long.gov[liss.long.gov$government == 1,])
liss.gov.opp <- lmer(stfgov~bailout + (1 | nomem_encr), data=liss.long.gov[liss.long.gov$government == 0,])

political_df <- data.frame(
  data = c("ESS","ESS","LISS","LISS", "ESS","ESS","LISS","LISS"),
  variable = c(rep("Economy", 4), rep("Government", 4)),
  sup = c(rep(c("Government\n supporter","Opposition\n supporter",
                "Government\n supporter","Opposition\n supporter"),2)),
  effect = c(
    coef(summary(ess.eco.sup))["bailout","Estimate"],
    coef(summary(ess.eco.opp))["bailout","Estimate"],
    coef(summary(liss.eco.sup))["bailout","Estimate"],
    coef(summary(liss.eco.opp))["bailout","Estimate"],
    coef(summary(ess.gov.sup))["bailout","Estimate"],
    coef(summary(ess.gov.opp))["bailout","Estimate"],
    coef(summary(liss.gov.sup))["bailout","Estimate"],
    coef(summary(liss.gov.opp))["bailout","Estimate"]   
  ),
  se = c(
    coef(summary(ess.eco.sup))["bailout","Std. Error"],
    coef(summary(ess.eco.opp))["bailout","Std. Error"],
    coef(summary(liss.eco.sup))["bailout","Std. Error"],
    coef(summary(liss.eco.opp))["bailout","Std. Error"],
    coef(summary(ess.gov.sup))["bailout","Std. Error"],
    coef(summary(ess.gov.opp))["bailout","Std. Error"],
    coef(summary(liss.gov.sup))["bailout","Std. Error"],
    coef(summary(liss.gov.opp))["bailout","Std. Error"]
  )
)

pdf('fig5-party.pdf', height=5, width=8)
ggplot(political_df, aes(x=sup, y=effect, group=data, shape=data)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=effect-1.645*se, ymax=effect+1.645*se), position=position_dodge(width=0.3), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se), position=position_dodge(width=0.3), colour="black", width=0) + 
  geom_point(position=position_dodge(width=0.3), size = 3, shape=16, colour = "white") + 
  geom_point(position=position_dodge(width=0.3), size = 2) + 
  scale_shape_manual(values=c(1, 2)) +
  scale_x_discrete("") +
  ylab("Effect of bailout") +
  facet_wrap(~ variable) 
dev.off()

png("fig5-party.tiff", width=2400, height=1500, res = 300)
ggplot(political_df, aes(x=sup, y=effect, group=data, shape=data)) +
  geom_hline(yintercept=0, col="gray70") +
  geom_errorbar(aes(ymin=effect-1.645*se, ymax=effect+1.645*se), position=position_dodge(width=0.3), colour="black", size=1, width=0) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se), position=position_dodge(width=0.3), colour="black", width=0) + 
  geom_point(position=position_dodge(width=0.3), size = 3, shape=16, colour = "white") + 
  geom_point(position=position_dodge(width=0.3), size = 2) + 
  scale_shape_manual(values=c(1, 2)) +
  scale_x_discrete("") +
  ylab("Effect of bailout") +
  facet_wrap(~ variable) 
dev.off()

lm.party.eco.ess <- lm(stfeco ~ bailout*government, data=ess)
lm.party.gov.ess <- lm(stfgov ~ bailout*government, data=ess)
lm.party.eco.liss <- lm(eco ~ government, data=liss)
lm.party.gov.liss <- lm(gov ~ government, data=liss)

stargazer(lm.party.eco.ess, lm.party.gov.ess, lm.party.eco.liss, lm.party.gov.liss,
            covariate.labels = c("Bailout", "Government", "Bailout * Government"),
            column.labels=c("Economy", "Government", "Economy", "Government"),
            keep.stat = c("n","adj.rsq"),
            title="Heterogeneous response to bailout, partisanship, OLS",
            model.names=FALSE,
            type="text", out="tab3-party.htm")

balance <- MatchBalance(form = bailout ~ gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_north + region_east + region_west + region_south, data = ess)

cova <- c("Gender", "Age", "Education", "Income", "Partner", "Unemployed", "Pol. interest", "Ideology", "Region: North", "Region: East", "Region: West", "Region: South")
balance.table <- baltest.collect(matchbal.out = balance, var.names = cova, after = FALSE)

balance.df <- data.frame(cova, as.data.frame(balance.table)[c("T pval", "KS pval")])
balance.df2 <- gather(balance.df, variable, value, T.pval:KS.pval)
levels(balance.df2$variable) <- c("t-test", "Kolmogorov-\nSmirnov")
balance.df2$cova <- factor(balance.df2$cova, as.character(levels(balance.df2$cova)[c(length(balance.df$cova):1)]))

names(balance.df2) <- c("cova","Test","value")
balance.df2
balance.df2$Test <- ifelse(balance.df2$Test == "T.pval", "t-test", "Kolmogorov-Smirnov")

pdf('fig2-randomisation.pdf', height=4, width=6)
ggplot(balance.df2, aes(y=cova, x=value, shape=Test)) +
  geom_point(size=2.5) +
  ylab("") +
  xlab("p value") +
  geom_vline(xintercept=c(0.05), col="gray30", linetype="dotted", size=1) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal')
dev.off()

png("fig2-randomisation.tiff", width=1800, height=1200, res = 300)
ggplot(balance.df2, aes(y=cova, x=value, shape=Test)) +
  geom_point(size=2.5) +
  ylab("") +
  xlab("p value") +
  geom_vline(xintercept=c(0.05), col="gray30", linetype="dotted", size=1) +
  scale_shape_manual(values=c(1, 2)) +
  theme(legend.title = element_blank(), legend.key = element_blank(),
        legend.position = 'top', legend.direction = 'horizontal')
dev.off()



# Results

ess.reg.eco <- lm(stfeco~bailout,data=ess)
ess.reg.eco.cov <- lm(stfeco~bailout + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south,data=ess)
ess.reg.gov <- lm(stfgov~bailout,data=ess)
ess.reg.gov.cov <- lm(stfgov~bailout + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south,data=ess)
liss.reg.eco <- lm(stfeco~bailout, data=liss.long.eco)
liss.reg.gov <- lm(stfgov~bailout, data=liss.long.gov)

stargazer(ess.reg.eco, ess.reg.eco.cov, ess.reg.gov, ess.reg.gov.cov, liss.reg.eco, liss.reg.gov, 
          omit.stat=c("LL","ser","f", "adj.rsq"),
          dep.var.labels=c("Economy","Government","Economy","Government"),
          covariate.labels=c("Bailout","Female","Age","Education","Income","Partner","Unemployed","Pol. interest", "Ideology", "Region: East", "Region: West", "Region: South"),
          type="text",
          out="tabD1-maineffects.htm"
)


# Party analysis 

ess.reg.eco.o <- lm(stfeco ~ bailout, data=ess[ess$government == 0,])
ess.reg.eco.o.cov <- lm(stfeco ~ bailout + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south, data=ess[ess$government == 0,])
ess.reg.eco.g <- lm(stfeco ~ bailout, data=ess[ess$government == 1,])
ess.reg.eco.g.cov <- lm(stfeco ~ bailout + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south, data=ess[ess$government == 1,])
ess.reg.gov.o <- lm(stfgov ~ bailout, data=ess[ess$government == 0,])
ess.reg.gov.o.cov <- lm(stfgov ~ bailout + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south, data=ess[ess$government == 0,])
ess.reg.gov.g <- lm(stfgov ~ bailout, data=ess[ess$government == 1,])
ess.reg.gov.g.cov <- lm(stfgov ~ bailout + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south, data=ess[ess$government == 1,])

stargazer(ess.reg.eco.o,ess.reg.eco.o.cov,ess.reg.eco.g,ess.reg.eco.g.cov,
          ess.reg.gov.o,ess.reg.gov.o.cov,ess.reg.gov.g,ess.reg.gov.g.cov, 
          omit.stat=c("LL","ser","f", "adj.rsq"),
          covariate.labels=c("Bailout","Female","Age","Education","Income","Partner","Unemployed","Pol. interest", "Ideology", "Region: East", "Region: West", "Region: South"),
          type="text",
          out="tabD2-partyeffects.htm"
)

# Descriptive statistics

stargazer(ess[c("stfgov", "stfeco", "bailout", "government", "gndr", "agea", "eduyrs", "hinctnta", "partner", "uemp3m", "polintr", "lrscale", "region_north", "region_east", "region_west", "region_south")],
          covariate.labels = c("Government satisfaction", "Economic satisfaction", "Bailout", "Government supporter", "Female", "Age", "Education", "Income", "Partner", "Unemployed", "Political interest", "Ideology", "Region: North", "Region: East", "Region: West", "Region: South"),
          title = "Summary statistics, ESS",
          median = TRUE, iqr = TRUE,
          digits = 2,
          summary = TRUE,  type="text",
          out="tabC1-descriptive.htm")

# Correlation matrix

ess %>%
  select(stfgov, stfeco, bailout, government, gndr, agea, eduyrs, hinctnta, partner, uemp3m, polintr, lrscale) %>%
  rename(Government = stfgov,
         Economy = stfeco, 
         Bailout = bailout,
         Supporter = government,
         Female = gndr, 
         Age = agea, 
         Education = eduyrs, 
         Income = hinctnta, 
         Partner = partner, 
         Unemployed = uemp3m, 
         Interest = polintr,
         Ideology = lrscale 
         ) %>%
  cor(., use="pairwise.complete.obs") %>%
  stargazer(., type="text", digits = 2, out="tabC2-cormatrix.htm")

# Eurobarometer

mean(eb$eco[eb$tr == 0], na.rm=TRUE)
sd(eb$eco[eb$tr == 0], na.rm=TRUE)

mean(eb$eco[eb$tr == 1], na.rm=TRUE)
sd(eb$eco[eb$tr == 1], na.rm=TRUE)

mean(eb$gov[eb$tr == 0], na.rm=TRUE)
sd(eb$gov[eb$tr == 0], na.rm=TRUE)

mean(eb$gov[eb$tr == 1], na.rm=TRUE)
sd(eb$gov[eb$tr == 1], na.rm=TRUE)

NROW(eb[!is.na(eb$eco) & !is.na(eb$gov), ])

chisq.test(eb$eco, eb$tr)
chisq.test(eb$gov, eb$tr)

eco_agg <- eb %>%
  group_by(tr) %>%
  count(eco) %>%
  drop_na(eco) %>%
  mutate(n_per = ifelse(tr == 0, n/1025, n/1026))

fig_eb_eco <- ggplot(eco_agg, aes(x=eco, y=n_per)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = c(0,1,2), labels = c("Worse", "Same", "Better")) +
  facet_wrap(~ ifelse(tr == 0, "April, 2008", "October, 2008"))  +
  labs(
    y = "",
    x = "Expectation for economic situation"
  ) +
  scale_y_continuous(labels=percent) 

eb_agg <- eb %>%
  group_by(tr) %>%
  summarise(gov = mean(gov, na.rm=TRUE)) %>%
  mutate(gov_no = 1 - gov) %>%
  gather(trust, value, gov, gov_no, factor_key=TRUE)

fig_eb_gov <- ggplot(eb_agg, aes(x=ifelse(trust == "gov", "Trust", "No trust"), y=value)) +
  geom_bar(stat="identity") +
  facet_wrap(~ ifelse(tr == 0, "April, 2008", "October, 2008")) +
  labs(y = "", x = "Trust in national government") +
  scale_y_continuous(labels=percent) 

pdf('fig7-eurobarometer.pdf', height=4, width=8)
grid.arrange(fig_eb_eco, fig_eb_gov, ncol=2)
dev.off()

png("fig7-eurobarometer.tiff", width=2400, height=1200, res = 300)
grid.arrange(fig_eb_eco, fig_eb_gov, ncol=2)
dev.off()

full_eco <- lm(stfeco ~ bailout + government + stfdem + stfgov + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south, data=ess)
full_gov <- lm(stfgov ~ bailout + government + stfdem + stfeco + gndr + agea + eduyrs + hinctnta + partner + uemp3m + polintr + lrscale + region_east + region_west + region_south, data=ess)

stargazer(full_eco, full_gov, type="text", omit.stat=c("LL","ser","f", "adj.rsq"),
          covariate.labels=c("Bailout", "Government supporter", "Satisfaction: Democracy", "Satisfaction: Government", "Satisfaction: Economy", "Female","Age","Education","Income","Partner","Unemployed","Pol. interest", "Ideology", "Region: East", "Region: West", "Region: South"), 
          single.row = TRUE,
          out = "tabF1-fullmodels.htm")

# Create and save sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")