# Poster_Publication_Figures.R
# Generates all five publication-quality figures for the poster

library(ggplot2); library(dplyr); library(cowplot); library(reshape2); library(lme4)
# Set color palette
cols <- c("explore"="#D8A7FF", "exploit"="#DB4DB1", "none"="#FF66B3", "plum"="#642B73", "Male"="#0073C2FF", "Female"="#FF66B3", "Original"="#FF9900", "Interaction"="#00B26D", "Random"="#CCCCCC", "Baseline"="#999999", "Relative rank"="#642B73", "Absolute rank"="#D8A7FF")
# 1. Context_Bars.png
{
  d <- read.csv("Explore Exploit Dataset.csv"); d$Outcome <- factor(tolower(d$OUTCOME), levels = c("explore", "exploit", "none")); d$CONDITION <- factor(d$CONDITION, levels = c("Solo", "Duo", "Trio"))
  tab <- d %>% count(CONDITION, Outcome) %>% group_by(CONDITION) %>% mutate(pct = n/sum(n), label = scales::percent(pct, accuracy=1))
  p <- ggplot(tab, aes(CONDITION, pct, fill=Outcome)) +
    geom_bar(stat="identity", width=0.7) +
    geom_text(aes(label=label), position=position_stack(vjust=0.5), size=5, family="Arial") +
    scale_fill_manual(values=cols) +
    scale_y_continuous(labels=scales::percent) +
    labs(title="Social Context Inhibits Exploration (n = 1 443)", x=NULL, y=NULL) +
    theme_bw() + theme(text=element_text(family="Arial"), panel.grid.minor=element_blank())
  ggsave("Context_Bars.png", p, width=16, height=12, units="cm", dpi=300)
}
# 2. Rank_Slope.png
{
  d <- read.csv("Explore Exploit Dataset.csv"); d$sex <- ifelse(d$monkey %in% c("FRAN","DALI","EBI"), "Male", "Female"); d$RELATIVE_RANK <- as.factor(d$RELATIVE_RANK)
  tab <- d %>% filter(tolower(OUTCOME)=="explore") %>% count(RELATIVE_RANK, sex) %>% left_join(d %>% count(RELATIVE_RANK, sex, name="total"), by=c("RELATIVE_RANK","sex")) %>% mutate(pct=100*n/total)
  fit <- lm(pct ~ as.numeric(RELATIVE_RANK), data=tab)
  ci <- predict(fit, newdata=data.frame(RELATIVE_RANK=1:3), interval="confidence")
  p <- ggplot(tab, aes(as.numeric(RELATIVE_RANK), pct, color=sex)) +
    geom_jitter(width=0.1, size=3, alpha=0.7) +
    geom_line(data=data.frame(x=1:3, y=ci[,1]), aes(x, y), color=cols["plum"], size=1) +
    geom_ribbon(data=data.frame(x=1:3, ymin=ci[,2], ymax=ci[,3]), aes(x, ymin=ymin, ymax=ymax), fill=cols["explore"], alpha=0.3, inherit.aes=FALSE) +
    scale_color_manual(values=c("Male"=cols["Male"], "Female"=cols["Female"])) +
    labs(x="Relative Rank", y="% Explore", title="Exploration by Relative Rank") +
    annotate("text", x=2.7, y=max(tab$pct)+2, hjust=1, label=sprintf("β = %.2f, p = %.3f", coef(fit)[2], summary(fit)$coefficients[2,4]), size=5, family="Arial") +
    theme_bw() + theme(text=element_text(family="Arial"), panel.grid.minor=element_blank())
  ggsave("Rank_Slope.png", p, width=16, height=12, units="cm", dpi=300)
}
# 3. Personality_Caterpillar.png
{
  d <- read.csv("Explore Exploit Dataset.csv"); d$Outcome <- factor(tolower(d$OUTCOME), levels=c("explore","exploit","none")); d$monkey <- factor(d$monkey)
  m <- glmer((Outcome=="explore") ~ 1 + (1|monkey), data=d, family=binomial)
  re <- ranef(m)$monkey[,1]; se <- sqrt(attr(ranef(m, condVar=TRUE)$monkey, "postVar")[1,1,])
  df <- data.frame(monkey=names(re), logodds=re, lower=re-1.96*se, upper=re+1.96*se)
  df <- df[order(-df$logodds),]; df$monkey <- factor(df$monkey, levels=df$monkey)
  p <- ggplot(df, aes(x=logodds, y=monkey)) +
    geom_point(size=3, color=cols["plum"]) +
    geom_errorbarh(aes(xmin=lower, xmax=upper), height=0.2, color=cols["explore"]) +
    labs(title="Monkey Personality: Exploration Log-Odds", x="Random Intercept (log-odds)", y=NULL) +
    theme_bw() + theme(text=element_text(family="Arial"), panel.grid.minor=element_blank())
  ggsave("Personality_Caterpillar.png", p, width=16, height=12, units="cm", dpi=300)
}
# 4. Accuracy_Bars.png
{
  df <- data.frame(Method=c("Random","Baseline","Original","Interaction"), Accuracy=c(33,34.2,46.9,48.9))
  p <- ggplot(df, aes(x=reorder(Method, Accuracy), y=Accuracy, fill=Method)) +
    geom_bar(stat="identity", width=0.7) +
    geom_hline(yintercept=46.9, linetype="dashed", color="red", size=1) +
    scale_fill_manual(values=c("Original"=cols["Original"], "Interaction"=cols["Interaction"], "Random"=cols["Random"], "Baseline"=cols["Baseline"])) +
    coord_flip() +
    labs(title="Model Accuracy Comparison", x=NULL, y="Accuracy (%)") +
    theme_bw() + theme(text=element_text(family="Arial"), panel.grid.minor=element_blank(), legend.position="none")
  ggsave("Accuracy_Bars.png", p, width=16, height=12, units="cm", dpi=300)
}
# 5. ELPD_Diff.png
{
  df <- data.frame(Model=c("Relative rank","Absolute rank"), ELPD=c(6.7,0))
  p <- ggplot(df, aes(x=Model, y=ELPD, fill=Model)) +
    geom_bar(stat="identity", width=0.7) +
    geom_text(aes(label="+6.7 log-lik units"), x=1, y=7, vjust=0, size=6, family="Arial") +
    scale_fill_manual(values=c("Relative rank"=cols["Relative rank"], "Absolute rank"=cols["Absolute rank"])) +
    labs(title="ΔELPD: Model Comparison", x=NULL, y=NULL) +
    theme_bw() + theme(text=element_text(family="Arial"), panel.grid=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), legend.position="none")
  ggsave("ELPD_Diff.png", p, width=16, height=12, units="cm", dpi=300)
} 