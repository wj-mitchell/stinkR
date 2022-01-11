# boncons <- function(depvar,
#                     cons,
#                     names = NA){
#   # Setup ----
#   ## Package Loading ----
#   
#   ## Options ----
#   options(scipen=100)
#   options(digits=3)
#   options(contrasts = c("contr.helmert", "contr.poly"))
#   
#   ## Errors ----
# 
#   ## No errors detected ----

c2 <- t.test(df$Correlation.fz[df$ROI == "NAcc" & df$Age.Group == "Child"], df$Correlation.fz[df$ROI == "NAcc" & df$Age.Group == "Adult"])
c3 <- t.test(df$Correlation.fz[df$ROI == "vmPFC" & df$Age.Group == "Child"], df$Correlation.fz[df$ROI == "vmPFC" & df$Age.Group == "Adult"])
c4 <- t.test(df$Correlation.fz[df$ROI == "AMY" & df$Age.Group == "Child"], df$Correlation.fz[df$ROI == "NAcc" & df$Age.Group == "Child"])
c22 <- t.test(df$Correlation.fz[(df$Valence.Pair == "Positive" | df$Valence.Pair == "Negative") & df$Age.Group == "Child"], df$Correlation.fz[(df$Valence.Pair == "Positive" | df$Valence.Pair == "Negative") & df$Age.Group == "Adult"])
c23 <- t.test(df$Correlation.fz[(df$Valence.Pair == "Positive" | df$Valence.Pair == "Negative") & df$Age.Group == "Child"], df$Correlation.fz[df$Valence.Pair == "Neutral" & df$Age.Group == "Child"])
c24 <- t.test(df$Correlation.fz[(df$Valence.Pair == "Positive" | df$Valence.Pair == "Negative") & df$Age.Group == "Adult"], df$Correlation.fz[df$Valence.Pair == "Neutral" & df$Age.Group == "Adult"])
c25 <- t.test(df$Correlation.fz[(df$Valence.Pair == "Negative" & df$Age.Group == "Child") | (df$Valence.Pair == "Positive" & df$Age.Group == "Adult")], 
              df$Correlation.fz[(df$Valence.Pair == "Negative" & df$Age.Group == "Adult") | (df$Valence.Pair == "Positive" & df$Age.Group == "Child")])
c26 <- t.test(df$Correlation.fz[((df$Valence.Pair == "Positive" | df$Valence.Pair == "Negative") & df$Age.Group == "Child") | (df$Valence.Pair == "Neutral" & df$Age.Group == "Adult")], 
              df$Correlation.fz[((df$Valence.Pair == "Positive" | df$Valence.Pair == "Negative") & df$Age.Group == "Adult") | (df$Valence.Pair == "Neutral" & df$Age.Group == "Child")])
c("Child vs. Adult, AMY", "Child vs. Adult, NAcc", "Child vs. Adult, vmPFC",
  "AMY vs. NAcc, Child", "AMY vs. vmPFC, Child", "NAcc vs. vmPFC, Child",
  "AMY vs. NAcc, Adult", "AMY vs. vmPFC, Adult", "NAcc vs. vmPFC, Adult",
  "AMY vs. NAcc, Child vs. Adult", "AMY vs. vmPFC, Child vs. Adult", 
  "NAcc vs. vmPFC, Child vs. Adult", "Positive vs. Negative, Adult", 
  "Positive vs. Negative, Child", "Positive vs. Neutral, Adult", 
  "Positive vs. Neutral, Child", "Negative vs. Neutral, Adult", 
  "Negative vs. Neutral, Child", "Child vs. Adult, Positive", 
  "Child vs. Adult, Negative", "Child vs. Adult, Neutral",
  "Val, Child vs. Val, Adult", "Val vs. Non-Val, Child", 
  "Val vs. Non-Val, Adult", "Child vs. Adult, Negative vs. Positive",
  "Val vs. Non-Val, Child vs. Adult")
Contrast <- c()
Estimate <- round(c(c1$estimate, c2$estimate, c3$estimate, c4$estimate, c5$estimate,
                    c6$estimate, c7$estimate, c8$estimate, c9$estimate, c10$estimate, 
                    c11$estimate, c12$estimate, c13$estimate, c14$estimate, c15$estimate,
                    c16$estimate,c17$estimate,c18$estimate,c19$estimate,c20$estimate,c21$estimate,
                    c22$estimate,c23$estimate,c24$estimate, c25$estimate, c26$estimate),3)
CI95 <- round(c(c1$conf.int, c2$conf.int, c3$conf.int, c4$conf.int, c5$conf.int,
                c6$conf.int, c7$conf.int, c8$conf.int, c9$conf.int, c10$conf.int, 
                c11$conf.int, c12$conf.int, c13$conf.int, c14$conf.int, c15$conf.int, 
                c16$conf.int, c17$conf.int, c18$conf.int, c19$conf.int, c20$conf.int, 
                c21$conf.int, c22$conf.int, c23$conf.int, c24$conf.int, c25$conf.int, c26$conf.int),3)
Statistic <- round(c(c1$statistic, c2$statistic, c3$statistic, c4$statistic, c5$statistic,
                     c6$statistic, c7$statistic, c8$statistic, c9$statistic, c10$statistic, 
                     c11$statistic, c12$statistic, c13$statistic, c14$statistic, c15$statistic,
                     c16$statistic, c17$statistic, c18$statistic, c19$statistic, c20$statistic,
                     c21$statistic, c22$statistic, c23$statistic, c24$statistic, c25$statistic, c26$statistic),3)
StdErr <- round(c(c1$stderr, c2$stderr, c3$stderr, c4$stderr, c5$stderr,
                  c6$stderr, c7$stderr, c8$stderr, c9$stderr, c10$stderr, 
                  c11$stderr, c12$stderr, c13$stderr, c14$stderr, c15$stder,
                  c16$stder, c17$stder, c18$stder, c19$stder, c20$stder,
                  c21$stder, c22$stder, c23$stder, c24$stder, c25$stder, c26$stder),3)
DoF <- round(c(c1$parameter, c2$parameter, c3$parameter, c4$parameter, c5$parameter,
               c6$parameter, c7$parameter, c8$parameter, c9$parameter, c10$parameter, 
               c11$parameter, c12$parameter, c13$parameter, c14$parameter, c15$parameter,
               c16$parameter, c17$parameter, c18$parameter, c19$parameter, c20$parameter,
               c21$parameter, c22$parameter, c23$parameter, c24$parameter, c25$parameter, c26$parameter),3)
Raw.P <- round(c(c1$p.value, c2$p.value, c3$p.value, c4$p.value, c5$p.value,
                 c6$p.value, c7$p.value, c8$p.value, c9$p.value, c10$p.value, 
                 c11$p.value, c12$p.value, c13$p.value, c14$p.value, c15$p.value,
                 c16$p.value, c17$p.value, c18$p.value, c19$p.value, c20$p.value,
                 c21$p.value, c22$p.value, c23$p.value, c24$p.value, c25$p.value, c26$p.value),3)

Cols <- c("Contrast", "Est.X", "Est.Y", "Diff", "SE", "CI.LWR", "CI.UPR", "T.Stat", "df", "Raw.P.Val", "Bonferroni.P.Val")

Rows <- 1:length(Contrast)
 
Contrasts <- data.frame(matrix(NA, 
                               nrow = length(Rows), 
                               ncol = length(Cols), 
                               dimnames = list(Rows, Cols)))
   
Contrasts$Contrast <- Contrast 
Contrasts$Est.X <- Estimate[seq(1,((length(Rows)*2) - 1),2)]
Contrasts$Est.Y <- Estimate[seq(2,(length(Rows)*2),2)]
Contrasts$Diff <- Contrasts$Est.X - Contrasts$Est.Y
Contrasts$SE <- StdErr
Contrasts$CI.LWR <- CI95[seq(1,((length(Rows)*2) - 1),2)]
Contrasts$CI.UPR <- CI95[seq(2,(length(Rows)*2),2)]
Contrasts$T.Stat <- Statistic
Contrasts$df <- DoF
Contrasts$Raw.P.Val <- Raw.P
Contrasts$Bonferroni.P.Val <- round(p.adjust(Contrasts$Raw.P.Val, method = "bonferroni"),3)
rm(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25, Cols, Rows, Contrast, Estimate, CI95, Statistic,DoF, Raw.P, StdErr)
  
write.csv(Contrasts, "C:/Users/Administrator/Desktop/Grad School/Projects, Current/RSA/Github/Data/3. Data, Reformatted/Primary.Contrasts.csv")
    
Contrasts
