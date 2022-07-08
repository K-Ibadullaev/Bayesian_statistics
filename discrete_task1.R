library(EBImage)

# Visualize the task
tsk = readImage("task1.png")
display(tsk)

# set the data

# distribution of rocks due to levels
df = data.frame(Level =factor(c("F1", "F2", "F3")) , 
                Brown=c(5,1,1), Green=c(2, 1, 6), White=c(1,2,9),
                prob_F = c(0.25, 0.5,0.25))
total = c(sum(df[1,c(2:4)]),sum(df[2,c(2:4)]),sum(df[3,c(2:4)]))
df = cbind(df,total)
df

# probabilities 
prbs_lklhd = data.frame(Level = factor(c("F1", "F2", "F3")),
                  Brown = df$Brown %o% 1/df$total,
                  Green = df$Green %o% 1/df$total,
                  White = df$White %o% 1/df$total,
                  total = df$total)
prbs_lklhd

# to obtain brown rock from F
Pr_brown = df[1,5] * prbs_lklhd[1,2] + df[2,5] * prbs_lklhd[2,2] + df[3,5] * prbs_lklhd[3,2]  
Pr_brown

# to obtain green rock from F
Pr_green = df[1,5] * prbs_lklhd[1,3] + df[2,5] * prbs_lklhd[2,3] + df[3,5] * prbs_lklhd[3,3]  
Pr_green

# to obtain white rock from F
Pr_white = df[1,5] * prbs_lklhd[1,4] + df[2,5] * prbs_lklhd[2,4] + df[3,5] * prbs_lklhd[3,4]  
Pr_white

#Join into the table
Pr_colour = data.frame(
  Level = factor("F") , 
  Brown=Pr_brown, 
  Green=Pr_green,
  White=Pr_white,
  prob_F = "---",
  total = sum(Pr_brown, Pr_green, Pr_white))
# as one can see result is proved as sum of probabilities for each rock equals 1  
df = rbind(df, Pr_colour)
df


F = rbind(prbs_lklhd, Pr_colour[,-5]) %>%
  mutate(total_Pr= c(sum(prbs_lklhd[1,c(2:4)]),
                  sum(prbs_lklhd[2,c(2:4)]),
                  sum(prbs_lklhd[3,c(2:4)]),
                  sum(Pr_brown, Pr_green, Pr_white)))

F = F[, -5]
F

results = F[4,]
results






# # One can calculate probability of picking brown rock from any of single formation 
# Brown_F123 = matrix(3,1)
# for (i in 1:3) {
#   Brown_F123[i] = round(as.numeric(prbs_lklhd[i,2])* as.numeric(df[i,5])/as.numeric(F[4,2]),3)
#   
# }
# 
# 
# 
# cnames(Brown_F123)=c("F1", "F2", "F3")
# Green_F123 = matrix(3,1)
# for (i in 1:3) {
#   Green_F123[i] = round(as.numeric(prbs_lklhd[i,3])* as.numeric(df[i,5])/as.numeric(F[4,3]),3)
#   
# }
# Green_F123
# 
# White_F123 = matrix(3,1)
# for (i in 1:3) {
#   White_F123[i] = round(as.numeric(prbs_lklhd[i,4])* as.numeric(df[i,5])/as.numeric(F[4,4]),3)
#   
# }
# White_F123
# 
# separate_fs = data.frame(0,Formation=factor(c("F1", "F2", "F3")))
#   
#   
# Brown_F123 %>% data.frame() 
# Green_F123 %>% data.frame() 
# White_F123 %>% data.frame() 
# separate_fs=cbind(separate_fs,Brown_F123,Green_F123,White_F123)
# separate_fs = separate_fs[,-1]
# separate_fs
