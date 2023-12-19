#load libraries
libs <- c("mclust", "psych", "corrplot", "ggplot2", "patchwork", "rstatix", "effsize", "ggpubr", "MASS", "rcompanion", "DescTools", "vcd", "epitools", "stringr")
for (p in libs) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p, dep = TRUE)
  }
}
lapply(libs, FUN = function(libs){
  do.call("library", list(libs))
})
trace(corrplot, edit=TRUE)
#In the popup window replace the function in line 443 with the following:
#place_points = function(sig.locs, point) {text(pos.pNew[, 1][sig.locs], (pos.pNew[, 2][sig.locs])+0.2, labels = point, col = pch.col, cex = pch.cex, lwd = 2)

question_labels <- c("mental_demand", "temporal_demand", "performance", "effort", "frustration", "distraction", "fokus")
task_labels <- c("Small Objects", "Large Objects", "Lights")
method_labels <- c("Optix\nDenoiser", "Noisy\nIncremental", "Denoised\nIncremental")

color_tasks <- c("#009E73", "#56B4E9", "#E69F00")
color_methods <- c("#e41a1c", "#999999", "#377eb8")

out_path <- "plots/"
file_ending <- ".pdf"
dir.create(file.path(out_path), showWarnings = FALSE)

data_all <- read.csv("data.csv")

################################################################################
#####################          Functions            ############################
################################################################################

calculate_nps <- function(data, thr_hi, thr_low)
{
  pro_A <- nrow(data[data$buy_nps_a > thr_hi,])
  pro_B <- nrow(data[data$buy_nps_b > thr_hi,])
  pro_C <- nrow(data[data$buy_nps_c > thr_hi,])
  contra_A <- nrow(data[data$buy_nps_a < thr_low,])
  contra_B <- nrow(data[data$buy_nps_b < thr_low,])
  contra_C <- nrow(data[data$buy_nps_c < thr_low,])
  nps_A <- pro_A/nrow(data) - contra_A/nrow(data)
  nps_B <- pro_B/nrow(data) - contra_B/nrow(data)
  nps_C <- pro_C/nrow(data) - contra_C/nrow(data)
  return(c(nps_A, nps_B, nps_C))
}

plot_tlx_all <- function(data, index)
{
  if(is.null(data)) return(NULL)
  plot <- ggplot(data, aes(x=question, y=data[,index], color=question)) + 
    geom_boxplot(outlier.shape=8) + 
    geom_jitter() +
    scale_y_continuous(limits=c(-2,6), breaks = c(0,2,4)) +
    theme_classic() + 
    theme(legend.position = "bottom") +
    ylab("score")

  return(plot)
}

plot_grid <- function(data, fun, title, pos="bottom")
{
  if(is.null(data)) return(NULL)
  data_plotting <- data
  
  plots <- lapply(1:9, fun, data = data_plotting)
  grid <- (((plots[[1]] + theme(plot.margin = unit(c(30,30,0,0), "pt")))+
           (plots[[2]] + theme(plot.margin = unit(c(0,0,0,0), "pt")))+
           (plots[[3]] + theme(plot.margin = unit(c(0,0,0,30), "pt")))+plot_layout(tag_level = 'new'))/
          ((plots[[4]] + theme(plot.margin = unit(c(30,30,0,0), "pt")))+
           (plots[[5]] + theme(plot.margin = unit(c(0,0,0,0), "pt")))+
           (plots[[6]] + theme(plot.margin = unit(c(0,0,0,30), "pt")))+plot_layout(tag_level = 'new'))/
          ((plots[[7]] + theme(plot.margin = unit(c(30,30,0,0), "pt")))+
           (plots[[8]] + theme(plot.margin = unit(c(0,0,0,0), "pt")))+
           (plots[[9]] + theme(plot.margin = unit(c(0,0,0,30), "pt")))+plot_layout(tag_level = 'new'))/
           guide_area()) +
    plot_layout(guides = "collect")  +
    plot_annotation(title = title, tag_levels = list(task_labels, gsub("[\r\n]", " ", method_labels)), tag_sep = ', ')& 
    theme(plot.tag.position = c(1, 1), plot.tag = element_text(size=12, hjust = 1, vjust = 0),
          plot.title = element_text(size=14), legend.position = pos)
  return(grid)
}

plot_tlx_sum <- function(data, task)
{
  plot_data_sum <- data.frame(values = as.vector(as.matrix(data[,c(paste0("accumulated_",task,"a"), paste0("accumulated_",task,"b"), paste0("accumulated_",task,"c"))])))
  plot_data_sum["method"] <- factor(rep(method_labels, each=nrow(data)), levels=method_labels)
  ggplot(plot_data_sum, aes(x=method, y=values, fill=method)) + 
    geom_boxplot(outlier.shape=8) + 
    geom_jitter() + 
    theme_classic() + 
    theme(legend.position = "bottom") +
    labs(title=task_labels[task]) +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=color_methods)
}

plot_diff_titles <- c("Favorite Incremental - Optix Denoiser", "Noisy Incremental - Optix Denoiser", "Denoised Incremental - Optix Denoiser", "Noisy Incremetal - Denoised Incremental")
annotations <- data.frame(
  xpos = c(Inf,Inf),
  ypos =  c(-Inf,Inf),
  hjustvar = c(1,1) ,
  vjustvar = c(-1,2))
plot_tlx_diff <- function(data, index)
{
  plot_data <- data[,c(index,5)]
  names(plot_data) <- c("values", "task")
  mLabel <- str_split(plot_diff_titles[index], " - ")
  m1 <- mLabel[[1]][1]
  m2 <- mLabel[[1]][2]
  if(index==1){
    m1 <- "Incremental"
  }
  ggplot(plot_data, aes(x=task, y=values)) + 
    geom_boxplot(outlier.shape=8,aes(fill=task)) + 
    #geom_jitter() +
    stat_summary(fun=mean, geom="point", shape=18, size=3, position=position_dodge(0.75)) +
    theme_classic() + 
    theme(legend.position = "top", plot.title = element_text(size=12), plot.title.position = "plot") +
    labs(title=plot_diff_titles[index]) +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=color_tasks, name = "Editing Scenario") +
    ylab("Difference in the aggregated Score") +
    xlab("Editing Scenario") +
    scale_y_continuous(limits=c(-4.1,5.1),breaks=c(-4,-2,0,2,4))+
    geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=c(paste("Favor", m2), paste("Favor", m1))), colour='#808080', size=3)
}

diff_short_labes <- c("F-D","I-D","DI-D","I-DI")
plot_qq <- function(data, index)
{
  p_long <- shapiro.test(data[,index+1])$p
  p <- round(p_long, 3)
  if (p==0)
  {
    p <- formatC(p_long, format = "e", digits = 1, drop0trailing=TRUE, replace.zero=TRUE, zero.print="")
  }
  ggqqplot(data[c(index+1,1)], x = paste0("values_",((index-1)%%3)+1,"_",diff_short_labes[((index-1)/3)+1]), xlab=FALSE, ylab=FALSE, size=0.7) +
    theme_classic() +
    labs(title=paste0("p=", p)) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(size=12) )
}

plot_row <- function(num_plots=1, data, fun, title, pos="bottom", margins=list(c(30,30,0,0), c(0,0,0,0), c(0,0,0,30)))
{
  if(is.null(data)) return(NULL)
  data_plotting <- data
  
  plots <- lapply(1:num_plots, fun, data = data_plotting)
  
  combined_plots <- plots[[1]] + theme(plot.margin = unit(margins[[1]], "pt"))
  if (num_plots > 1)
  {
    for (i in 2:num_plots)
    {
      combined_plots <- combined_plots | (plots[[i]] + theme(plot.margin = unit(margins[[i]], "pt")))
    }
  }
  heights <- c(3, 1)
  if (pos=="bottom")
  {
    combined_plots <- combined_plots / guide_area()
  }
  else if (pos=="top")
  {
    combined_plots <- guide_area() / combined_plots
    heights <- c(0.5, 4)
  }
  
  grid <- combined_plots +
    plot_annotation(title = title) +
    plot_layout(guides = "collect", heights = heights)
  return(grid)
}

structure_diff_data_for_plotting <- function(data)
{
  plot_data <- data.frame(values = as.vector(as.matrix(data[,c("differences_1", "differences_2", "differences_3")])))
  plot_data["task"] <- factor(rep(task_labels, each=nrow(data_tlx)), levels=task_labels)
  return(plot_data)
}

str_to_freq <- function(data, split=",", name="name")
{
  temp_data <- data
  temp_data[is.na(temp_data)] <- "NA"
  temp_data <- lapply(temp_data, as.character)
  temp_list <- lapply(temp_data, strsplit, split=split, fixed=TRUE)
  temp_vector <- unlist(lapply(temp_list,unlist))
  data_frame <- data.frame(table(temp_vector))
  colnames(data_frame) <- c(name, "Frequency")
  data_frame[,name] <- factor(data_frame[,name], levels = data_frame[,name][order(data_frame$Frequency, decreasing = TRUE)])
  return(data_frame)
}

################################################################################
#####################          General Data         ############################
################################################################################

data_relevant <- data_all[,20:176]
# colnames(data_relevant)
data_general <- data_relevant[,c(1,144:157)]

factor_rows <- c("reversed", "preference", "experience", "status", "frequency", "gender")
data_general[,factor_rows] <- lapply(data_general[,factor_rows], as.factor)
levels(data_general[,"reversed"]) <- c("no", "yes")
data_general[,"experience"] <- factor(data_general[,"experience"], levels=c("50-99","100-499","500-999","1,000-4,999","5,000-9,999","10,000+"), ordered=TRUE)
data_general[,"frequency"] <- factor(data_general[,"frequency"], levels=c("Never", "Occasionally", "Regularly", "Every time"), ordered=TRUE)
summary(data_general)
sd(data_general$age)
quantile(data_general$experience, probs = c(0,0.25,0.5,0.75,1), type=1)

data_general[,"used_apps"]<-gsub("Other modelling/rendering software", "Other RE", data_general[,"used_apps"])
data_general[,"used_apps"]<-gsub("Other game engines", "Other GE", data_general[,"used_apps"])
table_used_apps<-str_to_freq(data_general[,"used_apps"])
table_used_apps
ggplot(data=data.frame(table_used_apps), aes(x=name, y=Frequency)) + geom_bar(stat='identity')

data_general[,"field_of_work"]<-gsub("Video Game Development", "Games", data_general[,"field_of_work"])
data_general[,"field_of_work"]<-gsub("Digital Art \\(single frame rendering\\)", "Digital Art", data_general[,"field_of_work"])
data_general[,"field_of_work"]<-gsub("VR/AR|Hobby modeling|rendering research", "Other", data_general[,"field_of_work"])
table_field_of_work<-str_to_freq(data_general[,"field_of_work"])
table_field_of_work
ggplot(data=data.frame(table_field_of_work), aes(x=name, y=Frequency)) + geom_bar(stat='identity')

table_reasons<-str_to_freq(data_general[,"reasons"])
table_reasons
ggplot(data=data.frame(table_reasons), aes(x=name, y=Frequency)) + geom_bar(stat='identity')

data_general[,"denoiser"]
data_general[,"denoiser"]<-gsub("Open Image Denoise \\(Intel\\)", "Intel", data_general[,"denoiser"])
data_general[,"denoiser"]<-gsub("Vdenoise \\(V Ray\\)", "Vdenoise", data_general[,"denoiser"])
data_general[,"denoiser"]<-gsub("Nvidia Real-Time Denoisers \\(NRD\\)", "NRD", data_general[,"denoiser"])
data_general[,"denoiser"]<-gsub("Octane Renderer", "Octane", data_general[,"denoiser"])
data_general[,"denoiser"]<-gsub("Arnold Denoiser", "Arnold", data_general[,"denoiser"])
data_general[,"denoiser"]<-gsub("I'm not rendering with path tracing/other noise producing methods", "NA", data_general[,"denoiser"])
table_denoiser<-str_to_freq(data_general[,"denoiser"])
table_denoiser
ggplot(data=data.frame(table_denoiser), aes(x=name, y=Frequency)) + geom_bar(stat='identity')

data_meta <- data.frame(reversed=factor(data_relevant$reversed))
data_meta["participation"] <- factor(c(1,1,3,2,1,2,1,2,2,2,1,3,3,3,3,1,2,2,1,2,1))
levels(data_meta$participation) <- c("Parsec", "Own HW", "In Person")
summary(data_meta)
data_meta["fav_overall"] <- tlx_favorites$fav_overall
data_meta["fav_stated"] <- tlx_favorites$fav_stated
table(data_meta$reversed, data_meta$participation)


################################################################################
#####################    Extract Nasa-tlx data      ############################
################################################################################

# Extract data
data_tlx <- data.frame(row.names = 1:nrow(data_relevant))

for (question in question_labels)
{
  for (task in 1:3)
  {
    values <- c()
    for (i in 1:nrow(data_relevant))
    {
      if(data_relevant[i,"reversed"])
      {
        values <- c(values, data_relevant[i,paste0(question, "_",task,"d")])
      }
      else
      {
        values <- c(values, data_relevant[i,paste0(question, "_",task,"a")])
      }
    }
    data_tlx[paste0(question,"_",task,"a")] <- values
    
    data_tlx[paste0(question,"_",task,"b")] <- data_relevant[,paste0(question, "_",task,"b")]
    data_tlx[paste0(question,"_",task,"c")] <- data_relevant[,paste0(question, "_",task,"c")]
  }
}

data_parameters <- data.frame(row.names = 1:nrow(data_relevant))

for (question in c("parameters", "quality", "size", "threshold"))
{
  for (task in 1:3)
  {
    if (question != "threshold") {
      data_parameters[paste0(question,"_",task,"b")] <- data_relevant[,paste0(question, "_",task,"b")]
      data_parameters[paste0(question,"_",task,"c")] <- data_relevant[,paste0(question, "_",task,"c")]
    } else {
      data_parameters[paste0(question,"_",task,"c")] <- factor(data_relevant[,paste0(question, "_",task,"c")], ordered=TRUE, levels=c("0", "1-100", "101-200"))
    }
    
  }
}

# Gather data in columns
data_tlx_clean <- data.frame(row.names = 1:(nrow(data_relevant)*length(question_labels)))
for (task in 1:3)
{
  for (method in c("a", "b", "c"))
  {
    values <- c()
    for (question in question_labels)
    {
      values <- c(values, data_tlx[,paste0(question,"_",task,method)])
    }
    data_tlx_clean[paste0("accumulated_",task,method)] <- values
  }
}
data_tlx_clean["participant"] = rep(1:nrow(data_tlx), times=length(question_labels))
data_tlx_clean["question"] = factor(rep(gsub("_", "\n", question_labels), each=nrow(data_tlx)), levels=gsub("_", "\n", question_labels))

# Sum over questions
data_tlx_sum <- aggregate(data_tlx_clean[,1:9], by = list(data_tlx_clean$participant), FUN = sum)
data_tlx_sum[2:ncol(data_tlx_sum)] <- data_tlx_sum[2:ncol(data_tlx_sum)] / length(question_labels)
names(data_tlx_sum)[1] <- 'participant'

# Compute favorites
tlx_favorites_freq <- data.frame(method = c("A", "B", "C"))
tlx_favorites <- data.frame(participant = 1:nrow(data_all))
for (task in 1:3)
{
  frequ_a <- 0
  frequ_b <- 0
  frequ_c <- 0
  fav <- c()
  for (i in 1:nrow(data_tlx))
  {
    if(data_tlx_sum[i,paste0("accumulated_",task,"b")] > data_tlx_sum[i,paste0("accumulated_",task,"c")])
    {
      if(data_tlx_sum[i,paste0("accumulated_",task,"b")] > data_tlx_sum[i,paste0("accumulated_",task,"a")])
      {
        frequ_b <- frequ_b + 1
        fav <- c(fav, "B")
      }
      else
      {
        frequ_a <- frequ_a + 1
        fav <- c(fav, "A")
      }
    }
    else if(data_tlx_sum[i,paste0("accumulated_",task,"c")] > data_tlx_sum[i,paste0("accumulated_",task,"a")])
    {
      frequ_c <- frequ_c + 1
      fav <- c(fav, "C")
    }
    else
    {
      frequ_a <- frequ_a + 1
      fav <- c(fav, "A")
    }
  }
  tlx_favorites_freq[paste0("frequencies_", task)] <- c(frequ_a, frequ_b, frequ_c)
  tlx_favorites[paste0("fav_", task)] <- factor(fav, levels=c("A", "B", "C"))
}
frequ_a <- rowSums(tlx_favorites_freq[1,2:4])
frequ_b <- rowSums(tlx_favorites_freq[2,2:4])
frequ_c <- rowSums(tlx_favorites_freq[3,2:4])
tlx_favorites_freq["frequencies_overall"] <- c(frequ_a, frequ_b, frequ_c)
fav <- c()
for (i in 1:nrow(data_tlx))
{
  fav <- c(fav, majorityVote(c(tlx_favorites[i,2],tlx_favorites[i,3], tlx_favorites[i,4]))$majority)
}
tlx_favorites["fav_overall"] <- factor(fav, levels=c("A", "B", "C"))
tlx_favorites["fav_stated"] <- data_relevant["preference"]
tlx_favorites$fav_stated <- factor(tlx_favorites$fav_stated)
tlx_favorites[tlx_favorites$fav_1!=tlx_favorites$fav_2 & tlx_favorites$fav_2!=tlx_favorites$fav_3 & tlx_favorites$fav_1!=tlx_favorites$fav_3,5] <- NA
tlx_favorites[tlx_favorites$fav_overall=="A" & tlx_favorites$fav_stated=="C",]
tlx_favorites["fav_overall_group"] <- as.factor(tlx_favorites$fav_overall=="A")
levels(tlx_favorites$fav_overall_group) <- c("Incremental", "Global")
tlx_favorites["fav_stated_group"] <- as.factor(tlx_favorites$fav_stated=="A")
levels(tlx_favorites$fav_stated_group) <- c("Incremental", "Global")

# Compute differences
data_tlx_diff_a_b <- data.frame(row.names = 1:nrow(data_tlx))
for (task in 1:3)
{
  differences <- data_tlx_sum[,paste0("accumulated_",task,"b")]-data_tlx_sum[,paste0("accumulated_",task,"a")]
  data_tlx_diff_a_b[paste0("differences_", task)] <- differences
}
data_tlx_diff_a_c <- data.frame(row.names = 1:nrow(data_tlx))
for (task in 1:3)
{
  differences <- data_tlx_sum[,paste0("accumulated_",task,"c")]-data_tlx_sum[,paste0("accumulated_",task,"a")]
  data_tlx_diff_a_c[paste0("differences_", task)] <- differences
}
data_tlx_diff_c_b <- data.frame(row.names = 1:nrow(data_tlx))
for (task in 1:3)
{
  differences <- data_tlx_sum[,paste0("accumulated_",task,"b")]-data_tlx_sum[,paste0("accumulated_",task,"c")]
  data_tlx_diff_c_b[paste0("differences_", task)] <- differences
}
data_tlx_diff_c_b_abs <- abs(data_tlx_diff_c_b)
data_tlx_diff_a_fav <- data.frame(row.names = 1:nrow(data_tlx))
for (task in 1:3)
{
  differences <- c()
  for (i in 1:nrow(data_tlx))
  {
    if(data_tlx_sum[i,paste0("accumulated_",task,"b")] > data_tlx_sum[i,paste0("accumulated_",task,"c")])
    {
      differences <- c(differences, data_tlx_sum[i,paste0("accumulated_",task,"b")]-data_tlx_sum[i,paste0("accumulated_",task,"a")])
    }
    else
    {
      differences <- c(differences, data_tlx_sum[i,paste0("accumulated_",task,"c")]-data_tlx_sum[i,paste0("accumulated_",task,"a")])
    }
  }
  data_tlx_diff_a_fav[paste0("differences_", task)] <- differences
}
data_tlx_diffs <- cbind(structure_diff_data_for_plotting(data_tlx_diff_a_fav)[1],
                        structure_diff_data_for_plotting(data_tlx_diff_a_b)[1],
                        structure_diff_data_for_plotting(data_tlx_diff_a_c)[1],
                        structure_diff_data_for_plotting(data_tlx_diff_c_b))
data_tlx_diffs <- cbind(data_tlx_diffs, rep(1:21, 3))
names(data_tlx_diffs) <- c("diffs_a_fav", "diffs_a_b", "diffs_a_c", "diffs_b_c", "task", "participant")
data_tlx_diffs_by_task <- data.frame(bla=rep(1,nrow(data_tlx)))
for (i in 1:4)
{
  data_tlx_diffs_by_task <- cbind(data_tlx_diffs_by_task, data_tlx_diffs[data_tlx_diffs$task=="Small Objects", c(i,5)][1], data_tlx_diffs[data_tlx_diffs$task=="Large Objects", c(i,5)][1], data_tlx_diffs[data_tlx_diffs$task=="Lights", c(i,5)][1])
}
colnames(data_tlx_diffs_by_task) <- c("task", paste0(rep("values",12), "_", rep(1:3,4), "_", rep(diff_short_labes, each=3)))

get_summary_stats(data_tlx_sum, type = "full")
get_summary_stats(data_tlx_diff_a_b, type = "full")
get_summary_stats(data_tlx_diff_a_c, type = "full")
get_summary_stats(data_tlx_diff_c_b_abs, type = "full")
get_summary_stats(data_tlx_diff_c_b, type = "full")
get_summary_stats(data_tlx_diff_a_fav, type = "full")
tlx_favorites_freq
summary(tlx_favorites)
summary(data_parameters[,7:18])
quantile(data_parameters$threshold_1c, probs = c(0,0.25,0.5,0.75,1), type=1, na.rm=TRUE)
quantile(data_parameters$threshold_2c, probs = c(0,0.25,0.5,0.75,1), type=1, na.rm=TRUE)
quantile(data_parameters$threshold_3c, probs = c(0,0.25,0.5,0.75,1), type=1, na.rm=TRUE)


################################################################################
#####################     Correlations              ############################
################################################################################

#Cramers V = effect size for all
#df = 2 -> large if >0.35
catcorrm <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(dat[,x], dat[,y]))$cramer))
cormat <- catcorrm(c("fav_1", "fav_2", "fav_3", "fav_overall", "fav_stated"), tlx_favorites)
rownames(cormat)<-c("small obj.", "large obj.", "lights", "majority vote", "stated")
colnames(cormat)<-c("small obj.", "large obj.", "lights", "majority vote", "stated")
cormat
catp <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) fisher.test(table(dat[,x], dat[,y]))$p))
pmat <- catp(c("fav_1", "fav_2", "fav_3", "fav_overall", "fav_stated"), tlx_favorites)
rownames(pmat)<-c("small obj.", "large obj.", "lights", "majority vote", "stated")
colnames(pmat)<-c("small obj.", "large obj.", "lights", "majority vote", "stated")
pmat
pdf(file=paste0(out_path,"fav_assoc.pdf"), width=10, height=10)
corrplot(cormat, p.mat=pmat, insig='label_sig', type='lower', diag = FALSE, addCoef.col = 'black', number.cex=1.5,tl.srt=45,
         tl.cex = 1.5, tl.pos = 'ld', cl.pos = 'n', col = COL2('PiYG'), tl.col = 'black', mar=c(2,2,2,2), cex.main=2)
mtext(text = "Association (Cramer's V) between Favorites", side = 3, line = 0.5, cex=2)
dev.off()

con_group <- table(tlx_favorites$fav_overall_group, tlx_favorites$fav_stated_group)
fisher.test(con_group)
cohen.kappa(con_group, n.obs=21)

#McNemar on 2x2, if >0.05 all entries are the same
#mcnemar all values need to be different -> not suitable
tlx_fav <- data.frame(tlx_favorites$participant)
tlx_fav["stated_A"] <- as.factor(tlx_favorites$fav_stated == "A")
tlx_fav["chose_A"] <- as.factor(tlx_favorites$fav_overall == "A")
tlx_fav["chose_stated"] <- as.factor(tlx_favorites$fav_overall == tlx_favorites$fav_stated)
#tlx_fav$chose_A[is.na(tlx_fav$chose_A)] <- "FALSE"
tlx_fav$chose_A <- factor(tlx_fav$chose_A, levels=c("TRUE", "FALSE"))
tlx_fav$stated_A <- factor(tlx_fav$stated_A, levels=c("TRUE", "FALSE"))
tlx_fav$chose_stated <- factor(tlx_fav$chose_stated, levels=c("TRUE", "FALSE"))
con <- table(tlx_fav$chose_A, tlx_fav$stated_A)
con
con1 <- table(tlx_fav$chose_stated, tlx_fav$chose_A)
con1

cohen.kappa(con, n.obs=21-4)
#mcnemar.test(con, correct=TRUE)
chisq.test(con)
fisher.test(con)
oddsratio(con)


contingency_table<-table(tlx_favorites$fav_overall,tlx_favorites$fav_stated)
contingency_table
mosaicplot(contingency_table)
cramerV(contingency_table, bias.correct = FALSE) # should equal entry in matrix above

csq <- chisq.test(contingency_table)
csq
csq$expected
#expected values in one of the cells of the contingency table is less than 5, and in this case the Fisher’s exact test is preferred (McCrum-Gardner 2008; Bower 2003).
fisher.test(contingency_table)
GTest(contingency_table)

mat_residuals <- round(csq$residuals, 3)
rownames(mat_residuals) <- method_labels
colnames(mat_residuals) <- method_labels
pdf(file=paste0(out_path, "residuals.pdf"), width=10, height=10)
corrplot(mat_residuals, is.cor = FALSE, addCoef.col = 'black', tl.cex = 1.5,col = COL2('PiYG'),tl.col = 'black', cex.main=1, number.cex=1.5, mar = c(0,4,7,4),col.lim=c(-1.6,1.6),tl.srt=45, tl.offset=1)
mtext(text = expression(bold("Majority Vote Favorite")), side = 2, line = 2, cex=1.5)
mtext(text = expression(bold("Stated Favorite")), side = 3, line = -2, cex=1.5)
mtext(text = expression(paste("Residuals of ", chi^2, " test between evualuated and stated Favorite")), side = 3, line = 1, cex=2)
dev.off()

# nur für unabhängige rater
# >0.2 Fair, >0.4 Moderate, >0.6 Substantial
cohen_weights <- matrix(c(
  0,1,1,
  1,0,1,
  1,1,0),ncol=3)
cohen.kappa(contingency_table, cohen_weights, n.obs=21-4)

mat<-cor(data_tlx_sum[-1])
testRes <- cor.mtest(data_tlx_sum[-1], conf.level = 0.95)
corrplot(mat, method = 'color', type = 'lower', diag = FALSE, tl.col = 'black',tl.srt = 45, addrect = 2, p.mat = testRes$p, sig.level = 0.01,insig='blank',addCoef.col ='black',)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))

con <- table(tlx_favorites$fav_stated, data_meta$reversed)
con
fisher.test(con)
test<-glm(fav_stated ~ fav_overall, data=tlx_favorites, family = 'binomial')
summary(test)

con <- table(tlx_favorites$fav_stated, data_meta$participation)
con
fisher.test(con)
test<-glm(fav_stated ~ participation, data=data_meta, family = 'binomial')
summary(test)

################################################################################
#####################     Plot Nasa-tlx data        ############################
################################################################################

acc_grid <- plot_grid(data_tlx_clean, plot_tlx_all, "Raw Question Data")
acc_grid

sum_row <- plot_row(data=data_tlx_sum, fun=plot_tlx_sum, title="Summed Data", num_plots = 3)
sum_row

diff_row <- plot_row(data=data_tlx_diffs, fun=plot_tlx_diff, title="Differences", pos="top", num_plots=4, margins = list(c(0,30,0,0), c(0,30,0,0), c(0,0,0,0), c(0,0,0,30)))
diff_row

# QQ PLOTS
row <- plot_row(data=data_tlx_diffs_by_task, fun=plot_qq, title="QQ-Plots for Differences",num_plots=12, margins=rep(list(c(30,0,0,30), c(30,0,0,0), c(30,30,0,0)), 4))
row

layout <- "
A
B
"
wrap_plots(A=diff_row, B=row, design = layout, heights = c(3,1)) + plot_annotation(subtitle = "Pairwise Differences between the Rendering Methods by Editing Scenario") & theme(plot.subtitle = element_text(size=15))
ggsave(paste0(out_path, "differences", file_ending), width=14, height=7)

# Preference
data_preference <- data_relevant["preference"]
data_preference["participants"] = 1:nrow(data_preference)
data_preference <- aggregate(participants~preference,data_preference,length)
data_preference$method <- factor(data_preference$preference)
data_preference$preference <- factor(c("Global\nUpdate", "Incremental\nUpdate", "Incremental\nUpdate"), levels=c("Incremental\nUpdate", "Global\nUpdate"))
levels(data_preference$method) <- method_labels
pref_plot <- ggplot(data_preference, aes(x=preference, y = participants, fill=method)) + 
  geom_bar(stat = "identity") + 
  theme_classic(base_size=23) + 
  labs(title="Overall prefered Method", subtitle = "Selected in the General Section", fill="Method")+xlab("Preference")+ylab("Number of Participants")+
  scale_fill_manual(values=color_methods)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 14), breaks=c(0,4,8,12))
pref_plot
ggsave(paste0(out_path, "preference", file_ending), width=10, height=10)

data_majority <- data.frame(participants=c(aggregate(participant~fav_overall,tlx_favorites,length)$participant, sum(is.na(tlx_favorites$fav_overall))))
data_majority["method"] <- factor(c(method_labels, "Undecided"), levels = c(method_labels, "Undecided"))
data_majority$preference <- factor(c("Global\nUpdate", "Incremental\nUpdate", "Incremental\nUpdate", "Undecided"), levels=c("Incremental\nUpdate", "Global\nUpdate", "Undecided"))
pref_plot <- ggplot(data_majority, aes(x=preference, y = participants, fill=method)) + 
  geom_bar(stat = "identity") + 
  theme_classic(base_size=23) + 
  labs(title="Overall prefered Method", subtitle = "Computed via Majority Vote", fill="Method")+xlab("Preference")+ylab("Number of Participants")+
  scale_fill_manual(values=c(color_methods, '#000000'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10.5), breaks=c(0,2,4,6,8,10))
pref_plot
ggsave(paste0(out_path, "majority", file_ending), width=10, height=10)

# NPS
nps_threshold1 <- 7.5
nps_threshold2 <- 5.5
nps_scores <- calculate_nps(data_relevant, nps_threshold1, nps_threshold2)
data_nps <- data.frame(nps_score = c(data_relevant[,"buy_nps_a"], data_relevant[,"buy_nps_b"], data_relevant[,"buy_nps_c"]))
data_nps["method"] = factor(rep(method_labels, each=nrow(data_relevant)), levels=method_labels)
data_nps["nps_scores"] = rep(round(nps_scores,4)*100, each=nrow(data_relevant))
nps_plot <- ggplot(data_nps, aes(x=method, y=nps_score)) + 
  geom_hline(yintercept=nps_threshold2) + 
  geom_hline(yintercept=nps_threshold1) + 
  geom_boxplot(outlier.shape=8, size=1.5, aes(color=method)) + 
  geom_jitter(height=0.24, width=0.3, size=3, aes(color=method)) +
  theme_classic(base_size=23) +
  geom_text(aes(label=paste0("nps = ",nps_scores)), y=10.7,size=8) +
  labs(title="Raw Data for Net Promoter Score", color="Method") + ylab("NPS Score") + xlab("Method") +
  scale_color_manual(values=color_methods) +
  scale_y_continuous(breaks=c(0,2,4,6,8,10), limits = c(-0.5,10.7))
nps_plot
ggsave(paste0(out_path,"nps",file_ending),width=10,height=10)

calculate_nps(data_relevant, 6.5, 4.5)
calculate_nps(data_relevant, 7.5, 5.5)
calculate_nps(data_relevant, 8.5, 6.5)

# TEST
ggplot(data_tlx_diff_a_fav, aes(differences_1)) + 
  geom_histogram(fill = "white", color = "grey30")
gghistogram(data_tlx_diff_a_fav, x = "differences_1", y = "..density..", fill = "steelblue",add_density = TRUE, bins=8)
gghistogram(data_tlx_diff_a_fav, x = "differences_2", y = "..density..", fill = "steelblue",add_density = TRUE, bins=8)
gghistogram(data_tlx_diff_a_fav, x = "differences_3", y = "..density..", fill = "steelblue",add_density = TRUE, bins=8)


################################################################################
#####################     Statistical Testing        ###########################
################################################################################

# test normality, p should be above 0.05
data_test <- data_tlx_diffs_by_task[2:13]
#data_test[13,10]<-NA
test <- data.frame(values = as.vector(as.matrix(data_test)), id=rep(1:12, each=nrow(data_tlx)))
test %>%
  group_by(id) %>%
  shapiro_test(values)

test %>%
  group_by(id) %>%
  identify_outliers(values)

t.test(data_tlx_diff_a_fav$differences_1, mu = 0, alternative = "greater")
wilcox.test(data_tlx_diff_a_fav$differences_1, mu = 0, alternative = "greater")
cohens_d(data_tlx_diff_a_fav, differences_1 ~ 0)

# test C - A
shapiro.test(data_tlx_diff_a_c$differences_1)
shapiro.test(data_tlx_diff_a_c$differences_2)
shapiro.test(data_tlx_diff_a_c$differences_3)
identify_outliers(data_tlx_diff_a_c, differences_1)
identify_outliers(data_tlx_diff_a_c, differences_2)
identify_outliers(data_tlx_diff_a_c, differences_3)
t.test(data_tlx_diff_a_c$differences_1, mu = 0, alternative = "greater")
cohens_d(data_tlx_diff_a_c, differences_1 ~ 0)
t.test(data_tlx_diff_a_c$differences_3, mu = 0, alternative = "less")
cohens_d(data_tlx_diff_a_c*-1, differences_3 ~ 0)

#anovas
anova_data <- structure_diff_data_for_plotting(data_tlx_diff_a_c)
oneway <- aov(data=anova_data, values ~ task)
summary(oneway)
model <- lm(values ~ task, data = anova_data)
model
anova_data["participant"] <- rep(1:nrow(data_tlx), times=ncol(data_tlx_diff_a_c))
res.aov <- anova_test(data = anova_data, dv = values, within = task, wid=participant)
get_anova_table(res.aov)
pwc <- anova_data %>%
  pairwise_t_test(
    values ~ task, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
pwc <- pwc %>% add_xy_position(x = "task")
ggboxplot(anova_data, x = "task", y = "values", add = "point") + 
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

anova_data <- structure_diff_data_for_plotting(data_tlx_diff_c_b)
oneway <- aov(data=anova_data, values ~ task)
summary(oneway)

anova_data <- structure_diff_data_for_plotting(data_tlx_diff_a_b)
oneway <- aov(data=anova_data, values ~ task)
summary(oneway)

anova_data <- structure_diff_data_for_plotting(data_tlx_diff_a_fav)
oneway <- aov(data=anova_data, values ~ task)
summary(oneway)

# test raw data
test_data <- data.frame(values = as.vector(as.matrix(data_tlx_sum[,c("accumulated_1a", "accumulated_1c")])))
test_data["method"] <- factor(rep(c(1, 2), each=nrow(data_relevant)))
# shapiro test done on differences
t_test(test_data, values ~ method, paired=TRUE)
cohens_d(test_data, values ~ method, paired = TRUE)
wilcox_test(test_data, values~method, paired=TRUE)
wilcox_effsize(test_data, values~method, paired = TRUE)
t_test(test_data, values ~ method, paired=TRUE)
cohens_d(test_data, values ~ method, paired = TRUE)
test_data$values <- as.vector(as.matrix(data_tlx_sum[,c("accumulated_3a", "accumulated_3c")]))
t_test(test_data, values ~ method, paired=TRUE)
cohens_d(test_data, values ~ method, paired = TRUE)

data_tlx_sum_new <- data_tlx_sum
#data_tlx_sum_new[13,"accumulated_1a"] <- NA
#data_tlx_sum_new[13,"accumulated_1b"] <- NA
#data_tlx_sum_new[13,"accumulated_1c"] <- NA
#data_tlx_sum_new[19,"accumulated_3c"] <- NA
#data_tlx_sum_new[19,"accumulated_3b"] <- NA
#data_tlx_sum_new[19,"accumulated_3a"] <- NA
anova_data_sum <- data.frame(values = as.vector(as.matrix(data_tlx_sum_new[,c("accumulated_1a", "accumulated_1b", "accumulated_1c", "accumulated_2a", "accumulated_2b", "accumulated_2c", "accumulated_3a", "accumulated_3b", "accumulated_3c")])))
anova_data_sum["method"] <- factor(rep(method_labels, each=nrow(data_all), times=3), levels=method_labels)
anova_data_sum["task"] <- factor(rep(task_labels, each=nrow(data_all)*3), levels=task_labels)
anova_data_sum["participant"] <- rep(1:nrow(data_all), times=9)

anova_data_sum %>%
  group_by(task) %>%
  cohens_d(values ~ method, paired=TRUE)
LeveneTest(values ~ method*task, anova_data_sum)

anova_data_sum %>%
  group_by(task, method) %>%
  get_summary_stats(values, type = "mean_sd")
anova_data_sum %>%
  group_by(task, method) %>%
  shapiro_test(values)
ggqqplot(anova_data_sum, "values", ggtheme = theme_bw()) +
  facet_grid(task ~ method, labeller = "label_both")
anova_data_sum %>%
  group_by(task, method) %>%
  identify_outliers(values)
res.aov <- anova_test(
  data = anova_data_sum, dv = values, wid = participant,
  within = c(method, task), effect.size="ges"
)
res.aov
get_anova_table(res.aov, correction = "GG")

library(rmcorr)
corr_test <- data.frame(m1 = anova_data_sum[anova_data_sum$task=="Small Objects",]$values,
                        m2 = anova_data_sum[anova_data_sum$task=="Large Objects",]$values,
                        m3 = anova_data_sum[anova_data_sum$task=="Lights",]$values,
                        participant = rep(1:nrow(data_all), times=3))
rmcorr(participant, m1, m2, corr_test)
rmcorr(participant, m2, m3, corr_test)
rmcorr(participant, m1, m3, corr_test)

one.way <- anova_data_sum %>%
  group_by(method) %>%
  anova_test(dv = values, wid = participant, within = task) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way
pwc <- anova_data_sum %>%
  group_by(method) %>%
  pairwise_t_test(
    values ~ task, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
one.way2 <- anova_data_sum %>%
  group_by(task) %>%
  anova_test(dv = values, wid = participant, within = method) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2
pwc2 <- anova_data_sum %>%
  group_by(task) %>%
  pairwise_t_test(
    values ~ method, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2
pwc <- pwc %>% add_xy_position(x = "method")
pwc2 <- pwc2 %>% add_xy_position(x = "task")
ggboxplot(anova_data_sum, x = "method", y = "values", color = "task", palette = "jco") + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(subtitle = get_test_label(res.aov, detailed = TRUE), caption = get_pwc_label(pwc))


ggplot_anova <- ggboxplot(anova_data_sum, x = "task", y = "values", fill = "method", palette = color_methods, size=0.9, bxp.errorbar = TRUE) +
  #geom_hline(yintercept=0) +
  theme_classic(15) +
  stat_summary(aes(group=method), fun=mean, geom="point", shape=18, size=3, position=position_dodge(0.8)) +
  theme(legend.position = "bottom") +
  stat_pvalue_manual(pwc2, tip.length = 0, hide.ns = TRUE) +
  labs( subtitle = get_test_label(res.aov, detailed = TRUE), caption = get_pwc_label(pwc2), fill="Method", title="Averaged Task Question Score by Rendering Method and Editing Scenario") +
  xlab("Editing Scenario") + 
  ylab("Aggregated Score")
ggplot_anova
ggsave(paste0(out_path, "anova", file_ending), width=10, height=6)


# REGRESSION
fit <- lm(diffs_a_c ~ task, data = data_tlx_diffs)
summary(fit)

library(lme4)
library(lmerTest)
test<-lmer(formula = diffs_a_c ~ task + (1|participant), data = data_tlx_diffs)
test<-lmer(formula = diffs_a_c ~ 1 + task + (1 + task|participant), data = data_tlx_diffs)
summary(test)
autoplot(test)



m0.glm <- glm(diffs_a_c ~ 1, family = gaussian, data = data_tlx_diffs)
m0.lmer = lmer(diffs_a_c ~ 1 + (1|participant), REML = T, data = data_tlx_diffs)
AIC(logLik(m0.glm))
AIC(logLik(m0.lmer))

ma.glm <- glm(diffs_a_c ~ task, data = data_tlx_diffs)
ma.glm
summary(ma.glm)
ma.lm <- lm(diffs_a_c ~ task, data = data_tlx_diffs)
ma.lm
summary(ma.lm)
AIC(logLik(ma.lm))
plot(ma.lm)

ma.lmer = lmer(diffs_a_c ~ task + (1|participant), REML = T, data = data_tlx_diffs)
mb.lmer = lmer(diffs_a_c ~ task + (1 + task | participant), REML = T, data = data_tlx_diffs)
anova(ma.lmer, mb.lmer, test = "Chisq", refit = F)



m0.glm <- lm(values ~ 1, data = anova_data_sum)
m0.lmer = lmer(values ~ 1 + (1|participant), REML = T, data = anova_data_sum)
AIC(logLik(m0.glm))
AIC(logLik(m0.lmer))
ma.lmer = lmer(values ~ task + (1|participant), REML = T, data = anova_data_sum)
mb.lmer = lmer(values ~ task + (1 + task | participant), REML = T, data = anova_data_sum)
anova(ma.lmer, mb.lmer, test = "Chisq", refit = F)



anova_data_sum <- data.frame(values = as.vector(as.matrix(data_tlx_sum[,c("accumulated_1a", "accumulated_1c", "accumulated_2a", "accumulated_2c", "accumulated_3a", "accumulated_3c")])))
anova_data_sum["method"] <- factor(rep(c("Denoised", "Denoised+Incremental"), each=nrow(data_all), times=3), levels=c("Denoised", "Denoised+Incremental"))
anova_data_sum["task"] <- factor(rep(task_labels, each=nrow(data_all)*2), levels=task_labels)
twoway <- aov(data=anova_data_sum, values ~ task * method)
summary(twoway)

anova_data_sum <- data.frame(values = as.vector(as.matrix(data_tlx_sum[,c("accumulated_1a", "accumulated_1c", "accumulated_3a", "accumulated_3c")])))
anova_data_sum["method"] <- factor(rep(c("Denoised", "Denoised+Incremental"), each=nrow(data_all), times=2), levels=c("Denoised", "Denoised+Incremental"))
anova_data_sum["task"] <- factor(rep(c("Small Objects", "Lights"), each=nrow(data_all)*2), levels=c("Small Objects", "Lights"))
twoway <- aov(data=anova_data_sum, values ~ task * method)
summary(twoway)

anova_data <- data.frame(values = as.vector(as.matrix(data_tlx_sum[,c("accumulated_1a", "accumulated_1b", "accumulated_1c")])))
anova_data["task"] <- factor(rep(task_labels, each=nrow(data_tlx)), levels=task_labels)
oneway <- aov(data=anova_data, values ~ task)
summary(oneway)


           





regression_data_experience <- anova_data_sum
regression_data_experience["status"] <- rep(data_general$status, 9)
regression_data_experience["experience"] <- rep(data_general$experience, 9)
levels(regression_data_experience$experience) <- c(1:6)

m0.lm <- lm(values ~ 1, data = regression_data_experience)
m0.glm <- lm(values ~ 1, family = gaussian, data = regression_data_experience)
m0.lmer = lmer(values ~ 1 + (1|participant), REML = T, data = regression_data_experience)
AIC(logLik(m0.lm))
AIC(logLik(m0.glm))
AIC(logLik(m0.lmer))
ma.lmer = lmer(values ~ experience + (1|participant), REML = T, data = regression_data_experience)
mb.lmer = lmer(values ~ experience + (1 + experience | participant), REML = T, data = regression_data_experience)
anova(ma.lmer, mb.lmer, test = "Chisq", refit = F)
ma.lmer = lmer(values ~ status + (1|participant), REML = T, data = regression_data_experience)
mb.lmer = lmer(values ~ status + (1 + status | participant), REML = T, data = regression_data_experience)
anova(ma.lmer, mb.lmer, test = "Chisq", refit = F)
ma.lmer = lmer(values ~ status + (1|participant), REML = T, data = regression_data_experience)
mb.lmer = lmer(values ~ experience + status + (1|participant), REML = T, data = regression_data_experience)
anova(ma.lmer, mb.lmer, test = "Chisq", refit = F)
ma.lmer = lmer(values ~ experience + status + (1|participant), REML = T, data = regression_data_experience)
mb.lmer = lmer(values ~ experience + status + experience:status + (1|participant), REML = T, data = regression_data_experience)
anova(ma.lmer, mb.lmer, test = "Chisq", refit = F)


summary(mb.lmer)

mb.lmer
require(MuMIn)
r.squaredGLMM(mb.lmer) #conditional interpreted as a variance explained by the entire mode

ggplot(regression_data_experience, aes(x=experience, y=values, color=status))+geom_point()

table(data_general$experience, data_general$status)
chisq.test(table(data_general$experience, data_general$status))
fisher.test(table(data_general$experience, data_general$status))

regression_data_experience["reversed"] <- rep(data_meta$reversed, 9)
regression_data_experience["participation"] <- rep(data_meta$participation, 9)
mb.lmer <- lmer(values ~ reversed + (1|participant), REML = T, data = regression_data_experience)
summary(mb.lmer)
mb.lmer
r.squaredGLMM(mb.lmer)
mb.lmer <- lmer(values ~ participation + (1|participant), REML = T, data = regression_data_experience)
summary(mb.lmer)
mb.lmer
r.squaredGLMM(mb.lmer)
