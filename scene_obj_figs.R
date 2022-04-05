library(tidyverse)
library(ggplot2)
B = 1000        # number of samples (with replacement)

# subs = 10
# classified = scene/object (2) 
# ROI = Fovea, Peri, FF (3)
# Task = Scene, Object, Both (3)
expt1_df <- 'expt1_sub_x_classifed_x_ROI_x_Task.csv'
df <- read.csv(expt1_df, header=FALSE)
n_subs = nrow(df) # number of subjects
colnames(df)  <- c(
'scene_peri_scene',   'object_peri_scene',   'scene_ff_scene',   'object_ff_scene',   'scene_fov_scene',   'object_fov_scene',
'scene_peri_object',  'object_peri_object',  'scene_ff_object',  'object_ff_object',  'scene_fov_object',  'object_fov_object',
'scene_peri_both',    'object_peri_both',    'scene_ff_both',    'object_ff_both',    'scene_fov_both',    'object_fov_both'
)
# take to 'too long' format
mat <- data.matrix(df)
df <- as.data.frame.table(mat)
df <- as_tibble(df)
colnames(df) <- c('subject', 'condition', 'acc')
df <- subset(df, select = -subject)
# take to 'correct length' format
df <- df %>% separate(condition, (c('classify', 'roi', 'task')))
# set data types
df$classify <- as.factor(df$classify)
df$roi <- as.factor(df$roi)
df$task <- as.factor(df$task)
df$acc <- df$acc-0.5
# EXPERIMENT 1
# preallocate
lower = rep(-1,18)
upper = rep(-1,18)
p = rep(-1,18)
# horrible loop to get the bootstraps done
loop_count = 0
for (classify_idx in c('object', 'scene')) {
    for (roi_idx in c('ff', 'fov', 'peri')) {
        for (task_idx in c('both', 'object', 'scene')) {
            loop_count = loop_count + 1
            # print(c(classify_idx, roi_idx, task_idx))
            tmp <- subset(df, classify==classify_idx & roi==roi_idx & task==task_idx)
            boot.samples = matrix(sample(tmp$acc, size = B * n_subs, replace = TRUE), B, n_subs)
            boot.statistics = apply(boot.samples, 1, mean)
            lower[loop_count] <- sort(boot.statistics)[0.025*B]
            upper[loop_count] <- sort(boot.statistics)[0.975*B]
            p[loop_count] <- sum(boot.statistics < 0)/B
        }
    }
}
means <- df %>% group_by(classify, roi, task) %>% 
    summarise(acc = mean(acc))
# Add the 95% bootstrap CIs and p values
means$lower_boot <- lower
means$upper_boot <- upper
means$pvals <- p
# means %>% as_tibble() %>% print(n_subs=18)
summarise(acc = mean(acc))
# plot interesting parts of the data (ignore FF roi and combined task data)
task_names <- as_labeller(c('Scene', 'Object'))
subset(df, roi!='ff' & task!='both') %>% 
    ggplot(aes(x=factor(roi, c('peri', 'fov')),
               y=acc, 
               fill=factor(classify, c('scene', 'object')))) +
           geom_hline(yintercept=0) +
           geom_bar(stat='summary',
                    position=position_dodge(width=0.8), width=0.7, color='black') +
           geom_point(position=position_jitterdodge(jitter.width=0.2), color='darkgrey') +
           geom_errorbar(data=subset(means, roi!='ff' & task!='both'),
                         aes(ymin=lower_boot, ymax=upper_boot),
                         position=position_dodge(width=0.8), width=0.3) +
           scale_fill_manual(values = c('cornflowerblue', 'tomato3')) +
           facet_wrap(~factor(task, c('scene', 'object')), labeller=task_names) +
           coord_cartesian(ylim = c(-0.2, .2)) + 
           ylab('Classification Accuracy (-50%)') +
           xlab('ROI') +
           labs(fill = 'Classify')

# EXPERIMENT 2
# subs = 18
# classified = scene/BBQ presence/TENT presence/object (4) 
# ROI = Peri, FF, FoveaV123, FoveaV1  (4)
# Task = Scene, Object, Both (3)
expt2_df <- 'expt2_sub_x_classifed_x_ROI_x_Task.csv'
df <- read.csv(expt2_df, header=FALSE)
n_subs = nrow(df) # number of subjects
colnames(df)  <- c(
'scene_peri_scene',   'tent_peri_scene',   'bbq_peri_scene',   'object_peri_scene',   'scene_ff_scene',   'tent_ff_scene',   'bbq_ff_scene',   'object_ff_scene',   'scene_fovV123_scene',   'tent_fovV123_scene',   'bbq_fovV123_scene',   'object_fovV123_scene',   'scene_fov_scene',   'tent_fov_scene',   'bbq_fov_scene',   'object_fov_scene',
'scene_peri_object',  'tent_peri_object',  'bbq_peri_object',  'object_peri_object',  'scene_ff_object',  'tent_ff_object',  'bbq_ff_object',  'object_ff_object',  'scene_fovV123_object',  'tent_fovV123_object',  'bbq_fovV123_object',  'object_fovV123_object',  'scene_fov_object',  'tent_fov_object',  'bbq_fov_object',  'object_fov_object',
'scene_peri_both',    'tent_peri_both',    'bbq_peri_both',    'object_peri_both',    'scene_ff_both',    'tent_ff_both',    'bbq_ff_both',    'object_ff_both',    'scene_fovV123_both',    'tent_fovV123_both',    'bbq_fovV123_both',    'object_fovV123_both',    'scene_fov_both',    'tent_fov_both',    'bbq_fov_both',    'object_fov_both'
)
# take to 'too long' format
mat <- data.matrix(df)
df <- as.data.frame.table(mat)
df <- as_tibble(df)
colnames(df) <- c('subject', 'condition', 'acc')
df <- subset(df, select = -subject)
# take to 'correct length' format
df <- df %>% separate(condition, (c('classify', 'roi', 'task')))
# set data types
df$classify <- as.factor(df$classify)
df$roi <- as.factor(df$roi)
df$task <- as.factor(df$task)
df$acc <- df$acc-0.5
# preallocate
lower = rep(-1,48)
upper = rep(-1,48)
p = rep(-1,48)
# horrible loop to get the bootstraps done
loop_count = 0
for (classify_idx in c('bbq', 'object', 'scene', 'tent')) {
    for (roi_idx in c('ff', 'fov', 'fovV123', 'peri')) {
        for (task_idx in c('both', 'object', 'scene')) {
            loop_count = loop_count + 1
            # print(c(classify_idx, roi_idx, task_idx))
            tmp <- subset(df, classify==classify_idx & roi==roi_idx & task==task_idx)
            boot.samples = matrix(sample(tmp$acc, size = B * n_subs, replace = TRUE), B, n_subs)
            boot.statistics = apply(boot.samples, 1, mean)
            lower[loop_count] <- sort(boot.statistics)[0.025*B]
            upper[loop_count] <- sort(boot.statistics)[0.975*B]
            p[loop_count] <- sum(boot.statistics < 0)/B
        }
    }
}
means <- df %>% group_by(classify, roi, task) %>% 
   summarise(acc = mean(acc))
# Add the 95% bootstrap CIs and p values
means$lower_boot <- lower
means$upper_boot <- upper
means$pvals <- p
means %>% as_tibble() %>% print(n=48)
# plot interesting parts of the data (ignore FF roi and combined task data)
task_names <- as_labeller(c('Scene', 'Object'))
subset(df, roi!='ff' & roi!='fovV123' & task!='both') %>%
ggplot(aes(x=factor(roi, c('peri', 'fov')),
           y=acc,
           fill=factor(classify, c('scene', 'bbq', 'tent', 'object')))) +
       geom_hline(yintercept=0) +
       geom_bar(stat='summary',
                position=position_dodge(width=0.8), width=0.7, color='black') +
       geom_point(position=position_jitterdodge(jitter.width=0.2), color='darkgrey') +
       geom_errorbar(data=subset(means, roi!='ff' & task!='both'),
                     aes(ymin=lower_boot, ymax=upper_boot),
                     position=position_dodge(width=0.8), width=0.3) +
       scale_fill_manual(values = c('cornflowerblue', 'mediumpurple2', 'mediumpurple2', 'tomato3')) +
       facet_wrap(~factor(task, c('scene', 'object')), labeller=task_names) +
       coord_cartesian(ylim = c(-0.2, .2)) + 
       ylab('Classification Accuracy (-50%)') +
       xlab('ROI') +
       labs(fill = 'Classify')

