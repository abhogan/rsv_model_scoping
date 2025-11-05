library(tidyverse)
library(RColorBrewer)
library(ggpubr)

study_cols <- c("#14785a","#7ebf34",  "#D95F02", "#E7298A", "#7570B3", "#c1beed",  "#E6AB02")

theme_set(theme_bw(base_size = 14))
th <- theme(strip.background = element_rect(fill = NA),
            panel.border = element_blank(),
            axis.line = element_line(),
            legend.text=element_text(size=14, hjust = 0),
            legend.title=element_text(size=14),
            axis.text=element_text(size=14),
            axis.label=element_text(size=11),
            strip.text = element_text(size = 14))

dat <- read_csv("data.csv") %>%
  mutate(month = (month_upper-month_lower)/2+month_lower,
         study = paste0(author, " ", year, " ", strategy),
         paper = paste0(author, " ", year)) %>%
  pivot_longer(cols = c(`averted per 1000`, `percentage reduction`)) %>%
  mutate(group = paste0(paper, ", coverage ", coverage))

head(dat)

###########
# aligned axes plots

dat_mat1 <- dat %>%
  filter(type == "maternal",
         name == "averted per 1000")

dat_mat2 <- dat %>%
  filter(type == "maternal",
         name == "percentage reduction")

p1 <- ggplot(data = dat_mat1, aes(x=month, y = value, col = group, linetype = strategy)) +
  geom_line() +
  geom_point() +
  labs(x = "age (months)", linetype = "strategy", col = "study", y = "averted per 1000") +
  facet_wrap(~type, scales = "free") +
  th +
  scale_color_manual(values = c(study_cols[1:4],study_cols[7]))+
  scale_linetype_manual(values = c(3,1)) +
  scale_x_continuous(breaks = c(0,6,12,18,24), limits = c(0,24)) +
  scale_y_continuous(limits = c(-5, 40))

p2 <- ggplot(data = dat_mat2, aes(x=month, y = value, col = group, linetype = strategy)) +
  geom_line() +
  geom_point() +
  labs(x = "age (months)", linetype = "strategy", col = "study", y = "percentage reduction (%)") +
  facet_wrap(~type, scales = "free") +
  th +
  scale_color_manual(values = c(study_cols[1:4],study_cols[7]))+
  scale_linetype_manual(values = c(3,1)) +
  scale_x_continuous(breaks = c(0,6,12,18,24), limits = c(0,24)) +
  scale_y_continuous(limits = c(-15, 75))

dat_mono1 <- dat %>%
  filter(type == "monoclonal",
         name == "averted per 1000")

dat_mono2 <- dat %>%
  filter(type == "monoclonal",
         name == "percentage reduction")

p3 <- ggplot(data = dat_mono1, aes(x=month, y = value, col = group, linetype = strategy)) +
  geom_line() +
  geom_point() +
  labs(x = "age (months)", linetype = "strategy", col = "study", y = "averted per 1000") +
  facet_wrap(~type, scales = "free") +
  th +
  scale_color_manual(values = c(study_cols[1:2],study_cols[4:7]))+
  scale_linetype_manual(values = c(3,2,1)) +
  scale_x_continuous(breaks = c(0,6,12,18,24), limits = c(0,24)) +
  scale_y_continuous(limits = c(-5, 40))

p4 <- ggplot(data = dat_mono2, aes(x=month, y = value, col = group, linetype = strategy)) +
  geom_line() +
  geom_point() +
  labs(x = "age (months)", linetype = "strategy", col = "study", y = "percentage reduction (%)") +
  facet_wrap(~type, scales = "free") +
  th +
  scale_color_manual(values = c(study_cols[1:2],study_cols[4:7]))+
  scale_linetype_manual(values = c(3,2,1)) +
  scale_x_continuous(breaks = c(0,6,12,18,24), limits = c(0,24)) +
  scale_y_continuous(limits = c(-15, 75))

p5 <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "right", labels = c("A", ""))
p6 <- ggarrange(p3, p4, ncol = 2, common.legend = TRUE, legend = "right", labels = c("B", ""))


x <- ggarrange(p5, p6, nrow = 2)
x
ggsave("main_plot.png", x, height = 17, width = 30, units = c("cm"))

############################################

# Values for paper text

# impact in children 0-3 months : maternal

x <- dat %>%
  filter(month_lower %in% c(0,1,2),
         month_upper %in% c(1,2,3)) %>%
  filter(type == "maternal",
         name == "percentage reduction")
max(x$value)
min(x$value)

# impact in children 0-3 months : maternal, year-round

x <- dat %>%
  filter(month_lower %in% c(0,1,2),
         month_upper %in% c(1,2,3)) %>%
  filter(type == "maternal",
         name == "percentage reduction",
         strategy == "year-round")
max(x$value)
min(x$value)


y <- dat %>%
  filter(month_lower %in% c(0,1,2),
         month_upper %in% c(1,2,3)) %>%
  filter(type == "monoclonal",
         name == "percentage reduction")

max(y$value)
min(y$value)

x <- dat %>%
  filter(month_lower %in% c(0,1,2),
         month_upper %in% c(1,2,3)) %>%
  filter(type == "maternal",
         name == "averted per 1000")
max(x$value)
min(x$value)

###
# main results
x <- dat %>%
  filter(month_lower %in% c(0,1,2),
         month_upper %in% c(1,2,3)) %>%
  filter(type == "monoclonal",
         name == "percentage reduction")
median(x$value)
max(x$value)
min(x$value)

x <- dat %>%
  filter(month_lower %in% c(0,1,2),
         month_upper %in% c(1,2,3)) %>%
  filter(type == "monoclonal",
         name == "averted per 1000")
median(x$value)
max(x$value)
min(x$value)

x <- dat %>%
  filter(month_lower %in% c(3,4,5),
         month_upper %in% c(4,5,6)) %>%
  filter(type == "monoclonal",
         name == "percentage reduction",
         strategy != "year-round")
median(x$value)
max(x$value)
min(x$value)

x <- dat %>%
  filter(month_lower %in% c(3,4,5),
         month_upper %in% c(4,5,6)) %>%
  filter(type == "monoclonal",
         name == "percentage reduction")
median(x$value)
max(x$value)
min(x$value)

