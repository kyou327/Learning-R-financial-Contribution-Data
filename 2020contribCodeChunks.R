
library(gender)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(lubridate)
library(RColorBrewer)

getwd()

OR <- read.csv('P00000001-OR.csv', row.names = NULL)

dim(OR)
str(OR)

p1 <- ggplot(aes(x = contb_receipt_amt), data = OR) +
  geom_histogram(bins = 50)
p2 <- ggplot(aes(x = 1, y = contb_receipt_amt), data = OR) +
  geom_boxplot()
grid.arrange(p1, p2, ncol = 2)


tail(sort(table(OR$contb_receipt_amt)), 5)

summary(OR)

ggplot(aes(x = contb_receipt_amt), data = OR) +
  geom_histogram(binwidth = 0.05) + 
  scale_x_log10() +
  ggtitle('Histogram of Contributions')

sum(OR$contb_receipt_amt >= 2800)
sum(OR$contb_receipt_amt < 0)

ggplot(aes(x = cand_nm), data = OR) + 
  geom_bar() +
  scale_y_log10()

tail(sort(table(OR$cand_nm)), 3)

democrat <- c("Bennet, Michael F.", "Biden, Joseph R Jr", "Bloomberg, Michael R.",
              "Booker, Cory A.", "Bullock, Steve", "Buttigieg, Pete",
              "Castro, JuliÃ¡n", "Delaney, John K.", "Gabbard, Tulsi",
              "Gillibrand, Kirsten", "Gravel, Maurice Robert", "Harris, Kamala D.",
              "Hickenlooper, John W.", "Inslee, Jay R", "Klobuchar, Amy J.",
              "Moulton, Seth", "Oâ€™Rourke, Robert Beto", "Patrick, Deval",
              "Sanders, Bernard", "Sestak, Joseph A. Jr.", "Steyer, Tom",
              "Swalwell, Eric Michael", "Warren, Elizabeth", "Williamson, Marianne",
              "Yang, Andrew")

OR$party <- ifelse(OR$cand_nm %in% democrat, "democrat", "republican")
OR$party[OR$cand_nm %in% c("Charles, Mark R.", "Hawkins, Howie", "Jorgensen, Jo",
                           "Pierce, Brock", "Ryan, Timothy J." , "Walsh, Joe",
                           "West, Kanye")] <- "others"


OR$contbr_first_nm <- sub(" .*", "", sub(".*, ", "", OR$contbr_nm))
OR <- OR[OR$contb_receipt_amt > 0 & OR$contb_receipt_amt <= 2800, ]

OR$contb_receipt_dt <- as.Date(OR$contb_receipt_dt,format = "%d-%b-%y")

?gender

gender_df <- gender(OR$contbr_first_nm, method = 'ssa', c(1920, 1997),
                    countries = 'United States')

gender_df <- unique(gender_df)
names(gender_df)[1] <- 'contbr_first_nm'
OR <- inner_join(OR, gender_df, by = 'contbr_first_nm')

summary(OR$gender)

drops <- c('proportion_male', 'proportion_female', 'year_min', 'year_max')
OR <- OR[ , !(names(OR) %in% drops)]

party_group <- group_by(OR, party)

OR.contr_by_party <- summarize(party_group,
                               sum_party = sum(contb_receipt_amt),
                               number_of_candidate = length(unique(cand_id)), 
                               mean_party = sum_party/number_of_candidate, 
                               n = n())

OR.contr_by_party
OR.contr_by_party$party <- ordered(OR.contr_by_party$party, 
                                   levels = c('democrat', 'republican', 'others'))

ggplot(aes(x = party, y = n, fill = party), data = OR.contr_by_party) +
  geom_bar(stat = 'identity') +
  geom_text(stat = 'identity', aes(label = n),
            data = OR.contr_by_party, vjust = -0.4) +
  xlab('Party') +
  ylab('Number of Contributions') +
  ggtitle('Total Number of Contributions by Party') +
  scale_fill_manual(values = c('blue', 'red', 'gold'))
  
sum(OR.contr_by_party$n)

table(OR$cand_nm)


ggplot(aes(x = cand_nm), data = OR) + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('candidate') +
  ylab('Number of Contributions') +
  ggtitle('Number of Contributions by Candidate')


gender_group <- group_by(OR, gender)
OR.contr_by_gen <- summarize(gender_group, 
                             sum_gen = sum(contb_receipt_amt),
                             n_gen = n())

OR.contr_by_gen

ggplot(aes(x = gender, y = n_gen, fill = gender), 
       data = OR.contr_by_gen, vjust = -0.4) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = n_gen), stat = 'identity', data = OR.contr_by_gen, vjust = -0.4) +
  xlab('Gender') +
  ylab('Number of Contributions') +
  ggtitle('Number of Contributions by Gender') +
  scale_fill_manual(values = c("pink", 'cyan'))


occupation_group <- group_by(OR, contbr_occupation)
OR.contr_by_occu <- summarize(occupation_group, 
                              sum_occu = sum(contb_receipt_amt), 
                              mean_occu = mean(contb_receipt_amt), 
                              n = n())


OR.contr_by_occu <- subset(OR.contr_by_occu, contbr_occupation != "INFORMATION REQUESTED")
OR.contr_by_occu <- head(arrange(OR.contr_by_occu,desc(n)), n = 10)
OR.contr_by_occu$contbr_occupation <- ordered(OR.contr_by_occu$contbr_occupation, 
                                              levels = c('NOT EMPLOYED', 'RETIRED', 'PHYSICIAN', 'TEACHER', 'ATTORNEY',
                                                         'ENGINEER', 'SOFTWARE ENGINEER', 'CONSULTANT'))
                                              
                                              
OR.contr_by_occu


ggplot(aes(x = contbr_occupation, y = n), data = OR.contr_by_occu) +
  geom_bar(stat = 'identity') +
  xlab('Top 10 Occupations') +
  ylab('Number of Donors') +
  ggtitle('Top 10 Occupations of Donors') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(OR$contb_receipt_dt)

ggplot(aes(x = contb_receipt_dt), data = OR) + geom_histogram(binwidth = 30, position = position_dodge()) +
  xlab('Date') +
  ylab('Number of Contributions') +
  ggtitle('Histogram of Contribution Date')

summary(OR$contb_receipt_amt)

OR.contr_by_party

ggplot(aes(x = party, y = sum_party/1000, fill = party), data = OR.contr_by_party) +
  geom_bar(stat = 'identity') +
  geom_text(stat = 'identity', aes(label = round(sum_party/1000)),
            data = OR.contr_by_party, vjust = -0.4) +
  xlab('Party') +
  ylab('Contribution Received') +
  ggtitle('Total Contribution Amount by Party') +
  scale_fill_manual(values = c('blue', 'red', 'gold'))

ggplot(aes(x = party, y = mean_party/1000, fill = party), data = OR.contr_by_party) +
  geom_bar(stat = 'identity') +
  geom_text(stat = 'identity', aes(label = round(mean_party/1000)),
            data = OR.contr_by_party, vjust = -0.4) +
  xlab('Party') +
  ylab('Contribution Received') +
  ggtitle('Average Contribution Received by Party') +
  scale_fill_manual(values = c('blue', 'red', 'gold'))

sort(by(OR$contb_receipt_amt, OR$cand_nm, sum))

ggplot(aes(x = cand_nm, y = contb_receipt_amt/1000), data = OR) + 
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('candidate') +
  ylab('Contribution Amount in thousands') +
  ggtitle('Contribution Amount by Candidate')

sum(OR$contb_receipt_amt)

ggplot(aes(x = party, y = contb_receipt_amt, fill = party), data = OR) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 2800)) +
  xlab('party') +
  ylab('Contribution Amount') +
  ggtitle('Boxplot for Contribution Amount by Party') +
  scale_fill_manual(values = c('blue', 'red', 'gold'))


OR <- subset(OR, OR$cand_nm != "Charles, Mark R." &
               OR$cand_nm != "Hawkins, Howie" &
               OR$cand_nm != "Jorgensen, Jo" &
               OR$cand_nm != "Pierce, Brock" &
               OR$cand_nm != "Ryan, Timothy J." &
               OR$cand_nm != "Walsh, Joe" & 
               OR$cand_nm != "West, Kanye")

by(OR$contb_receipt_amt, OR$party, summary)

ggplot(aes(x = party, y = contb_receipt_amt, fill = party), data = OR) +
  geom_boxplot() +
  scale_y_log10() +
  xlab('party') +
  ylab('Contribution Amount') +
  ggtitle('Boxplot for Contribution Amount(Log_10) by Party') +
  scale_fill_manual(values = c('blue', 'red'))

by(OR$contb_receipt_amt, OR$cand_nm, summary)

ggplot(aes(x = cand_nm, y = contb_receipt_amt), data = OR) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('candidate') +
  ylab('Contribution Amount') +
  ggtitle('Contribution Amount by Candidate')

can_group <- group_by(OR, party, cand_nm)

OR.contr_by_can <- summarize(can_group, 
                             sum_can = sum(contb_receipt_amt), 
                             mean_can = mean(contb_receipt_amt),
                             n = n())
OR.contr_by_can <- arrange(OR.contr_by_can, sum_can)

OR.contr_by_can

ggplot(aes(x = cand_nm, y = sum_can/1000), data = OR.contr_by_can) +
  geom_bar(aes(fill = party), stat = 'identity') +
  scale_y_continuous(limits = c(0, 23000)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Candidate') +
  ylab('Contribution Received (Thousands)') +
  ggtitle('Contribution Received by Candidate') +
  scale_fill_manual(values = c("blue", "red"))

# Create candidate_party dataframe
can_party <- left_join(OR.contr_by_can, OR.contr_by_party, by = 'party')

ggplot(aes(x = cand_nm, y = sum_can/sum_party*100), data = can_party) +
  geom_bar(aes(fill = party), stat = 'identity') +
  geom_text(stat='identity', aes(label = paste(round(100*sum_can/sum_party,0),'%')), 
            size=3, data = can_party, vjust = -0.4)+
  scale_y_continuous(limits = c(0, 100)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Candidate') +
  ylab('Percentage of Donation') +
  ggtitle('Percentage of Contribution Received by Candidate from their Own Party') +
  scale_fill_manual(values = c("blue", 'red'))

top_candidate <- c("Biden, Joseph R Jr", "Sanders, Bernard", "Trump, Donald J.")

top_candidate

ggplot(aes(x = gender, y = contb_receipt_amt, fill = gender), data = OR) +
  geom_boxplot() +
  xlab('gender') +
  ylab('Contribution Amount') +
  ggtitle('Contribution Amount by Gender Boxplot') +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = c("pink", 'cyan'))


by(OR$contb_receipt_amt, OR$gender, summary)

# Create gender dataframe
gender_group <- group_by(OR, gender)

OR.contr_by_gen <- summarize(gender_group, 
                             sum_gen = sum(contb_receipt_amt),
                             n = n())

OR.contr_by_gen

ggplot(aes(x = gender, y = sum_gen/1000, fill = gender), 
       data = OR.contr_by_gen) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = sum_gen/1000), stat = 'identity', data = OR.contr_by_gen, vjust = -0.4) +
  xlab('Gender') +
  ylab('Contribution Amount (Thousands)') +
  ggtitle('Contribution Amount by Gender') +
  scale_fill_manual(values = c("pink", 'cyan'))


# Create gender_to_top_candidate dataframe for bar plot
OR.gen_to_top_candidate <- OR %>%
  filter(OR$cand_nm %in% top_candidate) %>%
  group_by(cand_nm, gender) %>%
  summarize(sum_gen_can = sum(contb_receipt_amt))

OR.gen_to_top_candidate

ggplot(aes(x = cand_nm, y = sum_gen_can/1000, fill = gender), 
       data = OR.gen_to_top_candidate) +
  geom_bar(stat = 'identity', position = position_dodge(width = 1)) +
  xlab('Candidate') +
  ylab('Contribution Amount (Thousands)') +
  ggtitle('Contribution Amount to Top Candidate by Gender') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("pink", 'cyan'))

OR.contr_by_occu


ggplot(aes(x = contbr_occupation, y = sum_occu/1000, fill = contbr_occupation), data = OR.contr_by_occu) +
  geom_bar(stat = 'identity') +
  geom_text(stat = 'identity', aes(label = round(sum_occu/1000)), data = OR.contr_by_occu, vjust = -0.4) +
  xlab('Top 10 Occupations') +
  ylab('Total Contribution Amount (Thousands)') +
  ggtitle('Total Contribution Amount From Top 10 Occupations') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(aes(x = contbr_occupation, y = round(mean_occu,2)), data = OR.contr_by_occu) + 
  geom_bar(stat = 'identity') +
  geom_text(stat = 'identity', aes(label = round(mean_occu,2)), data = OR.contr_by_occu, vjust = -0.4) +
  xlab('Top 10 Occupations') +
  ylab('Average Contribution Amount') +
  ggtitle('Average Contributions From the Top 10 Occupations') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create top_occupation dataframe 
top_occu_df <- filter(OR, contbr_occupation %in% OR.contr_by_occu[['contbr_occupation']])

ggplot(aes(x = contbr_occupation, y = contb_receipt_amt), data = top_occu_df) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('Top 10 Occupations') +
  ylab('Donations Amount') +
  ggtitle('Donations Made by Top 10 Occupations')

by(top_occu_df$contb_receipt_amt, top_occu_df$contbr_occupation, summary)

ggplot(aes(x = contbr_occupation, y = contb_receipt_amt), data = top_occu_df) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 300)) +
  xlab('Top 10 Occu') +
  ylab('Donations Amount') +
  ggtitle('Donations Made by Top 10 Occu. Excluding high donations')


OR.top_candidate <- OR %>%
  filter(cand_nm %in% top_candidate) %>%
  group_by(cand_nm, contb_receipt_dt) %>%
  summarize(n = n(), total = sum(contb_receipt_amt))

ggplot(aes(x = contb_receipt_dt, y = n, color = cand_nm), data = OR.top_candidate) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = 'loess') +
  xlab('Date') +
  ylab('Number of Contributions') +
  scale_color_manual(values = c("blue", 'gold', 'red')) +
  ggtitle('Time Series of Number of Contributions by Top Candidate')


ggplot(aes(x = contb_receipt_dt, y = total/1000, color = cand_nm), data = OR.top_candidate) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = 'loess') +
  xlab('Date') +
  ylab('Contribution Amount (Thousands)') +
  scale_color_manual(values = c("blue", 'gold', 'red')) +
  ggtitle('Time Series of Contribution Amount by Top Candidate')

ggplot(aes(x = contb_receipt_dt, y = total, color = cand_nm), data = OR.top_candidate) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = 'loess') +
  xlab('Date') +
  ylab('Contribution Amount') +
  ggtitle('Time Series of Contribution Amount(Log_10) by Candidate') +
  facet_wrap(~ cand_nm) +
  scale_y_log10(limits = c(NA, 10000), breaks = waiver()) +
  scale_color_manual(values = c("blue", 'gold', 'red')) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

ggplot(aes(x = contb_receipt_dt, y = n, color = cand_nm), data = OR.top_candidate) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = 'loess') +
  xlab('Date') +
  ylab('Contribution Amount') +
  ggtitle('Time Series of Contributions (Log_10) by Candidate') +
  facet_wrap(~ cand_nm) +
  scale_y_log10(limits = c(NA, 10000), breaks = waiver()) +
  scale_color_manual(values = c("blue", 'gold', 'red')) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

ggplot(aes(x = contb_receipt_dt, y = n, color = cand_nm), 
       data = OR.top_candidate) +
  geom_jitter(alpha = 0.05) +
  geom_smooth(method = 'loess') +
  scale_y_log10(limits = c(NA, 10000), breaks = waiver()) +
  scale_color_manual(values = c("blue", 'gold', 'red')) +
  xlab('Date') +
  ylab('Number of Contributions(Log_10)') +
  ggtitle('Time Series of Number of Contributions by Candidate') +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))

