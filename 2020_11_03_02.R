# 2020.11.03 (?™”)

# ?•œêµ?ë³µì?€?Œ¨?„?°?´?„°
# ?€? ?•œêµ?ë³´ê±´?‚¬?šŒ?—°êµ¬ì› ë°œê°„
# ?€? ê°€êµ¬ì˜ ê²½ì œ?™œ?™?„ ?—°êµ¬í•´ ? •ì±? ì§€?›?— ë°˜ì˜?•  ëª©ì 
# ?€? 2006~2015?…„ê¹Œì?€ ? „êµ??—?„œ 7000?—¬ ê°€êµ¬ë?? ?„ ? •?•´ ë§¤ë…„ ì¶”ì  ì¡°ì‚¬
# ?€? ê²½ì œ?™œ?™, ?ƒ?™œ?‹¤?ƒœ, ë³µì?€?š•êµ? ?“± ?ˆ˜ì²? ê°? ë³€?ˆ˜?— ??€?•œ ? •ë³´ë¡œ êµ¬ì„±

# ?Œ¨?‚¤ì§€ ì¤€ë¹„í•˜ê¸?
install.packages("foreign")    # foreign ?Œ¨?‚¤ì§€ ?„¤ì¹?

library(foreign)               # SPSS ?ŒŒ?¼ ë¡œë“œ
library(dplyr)                 # ? „ì²˜ë¦¬
library(ggplot2)               # ?‹œê°í™”
library(readxl)                # ?—‘??€ ?ŒŒ?¼ ë¶ˆëŸ¬?˜¤ê¸?

# ?°?´?„° ë¶ˆëŸ¬?˜¤ê¸?
raw_welfare <- read.spss(file = "data/Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)

# ë³µì‚¬ë³? ?°?´?„°?”„? ˆ?„ ?ƒ?„±
welfare <- raw_welfare
welfare

# ?°?´?„° ?ƒ?ƒ‰?•˜ê¸?
head(welfare)            # ?•?—?„œ 6ê°? ?°?´?„° ì¶”ì¶œ 
tail(welfare)            # ??—?„œ 6ê°? ?°?´?„° ì¶”ì¶œ
View(welfare)            # ë·°ì–´ì°½ìœ¼ë¡? ?°?´?„° ì¶œë ¥
dim(welfare)             # ?°?´?„° ì°¨ì› : 16664?–‰ 957?—´
str(welfare)             # ê°? ë³€?ˆ˜?˜ ?°?´?„°?˜• ì¶œë ¥
summary(welfare)         # ?†µê³„ì ?¸ ?š”?•½? •ë³? ì¶œë ¥


# ë³€?ˆ˜ëª? ë°”ê¾¸ê¸?
welfare <- rename(welfare,
                  sex = h10_g3,              #?„±ë³?
                  birth = h10_g4,            # ?ƒœ?–´?‚œ ?—°?„
                  marriage = h10_g10,        # ?˜¼?¸ ?ƒ?ƒœ
                  religion = h10_g11,        # ì¢…êµ
                  income = p1002_8aq1,       # ?›”ê¸?
                  code_job = h10_eco9,       # ì§ì¢… ì½”ë“œ
                  code_region = h10_reg7)    # ì§€?—­ ì½”ë“œ

View(welfare)

#----------------------------------------------------------------------
# 1. ?„±ë³„ì— ?”°ë¥? ?›”ê¸? ì°¨ì´
#    : ?„±ë³„ì— ?”°?¼ ?›”ê¸‰ì´ ?–¼ë§ˆë‚˜ ?‹¤ë¥¼ê¹Œ?

# 1) ?„±ë³?(sex) ê²€?† 
class(welfare$sex)          # sexë³€?ˆ˜?˜ ?°?´?„°?˜• 
# "numeric"                 # ?ˆ«? ?°?´?„°

table(welfare$sex)          # sexë³€?ˆ˜?˜ ë¹ˆë„?ˆ˜  
#  1     2                  # 1:?‚¨?, 2:?—¬?, 9:ëª¨ë¦„/ë¬´ì‘?‹µ
# 7578  9086 

# 2) ? „ì²˜ë¦¬
# ?´?ƒì¹? ?°?´?„° ?™•?¸
table(welfare$sex)         
# 1    2                    # ?´?ƒì¹? ?°?´?„°(9)ê°€ ?—†?Œ?Œ          
# 7578 9086

# ?´?ƒì¹? ?°?´?„°ë¥? ê²°ì¸¡ ì²˜ë¦¬
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

# ê²°ì¸¡ì§€ ?™•?¸
table(is.na(welfare$sex))
# FALSE 
# 16664 

# ?„±ë³?(1, 2) ?•­ëª©ì— ?´ë¦? ë¶€?—¬ : (1:male, 2:female)
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
# female   male 
# 9086     7578

# ?„±ë³? ë¹ˆë„ ë§‰ë?€ ê·¸ë˜?”„
qplot(welfare$sex)


# 3) ?›”ê¸?(income) ë³€?ˆ˜ ê²€?† 
class(welfare$income)     
# "numeric"                      # ?ˆ«? ?°?´?„°

# ?›”ê¸?(income) ë³€?ˆ˜?˜ ë¹ˆë„ êµ¬í•˜ê¸?
table(welfare$income)

# ?›”ê¸?(income) ë³€?ˆ˜?˜ ?†µê³? ?š”?•½? •ë³? êµ¬í•˜ê¸?
summary(welfare$income)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    NA's 
# 0.0   122.0   192.5     241.6   316.6  2400.0   12030 

# ?›”ê¸?(income) ë¹ˆë„ ë§‰ë?€ ê·¸ë˜?”„
qplot(welfare$income)

# xì¶•ì˜ ë²”ìœ„ ?„¤? • : 0 ~ 1000
qplot(welfare$income) + xlim(0, 1000)

# 4) ? „ì²˜ë¦¬
#    ?´?ƒì¹? ?°?´?„°ë¥? ê²°ì¸¡?œ¼ë¡? ì²˜ë¦¬
#    income ?˜ ë²”ìœ„ : 1 ~ 9998
#    income ?˜ ?´?ƒì¹? : 9999(ëª¨ë¦„/ë¬´ì‘?‹µ)
welfare$income <- ifelse(welfare$income==9999, NA, welfare$income)


# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(welfare$income))
# FALSE  TRUE 
# 4634   12030 

# 5) ?„±ë³„ì— ?”°ë¥? ?›”ê¸? ë¶„ì„?•˜ê¸?
sex_income <- welfare %>% 
              filter(!is.na(income)) %>%   # ê²°ì¸¡ì¹? ? œê±°í•˜ê¸? 
              group_by(sex) %>% 
              summarise(mean_income = mean(income))
sex_income
#    sex       mean_income
#    <chr>        <dbl>
# 1  female        162.
# 2  male          312.
 
# 6) ë§‰ë?€ ê·¸ë˜?”„ë¡? ?‹œê°í™”
ggplot(data=sex_income, aes(x=sex, y=mean_income)) + 
  geom_col()

#---------------------------------------------------------------
# 2.?‚˜?´??€ ?›”ê¸‰ê³¼?˜ ê´€ê³?
#   ëª‡ì‚´?•Œ ?›”ê¸‰ì„ ê°€?¥ ë§ì´ ë°›ì„ê¹??

# 1) birth ë³€?ˆ˜ ê²€?† ?•˜ê¸?
# birth ë³€?ˆ˜ : ?ƒœ?–´?‚œ ?—°?„ê°€ ??€?¥?œ ë³€?ˆ˜
class(welfare$birth)
# "numeric"                  # ?ˆ«? ?°?´?„°

# birth ë³€?ˆ˜?˜ ?†µê³? ?š”?•½ ? •ë³? êµ¬í•˜ê¸?
summary(welfare$birth)
# Min.  1st Qu.  Median   Mean   3rd Qu.  Max. 
# 1907    1946    1966    1968    1988    2014 

# birth ë³€?ˆ˜?˜ ë¹ˆë„ ë§‰ë?€ ê·¸ë˜?”„
qplot(welfare$birth)

# 2) ? „ì²˜ë¦¬ 
# ?´?ƒì¹? ?°?´?„° ?™•?¸
# birth ë³€?ˆ˜?˜ ? •?ƒ ë²”ìœ„ : 1900 ~ 2014
# birth ë³€?ˆ˜?˜ ?´?ƒì¹? : 9999 (ëª¨ë¦„/ë¬´ì‘?‹µ) 
summary(welfare$birth)
# Min.  1st Qu.  Median   Mean   3rd Qu.  Max. 
# 1907    1946    1966    1968    1988    2014 

# ?´?ƒì¹?(9999) ?°?´?„°ë¥? ê²°ì¸¡?œ¼ë¡? ì²˜ë¦¬
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth)

# ê²°ì¸¡ì¹? ?™•?¸
table(is.na(welfare$birth))
# FALSE 
# 16664 

# 3) ?ŒŒ?ƒë³€?ˆ˜ ?ƒ?„± : ?‚˜?´(age)
welfare$age <- 2015 - welfare$birth + 1

# ?‚˜?´(age) ë¹ˆë„ ê·¸ë˜?”„
qplot(welfare$age)

# 4) ?‚˜?´??€ ?›”ê¸? ê´€ê³? ë¶„ì„?•˜ê¸?
#    ?‚˜?´?— ?”°ë¥? ?‰ê·? ?›”ê¸? êµ¬í•˜ê¸?
age_income <- welfare %>% 
              filter(!is.na(income)) %>%   # ê²°ì¸¡ì¹? ? œê±°í•˜ê¸?
              group_by(age) %>%            # ?‚˜?´(age)ë³„ë¡œ ì²˜ë¦¬
              summarise(mean_income = mean(income))

head(age_income, 10)       # ?•?—?„œ ë¶€?„° 10ê°? ?°?´?„° êµ¬í•˜ê¸?

# 5) ?„  ê·¸ë˜?”„ë¡? ?‹œê°í™”
ggplot(data=age_income, aes(x=age, y=mean_income)) + geom_line()

#----------------------------------------------------------------
# 3. ?—°? ¹??€?— ?”°ë¥? ?›”ê¸? ì°¨ì´
#    ?–´?–¤ ?—°? ¹??€?˜ ?›”ê¸‰ì´ ê°€?¥ ë§ì„ê¹??
# 0 ~ 29  : young
# 30 ~ 59 : middle
# 60?´?ƒ  : old

# 1) ?ŒŒ?ƒ ë³€?ˆ˜ ?ƒ?„± : ?—°? ¹??€(ageg)
welfare <- welfare %>% 
           mutate(ageg = ifelse(age<30, "young",
                                ifelse(age<=59, "middle", "old")))
# ê°? ?—°? ¹??€ë³? ë¹ˆë„?ˆ˜
table(welfare$ageg)
# middle    old    young 
# 6049      6281   4334 

# ?—°? ¹??€(ageg)ë³? ë¹ˆë„ ê·¸ë˜?”„
qplot(welfare$ageg)

# 2) ?—°? ¹??€ë³? ?‰ê· ì›”ê¸? êµ¬í•˜ê¸?
ageg_income <- welfare %>% 
               filter(!is.na(income)) %>%   # ê²°ì¸¡ì¹? ? œê±?
               group_by(ageg) %>% 
               summarise(mean_income = mean(income))

# ?—°? ¹??€ë³? ?‰ê·? ?›”ê¸?
ageg_income
#    ageg         mean_income
#    <chr>         <dbl>
# 1  middle        281.
# 2  old           125.
# 3  young         164.

# 3) ë§‰ë?€ê·¸ë˜?”„ë¡? ?‹œê°í™”
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + geom_col()


# xì¶•ì˜ ì¶œë ¥ ?ˆœ?„œë¥? young - middle - old ?ˆœ?œ¼ë¡? ì¶œë ¥
ggplot(data=ageg_income, aes(x=ageg, y=mean_income)) + 
  geom_col() +
  scale_x_discrete(limits=c("young","middle","old"))

#-------------------------------------------------------------------
# 4. ?‚˜?´ ë°? ?„±ë³? ?›”ê¸? ì°¨ì´ ë¶„ì„

# 1) ?—°? ¹ë³? ?„±ë³? ?›”ê¸? ë¶„ì„?•˜ê¸?
sex_age <- welfare %>% 
           filter(!is.na(income)) %>%       # ê²°ì¸¡ì¹? ? œê±?
           group_by(age, sex) %>% 
           summarise(mean_income = mean(income)) 
sex_age

# 2) ?„ ê·¸ë˜?”„ë¡? ?‹œê°í™”
ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + 
  geom_line()

#---------------------------------------------------------------

# 5. ì§ì—…ë³? ?›”ê¸? ì°¨ì´
#    ?–´?–¤ ì§ì—…?´ ê°€?¥ ë§ì?€ ?›”ê¸‰ì„ ë°›ì„ê¹??

# 1) ë³€?ˆ˜(code_job) ê²€?† ?•˜ê¸?
class(welfare$code_job)
# "numeric"                  # ?ˆ«? ?°?´?„°

# ë¹ˆë„?ˆ˜ êµ¬í•˜ê¸?
table(welfare$code_job)

# 2) ? „ì²˜ë¦¬
# ì§ì¢…ì½”ë“œ(code_job)??€ ì§ì—…(job) ? •ë³´ë?? ê°€ì§€ê³? ?ˆ??€ ?—‘??€?ŒŒ?¼
# ë¶ˆëŸ¬?˜¤ê¸?
library(readxl)
list_job <- read_excel("data/Koweps_Codebook.xlsx",
                       col_names = T,
                       sheet = 2)

head(list_job)
dim(list_job)           # 149?–‰ 2?—´
View(list_job)

# welfare??€ list_job ?°?´?„°?”„? ˆ?„?„ ê°€ë¡œë°©?–¥?œ¼ë¡? ?•©ì¹˜ê¸°
welfare <- left_join(welfare, list_job, by="code_job")

# ê°€ë¡œë°©?–¥?œ¼ë¡? ?•©ì³ì§„ ?°?´?„°?”„? ˆ?„?„ ì¶œë ¥
welfare %>% filter(!is.na(code_job)) %>%     # ê²°ì¸¡ì¹? ? œê±?
            select(code_job, job) %>% 
            head(10)

# 3)ì§ì—…ë³? ?‰ê· ì›”ê¸? êµ¬í•˜ê¸?
job_income <- welfare %>% 
              filter(!is.na(job) & !is.na(income)) %>%  # ê²°ì¸¡ì¹? ? œê±?
              group_by(job) %>% 
              summarise(mean_income = mean(income))
head(job_income)

# 4) ?›”ê¸‰ì„ ë§ì´ ë°›ëŠ” ?ƒ?œ„ 10ê°œì˜ ì§ì—…?„ êµ¬í•´ë³´ì : ?‚´ë¦¼ì°¨?ˆœ ? •? ¬
top10 <- job_income %>% 
         arrange(desc(mean_income)) %>%   # ?‰ê· ì›”ê¸‰ì„ ?‚´ë¦¼ì°¨?ˆœ ? •? ¬
         head(10)
top10

# 5) ë§‰ë?€ ê·¸ë˜?”„ë¡? ?‹œê°í™”
ggplot(data=top10, aes(x=reorder(job, mean_income), y=mean_income)) + 
  geom_col() +        # ë§‰ë?€ ê·¸ë˜?”„
  coord_flip()        # x, yì¶•ì´ ë°”ë€?

#-----------------------------------------------------------------------
# 6. ?„±ë³? ì§ì—… ë¹ˆë„ êµ¬í•˜ê¸?
#    ?„±ë³„ë¡œ ?–´?–¤ ì§ì—…?´ ê°€?¥ ë§ì„ê¹??

# 1) ?‚¨?„± ì§ì—… ë¹ˆë„ ?ƒ?œ„ 10ê°? êµ¬í•˜ê¸?
job_male <- welfare %>% 
            filter(!is.na(job) & sex == "male") %>% 
            group_by(job) %>% 
            summarise(n = n()) %>% # ?‚¨?„± ì§ì—…?˜ ë¹ˆë„ êµ¬í•˜ê¸? 
            arrange(desc(n)) %>%   # ë¹ˆë„ê°€ ?†’??€ ì§ì—…?ˆœ?œ¼ë¡? ?‚´ë¦¼ì°¨?ˆœ ? •? ¬
            head(10)
job_male                        
#   job                         n
#   <chr>                      <int>
# 1 ?‘ë¬¼ì¬ë°? ì¢…ì‚¬?            640
# 2 ??™ì°? ?š´? „?›              251
# 3 ê²½ì˜ê´€? ¨ ?‚¬ë¬´ì›            213
# 4 ?˜?—… ì¢…ì‚¬?                141
# 5 ë§¤ì¥ ?Œë§? ì¢…ì‚¬?           132
# 6 ? œì¡°ê?€? ¨ ?‹¨?ˆœ ì¢…ì‚¬?›       104
# 7 ì²??†Œ?› ë°? ?™˜ê²? ë¯¸í™”?›       97
# 8 ê±´ì„¤ ë°? ê´‘ì—… ?‹¨?ˆœ ì¢…ì‚¬?›    95
# 9 ê²½ë¹„?› ë°? ê²€?‘œ?›            95
# 10 ?–‰? • ?‚¬ë¬´ì›                92

# 2) ë§‰ë?€ ê·¸ë˜?”„ë¡? ?‹œê°í™”
# ?‚¨?„± ì§ì—… ë¹ˆë„ ?ƒ?œ„ 10ê°? ì§ì—…?„ ë§‰ë?€ ê·¸ë˜?”„ë¡? ì¶œë ¥
ggplot(data=job_male, aes(x=reorder(job, n), y=n)) + 
  geom_col() +           # ë§‰ë?€ ê·¸ë˜?”„
  coord_flip()           # x, yì¶•ì´ ë°”ë€?


# 3) ?—¬?„± ì§ì—… ë¹ˆë„ ?ƒ?œ„ 10ê°? êµ¬í•˜ê¸?
job_female <- welfare %>% 
              filter(!is.na(job) & sex=="female") %>% 
              group_by(job) %>% 
              summarise(n = n()) %>% # ?—¬?„±ì§ì—…?˜ ë¹ˆë„ êµ¬í•˜ê¸?
              arrange(desc(n)) %>%   # ë¹ˆë„ê°€ ?†’??€ì§ì—…?ˆœ?œ¼ë¡? ?‚´ë¦¼ì°¨?ˆœ? •? ¬
              head(10)
job_female
#    job                            n
#    <chr>                        <int>
# 1 ?‘ë¬¼ì¬ë°? ì¢…ì‚¬?                680
# 2 ì²??†Œ?› ë°? ?™˜ê²? ë¯¸í™”?›          228
# 3 ë§¤ì¥ ?Œë§? ì¢…ì‚¬?               221
# 4 ? œì¡°ê?€? ¨ ?‹¨?ˆœ ì¢…ì‚¬?›           185
# 5 ?šŒê³? ë°? ê²½ë¦¬ ?‚¬ë¬´ì›            176
# 6 ?Œ?‹?„œë¹„ìŠ¤ ì¢…ì‚¬?              149
# 7 ì£¼ë°©?¥ ë°? ì¡°ë¦¬?‚¬               126
# 8 ê°€?‚¬ ë°? ?œ¡?•„ ?„?š°ë¯?            125
# 9 ?˜ë£? ë³µì?€ ê´€? ¨ ?„œë¹„ìŠ¤ ì¢…ì‚¬?   121
# 10 ?Œ?‹ê´€? ¨ ?‹¨?ˆœ ì¢…ì‚¬?›          104

# 4) ë§‰ë?€ ê·¸ë˜?”„ë¡? ?‹œê°í™”
#    ?—¬?„± ì§ì—… ë¹ˆë„ ?ƒ?œ„ 10ê°? ì§ì—…?„ ë§‰ë?€ ê·¸ë˜?”„ë¡? ?‹œê°í™”
ggplot(data=job_female, aes(x=reorder(job, n), y=n)) + 
  geom_col()  +          # ë§‰ë?€ ê·¸ë˜?”„
  coord_flip()           # x, yì¶•ì´ ë°”ë€?

#---------------------------------------------------------------
# 7. ì¢…êµ ?œ ë¬´ì— ?”°ë¥? ?´?˜¼?œ¨ ë¶„ì„
#    ì¢…êµê°€ ?ˆ?Š” ?‚¬?Œ?“¤?´ ?´?˜¼?„ ?œ ?• ê¹??

# 1) ë³€?ˆ˜ ê²€?† 
# religion : ì¢…êµ?œ ë¬?,  marriage : ê²°í˜¼?—¬ë¶€ë¶€
class(welfare$religion)
# "numeric"                 # ?ˆ«? ?°?´?„°

table(welfare$religion)     # ì¢…êµ ?œ ë¬?
# 1     2                   # 1:ì¢…êµ?ˆ?Œ
# 8047  8617                # 2:ì¢…êµ ?—†?Œ
                            # 9:ëª¨ë¦„/ë¬´ì‘?‹µ 
class(welfare$marriage)
# "numeric"                 # ?ˆ«? ?°?´?„°  

table(welfare$marriage)
#  0    1    2     3    4    5    6 
# 2861 8431 2117  712   84 2433   26 

# 0 : ë¹„í•´?‹¹(18?„¸ ë¯¸ë§Œ)
# 1 : ?œ ë°°ìš°?(ê²°í˜¼)
# 2 : ?‚¬ë³?
# 3 : ?´?˜¼
# 4 : ë³„ê±°
# 5 : ë¯¸í˜¼(18?„¸ ?´?ƒ, ë¯¸í˜¼ëª? ?¬?•¨)
# 6 : ê¸°í?€(?‚¬ë§? ?“±)

# 2)? „ì²˜ë¦¬
# ì¢…êµ ?œ ë¬? ê°? ?ˆ˜? •
# religion ë³€?ˆ˜ :  1 ->  yes, 2 -> noë¡? ?ˆ˜? •
welfare$religion <- ifelse(welfare$religion==1,"yes","no")

table(welfare$religion)
# no  yes 
# 8617 8047


welfare$group_marriage <- ifelse(welfare$marriage==1, "marriage",
                                 ifelse(welfare$marriage==3, "divorce",
                                        NA))
welfare$group_marriage
table(welfare$group_marriage)

table(is.na(welfare$group_marriage))

religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group *100, 1))

religion_marriage

divorce <-religion_marriage %>% 
  filter(group_marriage == 'divorce') %>% 
  select(religion, pct)
  
divorce
ggplot(data = divorce, aes(x=religion, y=pct))+geom_col()

ageg_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group *100, 1))

ageg_marriage

divorce_ageg <-ageg_marriage %>% 
  filter(group_marriage =='divorce') %>% 
  select(ageg, pct)

divorce_ageg

ggplot(data=divorce_ageg, aes(x=ageg, y=pct))+geom_col()

ageg_religion_marriage <- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  group_by(ageg, religion, group_marriage) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group *100, 1))

ageg_religion_marriage

df_divorce <- ageg_religion_marriage %>% 
  filter(group_marriage =='divorce') %>% 
  select(ageg, religion, pct)
df_divorce

ggplot(data=df_divorce) + geom_col(mapping=aes(x=ageg, y=pct, fill=religion))

ggplot(data=df_divorce) + geom_col(mapping=aes(x=ageg, y=pct, fill=religion), position = "dodge")

install.packages("rJava")








