
# STUDENT AVERAGE DAILY ATTENDANCE 2003 - 2018 ----------------------------

# This dataset shows us the end of year average daily attendance for each student. 
# Each row represents one students ada for an entire school year. The number of rows 
# that a student appears in in the dataset corresponds to the number of years the 
# student has been with KIPP Chiago (Note: We have removed the current 19-20 school year).

student_yearly_ada <- 
  student_daily_attendance %>% 
  
  # filter out attendance data for this year. 
  filter(yearid < PS_YEARID_1920) %>%
  group_by(yearid,
           studentid, 
           student_number) %>% 
  summarize(enrolled = sum(enrolled),
            absent = sum(absent),
            year_end_atten_pct = 1 - (absent/enrolled))


# STUDENT CUMULATIVE AVERAGE DAILY ATTENDANCE -----------------------------

# The "student_cum_ada" gives a running total of each students average daily 
# attendance each day for each year. For example, you could filter for yearid == 28 
# (last school year), and check each childs average daily attendance at a specific date.

student_cum_ada <- 
  student_daily_attendance %>%
  
  # only looking at students before the 19-20 school year
  filter(yearid < CURRENT_FIRST_YEAR) %>%
  group_by(yearid,
           studentid, 
           student_number) %>% 
  arrange(student_number, 
          att_date) %>%
  mutate(cum_enrolled = cumsum(enrolled),
         cum_absent = cumsum(absent),
         running_ada = 1 - (cum_absent/cum_enrolled)) %>%
  
  # only keep records for when students were actually enrolled. 
  filter(cum_enrolled > 0) %>%
  select(student_number, 
         studentid, 
         dob, 
         gender, 
         grade_level, 
         schoolid, 
         yearid,
         sy, 
         att_date, 
         present, 
         absent, 
         cum_present = cum_enrolled, 
         cum_absent, 
         running_ada
  )