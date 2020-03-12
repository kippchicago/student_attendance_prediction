# pulls datasets from Big Query Database
sy <- silounloadr::calc_academic_year(ymd("2020-06-07"), format = "firstyear")

ps_sy_termid <-
  silounloadr::calc_ps_termid(sy) %>%
  str_extract("\\d{2}") %>%
  as.integer()

membership <- 
  get_powerschool("ps_membership_reg") %>% 
  select(studentid,
         schoolid,
         date = calendardate,
         enrolled = studentmembership,
         grade_level,
         attendance = ATT_CalcCntPresentAbsent,
         yearid) %>%
  filter(yearid >= 25) %>% 
  collect()

attendance <- 
  get_powerschool("attendance") %>% 
  filter(yearid >= 25,
         att_mode_code == "ATT_ModeDaily") %>% 
  collect()

attendance_code <- 
  get_powerschool("attendance_code") %>%
  mutate(att_code = if_else(att_code == "true", "T", att_code)) %>% 
  select(attendance_codeid = id,
         att_code) %>% 
  collect()

students <- 
  get_powerschool("students") %>% 
  select(studentid = id, 
         student_number,
         gender,
         entrydate,
         schoolentrydate,
         districtentrydate,
         geocode) %>%
  collect()