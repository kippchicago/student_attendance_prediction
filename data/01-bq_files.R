# pulls datasets from Big Query Database
terms <- get_powerschool("terms") %>%
  select(id,
         abbreviation,
         firstday,
         lastday) %>%
  collect()  %>%
  filter(grepl("-", abbreviation)) %>%
  group_by(id) %>%
  filter(row_number(desc(lastday)) == 1) %>%
  unique()

attendance <-
  get_powerschool("attendance") %>%
  filter(att_mode_code == "ATT_ModeDaily") %>%
  select(studentid, 
         att_date,
         att_comment, 
         attendance_codeid
  ) %>%
  collect()

attendance_code <- 
  get_powerschool("attendance_code") %>%
  mutate(att_code = if_else(att_code == "true", "T", att_code)) %>% 
  select(att_code,
         description,
         id
  ) %>%
  collect()

membership <- 
  get_powerschool("ps_membership_reg") %>% 
  select(studentid,
         schoolid,
         date = calendardate,
         membership = studentmembership,
         grade_level,
         attendance = ATT_CalcCntPresentAbsent,
         yearid) %>%
  collect()

students <- 
  get_powerschool("students") %>% 
  select(studentid = id, 
         student_number,
         lastfirst,
         dob, 
         gender,
         ethnicity, 
         street,
         city,
         state,
         zip,
         geocode,
         entrydate,
         schoolentrydate,
         schoolentrygradelevel) %>%
  collect()