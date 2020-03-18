# Initial Data munging Script

# PARAMETERS --------------------------------------------------------------
SY_1920 <- silounloadr::calc_academic_year(ymd("2019-08-19"), format = "firstyear")

CURRENT_FIRST_YEAR <- calc_academic_year(lubridate::today(),
                                         format = "first_year")

PS_TERMID_1920 <- calc_ps_termid(CURRENT_FIRST_YEAR)

PS_YEARID_1920 <-
  silounloadr::calc_ps_termid(SY_1920) %>%
  str_extract("\\d{2}") %>%
  as.integer()

FIRST_DAY_SCHOOL_1920 <- terms %>%
  filter(id == PS_TERMID_1920) %>% 
  select(firstday)

LAST_DAY_SCHOOL_1920 <- terms %>%
  filter(id == PS_TERMID_1920) %>% 
  select(lastday)


# STUDENT DAILY ATTENDANCE DF ---------------------------------------------
student_daily_attendance <- 
  
  # this table incldes only days where students where not first marked as present
  attendance %>%
  
  # This table adds the attendance codes (tardy, absent, suspension, etc.)
  left_join(attendance_code,
            by = c("attendance_codeid" = "id")) %>%
  
  # this table ensures that each student has a row for each day they were enrolled in school. 
  # because attendance table only records "special circumstances", membership table is needed
  # to figure out all days students were present. 
  right_join(membership, 
             by = c("studentid" = "studentid", 
                    "att_date" = "date")) %>%
  
  # Identify whether att_code is enrolled, present, absent, or tardy for each student by day
  mutate(enrolled0 = 1,
         enrolled = if_else(att_code == "D" & !is.na(att_code), 0, enrolled0),
         present0 = ifelse(is.na(att_code) | att_code == "", 1, 0),
         present1 = ifelse(att_code %in%  c("A", "S"), 0, present0),
         present2 = ifelse(att_code == "H", 0.5, present1),
         present3 = ifelse(att_code %in% c("T", "E", "L", "I"), 1, present2),
         present = ifelse(is.na(present2), 1, present3),
         absent = (1 - present)*enrolled,
         tardy = ifelse(att_code %in% "T", 1, 0)) %>%
  
  # Add additional student info
  left_join(students,
            by = c("studentid" = "studentid")
  ) %>%
  arrange(yearid, 
          student_number, 
          att_date) %>%
  
  # filter out students who were DNA (students who signed up but never came to school)
  filter(exitcode != "99") %>%
  
  # create human readable school year abbrevations
  mutate(sy = case_when(yearid == "13" ~ "03-04", 
                        yearid == "14" ~ "04-05", 
                        yearid == "15" ~ "06-06", 
                        yearid == "16" ~ "06-07",
                        yearid == "17" ~ "07-08", 
                        yearid == "18" ~ "08-09",
                        yearid == "19" ~ "09-10", 
                        yearid == "20" ~ "10-11",
                        yearid == "21" ~ "11-12", 
                        yearid == "22" ~ "12-13",
                        yearid == "23" ~ "13-14", 
                        yearid == "24" ~ "14-15",
                        yearid == "25" ~ "15-16",
                        yearid == "26" ~ "16-17", 
                        yearid == "27" ~ "17-18",
                        yearid == "28" ~ "18-19", 
                        yearid == "29" ~ "19-20")
  ) %>%
  mutate(yearid = as.character(yearid), 
         student_number = as.character(student_number), 
         studentid = as.character(studentid), 
         schoolid = as.character(schoolid), 
         grade_level = as.factor(grade_level), 
         att_date = ymd(att_date)
  ) %>%
  left_join(school_id_abbr, 
            by = "schoolid") %>%
  select(student_number,
         studentid,
         lastfirst,
         dob,
         gender,              
         grade_level,
         schoolid,
         abbr,
         att_date,
         yearid,
         sy,
         street,
         city,
         state,             
         zip,
         schoolentrydate,
         schoolentrygradelevel,
         enrolled,
         present,
         absent,
         tardy, 
         exitcode, 
         exitcomment
  ) %>%
  
  # Filter out students with missing student numbers, dob, and location info.
  # This will cause us to lose 121 students from our dataset. The majority of these students transferred out
  # quickly or graduated around a decade ago. 
  filter(!is.na(student_number | !is.na(dob) | !is.na(street) | !is.na(city) | !is.na(state))
  ) %>%
  
  # Change empty strings to NA to more easily identify them
  mutate(
    street = na_if(street, ""),
    gender = na_if(gender, ""),
    city = na_if(city, ""),
    state = na_if(state, ""),
    zip = na_if(zip, "")
  )