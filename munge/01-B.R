# The "student_identifying_info" table pulls together all identifying information 
# for students in one place. This datframe is one row per student. 

# STUDENT IDENTIFYING INFO ------------------------------------------------
student_identifying_info <-
  student_daily_attendance %>%
  select(student_number, 
         lastfirst,
         dob,
         grade_level,
         gender,
         street,
         city,
         state,
         zip, 
         exitcode, 
         exitcomment) %>%
  distinct() %>%
  arrange(dob)