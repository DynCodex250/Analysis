duty_req <- run_readonly_query(cnn, "sql/07_licensing_duty_requirements.sql", params = list(EntitledOnly = TRUE))
head(duty_req)
priv_req <- run_readonly_query(cnn, "sql/08_licensing_privilege_requirements.sql", params = list(EntitledOnly = TRUE))
head(priv_req)
