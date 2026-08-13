SELECT S.RECID, S.NAME, S.AOTNAME
FROM SECURITYROLE S
WHERE S.RECID IN (99, 412, 418, 444, 496);   -- die RecIds aus deinem allerersten Screenshot

licensing_req <- load_licensing_requirements(cnn, "sql")
sec_structure <- analyze_security_structure(secgov, licensing_req)
unique(sec_structure$ENTITLED)   # sollte jetzt 0/1-Werte zeigen, nicht mehr NA
