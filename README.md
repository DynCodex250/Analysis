# Vollständige Spaltenliste beider Quellen
names(licensing_req$role)
str(licensing_req$role, max.level = 1)

names(secgov)
str(secgov, max.level = 1)

# Falls es in licensing_req$role eine Textspalte für den Rollennamen gibt
# (z.B. NAME, SECURITYROLENAME, ROLENAME), sollte sie hier auftauchen:
names(licensing_req$role)[sapply(licensing_req$role, is.character)]

# Falls secgov stattdessen eine numerische Rollen-ID neben ROLENAME führt
# (z.B. ROLERECID, SECURITYROLE, ROLEID):
names(secgov)[sapply(secgov, function(x) is.numeric(x) || inherits(x, "integer64"))]# Analysis
