#Run PRIME for a specific country (eg Rwanda) and load this into a data frame called result:
result <- RunCountry ("RWA")

#Print out all the rows for some specific columns:
Some_data = result [,c("vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey","disability", "cost.cecx")]
print(Some_data)

#These specific columns give RATES (rather than actual values). To convert to actual population values,
#multiply each number by the cohort size. Replace the current Some_data data frame with these new values:
#result [,c("vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey","disability", "cost.cecx")] <- result[, cohort_size] * result[,c("vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey", "disability", "cost.cecx")]
result [,c("vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey","disability", "cost.cecx")] <- result[, cohort_size] * result[,c("vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey", "disability", "cost.cecx")]

New_some_data = result [,c("vaccinated", "immunized", "inc.cecx", "mort.cecx", "lifey","disability", "cost.cecx")]
print(New_some_data)

#Lastly, look at all data for people aged 16+.
Adult_data=result [age>15]
#print(Adult_data)
View(Adult_data)