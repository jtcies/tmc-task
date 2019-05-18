import pandas
from sqlalchemy import create_engine

engine = create_engine('sqlite://', echo=False)
conn = engine.raw_connection()

voteshare = pandas.read_csv("data/voteshare.csv")

candidate = pandas.read_csv("data/candidate.csv")
                
voteshare.to_sql('voteshare', conn, if_exists='replace', index=False)
candidate.to_sql("candidate", conn, if_exists='replace', index=False)

q4 = pandas.read_sql_query('''

SELECT AVG(voteshare.win_probability) as avg_win_prob,
           candidate.incumbent as incumbent

FROM voteshare
INNER JOIN candidate on voteshare.state = candidate.state 
AND voteshare.party = candidate.party
AND voteshare.district = candidate.district

WHERE voteshare.forecastdate = '2018-11-06'

GROUP BY candidate.incumbent

''', conn
 )

print(q4)



q2b = pandas.read_sql_query('''SELECT *
        
        FROM (SELECT 'first date' as type,
                    forecastdate as date
                          FROM voteshare 
                                ORDER BY forecastdate
                                      LIMIT 1) as first_date
                          
        UNION 
                          
        SELECT *
        
        FROM (SELECT 'last date' as type,
                    forecastdate as date
                          FROM voteshare 
                                ORDER BY forecastdate DESC
                                      LIMIT 1) as last_date''', conn)
                   
print(q2b)
