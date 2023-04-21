#Εργασία Ανάλυσης Δεδομένων SQL και R:
 
#Άσκηση 2

#H παρακάτω άσκηση θα χρησιμοποιεί τις βιβλιοθήκες tidyverse και DBI.

library(tidyverse)
library(DBI)

#1.Συνδεθείτε στην βάση δεδομένων football-database.sqlite

con<-dbConnect(RSQLite::SQLite(), "database.sqlite")
con

#2.Διαβάστε όλους τους πίνακες της βάσης και ελέγξτε τη δομή τους

#Διάβασμα όλων των πινάκων της βάσης:

dbListTables(con)

dbReadTable(con,"Country")
dbReadTable(con,"League")
dbReadTable(con,"Match")
dbReadTable(con,"Player")
dbReadTable(con,"Player_Attributes")
dbReadTable(con,"Team")
dbReadTable(con,"Team_Attributes")
dbReadTable(con,"sqlite_sequence")

#Έλεγχος της δομής τους:

str(dbReadTable(con,"Country"))
str(dbReadTable(con,"League"))
str(dbReadTable(con,"Match"))
str(dbReadTable(con,"Player"))
str(dbReadTable(con,"Player_Attributes"))
str(dbReadTable(con,"Team"))
str(dbReadTable(con,"Team_Attributes"))
str(dbReadTable(con,"sqlite_sequence"))


#Ερώτημα 3: Χρησιμοποιήστε την συνάρτηση tbl() για να δημιουργήσετε pointers για τους 7 πίνακες
#(εξαιρέστε τον πίνακα sqlite_sequence) και ακολούθως ελέγξτε τις διαστάσεις τους.
#Τα ονόματα που θα δώσετε στα pointers να είναι ίδια με τα ονόματα που έχουνε οι πίνακες
#στη βάση αλλά με μικρά γράμματα(lowecase)

country<-tbl(con,"Country")
head(country)

league<-tbl(con,"League")
head(league)

match<-tbl(con,"Match")
head(match)

player<-tbl(con,"Player")
head(player)

player_attributes<-tbl(con,"Player_Attributes")
head(player_attributes)

team<-tbl(con,"Team")
head(team)

team_attributes<-tbl(con,"Team_Attributes")
head(team_attributes)

#Ερώτημα 4: Ξαναγράψτε τα ερωτήματα SQL τη Άσκησης 1 αλλά αυτή τη φορά με τον τρόπο της dplyr.

#Ερώτημα a:

players_with_height_160_180<-player%>%filter(between(height,160,180))%>%select(player_name,height,weight)%>%
  arrange(desc(weight))
players_with_height_160_180

#Ερώτημα b:

match1239485<-match%>%left_join(team,by=c("home_team_api_id"="team_api_id"))%>%filter(match_api_id==1239485)%>%
  select(date,team_long_name,home_team_goal)
match1239485

#Ερώτημα c:

country_of_each_team<-match%>%left_join(team,by=c("home_team_api_id"="team_api_id"))%>%
  left_join(country,by=c("country_id"="id"))%>%distinct(team_long_name,name)
country_of_each_team

#Ερώτημα d:

home.team_long_name<-rename(team,"home_team_long_name"=team_long_name)
home.team_long_name

away.team_long_name<-rename(team,"away_team_long_name"=team_long_name)
away.team_long_name

match1239485_more<-match%>%left_join(home.team_long_name,by=c("home_team_api_id"="team_api_id"))%>%
  left_join(away.team_long_name,by=c("away_team_api_id"="team_api_id"))%>%filter(match_api_id==1239485)%>%
  select(date,home_team_long_name,home_team_goal, away_team_long_name, away_team_goal)
match1239485_more


#Ερώτημα 5a: Βρείτε πόσοι παίκτες είναι αριστεροπόδαροι και πόσοι δεξιοπόδαροι.

#αριστεροπόδαροι:

leftfooters<-player_attributes%>%filter((preferred_foot=="left"))%>%group_by(preferred_foot)%>%summarise(count(id))
leftfooters

#δεξιοπόδαροι:

rightfooters<-player_attributes%>%filter((preferred_foot=="right"))%>%group_by(preferred_foot)%>%summarise(count(id))
rightfooters



#Ερώτημα 5b: Χρησιμοποιήστε τη βιβλιοθήκη ggplot2 και κατασκευάστε ένα απλό ραβδόγραμμα για τη μεταβλητή
#preffered_foot και του πίνακα player_attributes

library(ggplot2)

r<-ggplot(player_attributes,aes(x=preferred_foot))+geom_bar()
r


#Ερώτημα 6a: Με χρήση της ggplot2 κάντε ένα διάγραμμα διασποράς για τις μεταβλητές ύψος (height) και βάρος
#(weight) των παικτών 

sp<-ggplot(player,aes(x=height,y=weight,col=height))+geom_point()
sp

#Eρώτημα 6b: Χρησιμοποιήστε το geom_jitter για να βελτιώσετε το παραπάνω γράφημα και αλλάξτε τη διαφάνεια
#(alpha) έτσι ώστε να αποφύγετε το overplotting

sp2<-ggplot(player,aes(x=height,y=weight,col=height))+geom_jitter(alpha=0.4)
sp2


#Ερώτημα 7: Άν ο ακόλουθος κώδικας υπολογίζει τους βαθμούς που πήρε κάθε ομάδα σε κάθε αγώνα και τους προσθέτει
#στο πίνακα match:

match_points <- match %>%
  mutate(home_team_points = if_else((home_team_goal > away_team_goal), 3,
                                    if_else((home_team_goal == away_team_goal), 1, 0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal), 0,
                                    if_else((home_team_goal == away_team_goal), 1, 3)))
match_points


#Eρώτημα 7a: Χρησιμοποιήστε τον πίνακα match_points που ορίζεται παραπάνω και υπολογίστε τον μέσο όρο
#βαθμών που είχε η ομάδα εντός έδρας ανά παιχνίδι αποθηκεύοντας το σε μια μεταβλητή με το όνομα home_points.
#(Να περιέχονται μόνο οι στήλες league_id,team_api_id και η στήλη με το μέσο όρο βαθμών εντος έδρας)

home_team<-team%>%left_join(match_points,by=c("team_api_id"="home_team_api_id"))%>%
  select(league_id,team_api_id,home_team_points)
home_team

home_points<-home_team%>%group_by(league_id,team_api_id)%>%
  summarise(mean_home_team_points=mean(home_team_points,na.rm = TRUE))
home_points

#Ερώτημα 7b: Χρησιμοποιήστε τον πίνακα match_points που ορίζεται παραπάνω και υπολογίστε τον μέσο όρο
#βαθμών που είχε η ομάδα εκτός έδρας ανά παιχνίδι αποθηκεύοντας το σε μια μεταβλητή με το όνομα away_points.
#(Να περιέχονται μόνο οι στήλες league_id,team_api_id και η στήλη με το μέσο όρο βαθμών εκτος έδρας)

away_team<-team%>%left_join(match_points,by=c("team_api_id"="away_team_api_id"))%>%
  select(league_id,team_api_id,away_team_points)
away_team

away_points<-away_team%>%group_by(league_id,team_api_id)%>%
  summarise(mean_away_team_points=mean(away_team_points,na.rm = TRUE))
away_points

#Ερώτημα 7c: Ενώστε τους δύο πίνακες home_points και away_points σε έναν πίνακα 

all_points<-inner_join(home_points,away_points,by="team_api_id")
all_points

#Ερώτημα 7d: Χρησιμοποιήστε τη βιβλιοθήκη ggplot2 και κάντε ένα διάγραμμα διασποράς του μέσου όρου βαθμών 
#που μαζεύουν οι ομάδες εντός έδρας με το μέσο όρο βαθμών που μαζεύουν εκτός έδρας.Χρησιμοποιήστε και το
#geom_smooth(method=lm) για να δείτε καλύτερα τη γραμμική σχέση. Τι παρατηρείτε;

all_points_plot<-ggplot(all_points,aes(x=mean_home_team_points,y=mean_away_team_points))+
  geom_jitter(alpha=0.4)+geom_smooth(method = lm)
all_points_plot

#Σύμφωνα με το γράφημα, παρατηρώ ότι η μέση τιμή των πόντων της γηπεδούχου ομάδας 
#παρουσιάζει θετική γραμμική εξάρτηση από τη μέση τιμή της φιλοξενούμενης ομάδας


#Στο τέλος, κλείνω τη σύνδεση με τη βάση δεδομένων:
dbDisconnect(con)

