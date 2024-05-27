# NBA-Regular-Season-vs.-Postseason-Play-Styles

Project with Parker Gibbons, as a part of the Stat 306: Multivariate Sports Analytics class with Professor Bradley A. Hartlaub at Kenyon College. April 3rd, 2024.

In this project, Parker and I analyzed the differences in NBA team styles of play between the regular season and postseason. Specifically, we wanted to understand whether or not NBA teams change their style of play in the postseason as compared to the regular season, what aspects of their style of play they change, and whether or not there are risks or adverse effects involved with changing play style in the postseason. Firstly, we completed a MANOVA test to compare the mean statistics between the regular season and postseason statistics, which suggested that there is some difference in playstyle in the regular season as compared to the postseason.

Next, we considered linear regression models to choose 9 variables with which to cluster the different team play styles using K-means clustering. We made a variety of different clusters, including clustering the 80 teams who made the playoffs from the 2018-2019 to the 2022-2023 seasons, which becomes 160 data points considering regular season and postseason separately. Of the 80 teams considered (16 postseason teams during 5 seasons), 47 teams ended up in different clusters in the postseason, while 33 stayed in the same. The teams that were in different clusters (i.e. changed their play style in the postseason) had a mean of 5.7 postseason wins, with a standard deviation of 5.2 and a median of 3. The teams in the same cluster in both the regular season and the postseason had a mean of 4.7 postseason wins, with a standard deviation of 4.2 and a median of 4.

The data already does not appear to suggest a statistically significant difference between the mean number of postseason wins in the group that changed clusters in the postseason as compared to the group that maintained the same playstyle in the postseason. 
This is confirmed when conducting a Two-Sample t-Test, which shows that we do not have significant evidence to support a difference in mean postseason wins in the two groups. Therefore, it appears that there is no evidence that changing play styles has a negative or a
positive effect on postseason wins.

This repository includes a five page executive summary of results, the code (written in R) used to create our models and do the analysis, and the data we considered. If you have any questions or are interested in our process, data, models, code, or analysis, please do not hesitate to reach out!
