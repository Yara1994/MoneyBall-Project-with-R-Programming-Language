<h1 align="center">MoneyBall Project with - R Programming Language</h1>

<div align="center">This project is based off the book written by Michael Lewis (later turned into a movie).</div>

<br>

<p align="center">
  <img src=Images/Moneyball%20Title.jpg>
</p>

----------------------------------------------------------------------------------------------------------------------------------------

<div align="center">
   <h3><a href='https://www.youtube.com/watch?v=yGf6LNWY9AI'>Moment from the movie (YouTube)</a></h3>
</div>

<p align="center">
  <img width="950" height="600" src=Images/Moneyball.jpg>
</p>

----------------------------------------------------------------------------------------------------------------------------------------

<div align="center">Source: Wikipedia</div>

<h3 align="center">The 2002 Oakland A's</h3>

<br>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The Oakland Athletics' 2002 season was the team's 35th in Oakland, California. It was also the 102nd season in franchise history. The Athletics finished first in the American League West with a record of 103-59.</br>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The Athletics' 2002 campaign ranks among the most famous in franchise history. Following the 2001 season, Oakland saw the departure of three key players (the lost boys). Billy Beane, the team's general manager, responded with a series of under-the-radar free agent signings. The new-look Athletics, despite a comparative lack of star power, surprised the baseball world by besting the 2001 team's regular season record. The team is most famous, however, for winning 20 consecutive games between August 13 and September 4, 2002. The Athletics' season was the subject of Michael Lewis' 2003 book Moneyball: The Art of Winning an Unfair Game (as Lewis was given the opportunity to follow the team around throughout that season)

<h3 align="center">Moneyball Book</h3>

<br>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;The central premise of book Moneyball is that the collective wisdom of baseball insiders (including players, managers, coaches, scouts, and the front office) over the past century is subjective and often flawed. Statistics such as stolen bases, runs batted in, and batting average, typically used to gauge players, are relics of a 19th-century view of the game and the statistics available at that time. The book argues that the Oakland A's' front office took advantage of more analytical gauges of player performance to field a team that could better compete against richer competitors in Major League Baseball (MLB).

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Rigorous statistical analysis had demonstrated that on-base percentage and slugging percentage are better indicators of offensive success, and the A's became convinced that these qualities were cheaper to obtain on the open market than more historically valued qualities such as speed and contact. These observations often flew in the face of conventional baseball wisdom and the beliefs of many baseball scouts and executives.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;By re-evaluating the strategies that produce wins on the field, the 2002 Athletics, with approximately US 44 million dollars in salary, were competitive with larger market teams such as the New York Yankees, who spent over US$125 million in payroll that same season.


<p align="center">
  <img width="850" height="600" src=Images/salary.png>
</p>


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Because of the team's smaller revenues, Oakland is forced to find players undervalued by the market, and their system for finding value in undervalued players has proven itself thus far. This approach brought the A's to the playoffs in 2002 and 2003.

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;In this project I'll work with some data and with the goal of trying to find replacement players for the ones lost at the start of the off-season - During the 2001???02 offseason, the team lost three key free agents to larger market teams: 2000 AL MVP Jason Giambi to the New York Yankees, outfielder Johnny Damon to the Boston Red Sox, and closer Jason Isringhausen to the St. Louis Cardinals.

----------------------------------------------------------------------------------------------------------------------------------------

<h3 align="center">Data</h3>

<br>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;I'll be using data from [Sean Lahaman's Website](http://www.seanlahman.com/baseball-archive/statistics/) a very useful source for baseball statistics. The documentation for the csv files is located in the `readme2013.txt` file. You may need to reference this to understand what acronyms stand for.

----------------------------------------------------------------------------------------------------------------------------------------

<h3 align = "center">Our goal is to find Replacement Players for the key three players we lost! There are 3 criterias:</h3>

<br>
<ul>
  <li>The total combined salary of the three players can not exceed 15 million $.<strong> (Total Combined Ex-Player's Salary was: 11,493,333 $)</strong></li>
  <li>Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.<strong> (Combined Ex-Player's AB score was: 1469)</strong></li>
  <li>Their mean OBP had to equal to or greater than the mean OBP of the lost players.<strong> (Mean (Average) Ex-Player's OBP score was: 0.364)</strong></li>
</ul>
<br>

<h4 align="center">3 Ex-Players and their stats, who left the team</h4>

<p align="center">
  <img src=Images/Ex%20Players.PNG>
</p>

<h4 align="center">Dependence of On Base Percentage(OBP) and Salary</h4>

<p align="center">
  <img src=Images/OBP%20and%20Salary%20-%20Full.png>
</p>


&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Looks like there is no point in paying above 5 million or so (I'm just eyeballing this number). There are also a lot of players with OBP==0. Let's filter them out.

<h3 align="center">19 players, that can be great substitution for our Ex-players:</h3>

<p align="center">
  <img src=Images/Potential%20Replacement.PNG>
</p>

<div align="center">Let's visualize them as well.</div>
<br>

<p align="center">
  <img src=Images/OBP%20and%20Salary%20-%20Filtered.png>
</p>

<div align="center">3 Cheapest Players</div>
<br>

<p align="center">
  <img src=Images/3%20Cheapest%20Players.PNG>
</p>

<div align="center">3 Players with the Best On Base Percentage (OBP)</div>
<br>

<p align="center">
  <img src=Images/3%20Best%20OBP.PNG>
</p>

<div align="center">3 Players with the Best At Bats (AB)</div>
<br>

<p align="center">
  <img src=Images/3%20Best%20AB.PNG>
</p>
