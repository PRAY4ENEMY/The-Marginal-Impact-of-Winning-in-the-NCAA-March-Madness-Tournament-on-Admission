This repository is designed to showcase my economics research on college admissions 

# Introduction

**Abstract:** The utility of success in college athletics is often debated by those who have an interest the broader success of institutions of higher education. While some believe that any dollar spent on athletics is a dollar not going towards investing in educational infrastructure, other hold that success in sports can be an effective mechanism for raising the national profile of a university. After adjusting winning based on a teams seeding in the NCAA's *March Madness* tournament and using a fixed-effects regression model, we can estimate the causal impact of post-season success in NCAA Division 1 basketball on the quantity of applications that a university receives. We find that success, conditional on expectations, is causally correlated with a 1% increase in the number of applications a schools admission office receives.

American Universities are unique in their willingness to spend large sums of money on their athletics teams. The two sports that are most responsible for this spending are college basketball and football. The NCAA *March Madness* tournament (the tournament) is one of the most recognizable fixtures of intercollegiate athletics. The entertainment apparatus that has popped up around this event is incredibly lucrative with 9.7 million viewers in 2018 and a 8.8 billion dollar exclusive broadcasting contract held by Columbia Broadcasting System (CBS) and Turner Sports. The upkeep of athletic departments is also major business. According to the NCAA technical report in 2018, the total expenditures of NCAA schools amounted to a total of 18.1 billion dollars. the total revenue reported in that year totaled to 18.2 billion dollars. Despite reporting net profits in 2018, that figure includes 6.5 billion dollars in *Institution and Government support* and 1.5 billion dollars in Student fees [@NCAA18]. When taken together, these revenues account for 8 billion or 44% of the inflows received by athletic departments that are a part of the NCAA. This calls into question the financial viability of college athletics because these funds could be used by universities in ways that are not related to athletics. In fact, out of the over 350 schools that participated in NCAA athletics in 2010, 18% of them were able to internally cover their operating costs [@Anderson17]. 

There is an argument to be made, however, that athletic success has other positive effects on universities that justify the funds that they are given. In this manner we can think of athletics teams as amenities that students value [@Jacob18]. There is literature to suggest that at least some prospective college students state that a universities athletic prowess is a factor in their decision to attend a particular university [@Moshe22]. There is also literature to suggest that athletics helps college bring in more donations [@Humphrey07]. The main question that universities are tasked with answering is whether or not investments in sports yield any sort of return in any relevant domain?

There are many ways that this question has been answered in the literature but this paper will focus on the relationship between athletics success in the tournament^[ The NCAA *March Madness* tournament is an annual 64 team tournament that determines the national champion in Division-I basketball for that year. Teams are invited to the tournament based on how their regular season performance is evaluated by the NCAA selection committee. Generally speaking they select the best 64 teams] and the amount of applications that a university receives. The number of applications received by a university in a given admissions year is important for school because it gauges a general interest in their university and theoretically allows them to be more selective and pay-out less in financial aid, scholarships and etc.

The problem with identifying the causal relationship between success in the tournament and the number of applications received is two fold. The first is the problem of omitted variable bias. Schools with successful athletic departments likely have money to invest into better athletic facilities, coaches and etc. and thus may be more likely to also able to invest into other facilities that make students more likely to apply to them. While money invested into athletics doesn't have a one-to-one relationship with actualized success [@Yi10], it is a general trend and does represent and endogeneity problem. We overcome this endogeneity problem by using a fixed effect model at the School-Year level. The second issue with finding the causal relationship is in how we define success for each team. Because of the nature of college basketball, and college sports in general, teams of widely varying strength and prestige play each other and have different levels of expectations that change the way they and prospective students perceive each additional win. For example, in 2018, The University of Tennessee (UT) and the University of Maryland, Baltimore County (UMBC) both won one game but came into the tournament with vastly different expectations. UT was considered the 6th best team in the country and expected to win several games. UMBC on the other hand, was part of the biggest upset victory against the University of Virginia who was the top rated team in the country that year. To overcome this issue, I utilize each teams seeding^[ Teams are "Seeded" into the tournament in such a way that the best teams play the weaker teams in the first round of the tournament] into the tournament as a proxy to control for the teams expectations entering into the tournament. 

The results of this study suggest that their is a small, but statistically significant relationship between success in the NCAA tournament and the quantity of applications received by the university. 

# Literature Review

The most recent paper in the literature to examine this question found that there was a 2-8% increase in the quantity of applicants and no increase on the average SAT score of the applicant pool. @Pope09 examined the relationship between success in NCAA Division I sports and the quantity and quality of student applications. They use the number of applications received by the university as a measure of the quantity and the average SAT score as a proxy for quality. While there are several problems with a standardized test such as the SAT, the use of SAT as a measure make sense given that school has an interest in increasing their average standardized test score as it impacts their overall ranking in outlets such as *U.S. News* [@Usa22]. The researchers use data from both *Peterson's Guide to America's Colleges and university* as well as the College Board's *SAT Database*. Utilizing both of these data sets allows the authors to compare individual data with headline numbers for robustness. The researchers state that a lag period of one year (T-1) would most accurately capture the impact of collegiate athletic success. This is because of the timing of prospective students sending in their applications to each school. While decision processes across colleges vary by deadline and rolling admission, the length and relative cost (financial and time costs) felt by students indicate that athletic success impacts proceeding periods. They also look at a lag period of 2 and 3 to examine the persistence of the impact of athletic success from 2 to 4 years prior. They, however, did not find any significant results. Their finding suggests that college applicants have a short memory when it comes to recalling the previous success of sports teams. 

Grimes and Chressanthis Sought out to find how NCAA sporting success is related to alumni contributions to the academic endowment. The authors found that alumni contributions are positively correlated with academic giving at a .10 significance level [@Grimes94]. This research offered three unique contributions to the literature. The first is the time series nature of his data set, the second is the data sets inclusion of NCAA sanctions data which offers a unique point of analysis. The third contribution relates to data on television exposure. Additionally, this paper focuses in on academic donations instead of overall donations. By looking at television exposure, authors Grimes and Chressanthis bring up and interesting question concerning the mechanism by which college athletics impacts the a given dependent variable. It could be argued that the relationship would be indistinguishable from effective television advertising. While this paper offers some interesting insights, the methodology is outdated, the sample size is small (N = 30), and the analysis includes baseball success which is not applicable to every school in the data set. Regardless, this paper is often cited in similar research literature.

The @KernA09 looked at the impact of college football and basketball performance on Tuition rates at public universities. They utilize wins/loss record in both football and basketball as a measure of athletics success. the authors also look at both in-state and out-of-state tuition. One of the main problems faced by this paper is the fact that students rarely pay the 'sticker price' and that universities often employ price discrimination. As a result, they removed private universities from their sample. they do not take into account any data concerning scholarships and financial aid. Their reasoning for this decision was rising sticker price is an indication of rising prices of tuition and fees actually paid to the university. This paper also is one of the most expansive in the literature as it looks at 181 school over the course of 23 years. Both the time-span and number of observations of the data set allows the authors to make statements on prolonged success in sports and provides a large sample side. The authors found that their was a significant and positive impact of both in-state and out-of-state tuition. However, they found that the impact was mostly relegated to schools in the Major 6 power conferences (Big Ten, Big 12, PAC-12, Big East, ACC, SEC).

Anderson examined the causal relationship between athletic success and a myriad of success metrics such as SAT scores of the incoming class, acceptance rate and alumni contributions [@Anderson17]. In his data, he chooses schools with football teams who participate in the Bowl Championship Series (BCS) n=394. What make this paper stand out from the rest of the literature is how the author uses a propensity score design to isolate the causal impact. instead of taking raw win/loss records, the authors weights them by the expectations of bookkeepers. the author states that, "conditional on bookmakers spreads, winning is uncorrelated to potential outcomes. When using this methodology, the paper finds that winning was positively correlated to success in reducing acceptance rate, increasing donations, applications received, in-state enrollment, incoming SAT scores, Alumni contributions to athletics, SAT scores of the incoming class, first-time in-state enrollment and the number of applicants were statistically significant. The author also discusses how success is more important, both to teams, fans and prospective students, during different parts of the year. to control for this, the author drops games from the first part of the year in one of his regressions. 

Other researchers, such as @Moshe22, have taken a survey approach to exploring the link between success in athletics and success in the institution generally. More than half of survey respondents stated that college athletic success was at least an important factor. 37.6% of respondents stated that they would not consider applying to a university that didn't have a successful college football team. They also find that gender plays a statistically significant role in whether or on college athletic success is important for prospective students. Male students were more likely to cite sports as an important factor. While this paper doesn't utilize hard econometrics, it could potentially be important to understanding whether college athletic success only works on institutional success through increasing a school exposure to the broader public. 

# Data

In order to construct our panel data set for the fixed-effects regression model, we combine two separate data sets in this analysis. For March Madness results data, we utilize a data set hosted curated by "micheal.roy" on *data.world*^[ Data World is a service that is hosted by Google LLC]. For the data on university, we utilize the data collected by the National Center for Education Statistics (NCES). The data points that are included into the study are school-years that participated in the tournament between the academic-years of 2008-2009 and 2017-2018 (n = 640). Starting in the year 2008, the NCES started tracking net-price data which is the average price that is actually paid by students after financial aid, scholarships and grants are included. This data point gives us the opportunity to more accurately control for tuition and is a big advantage over previous research into this topic. The year over year percent change in the number of applications received is used to control for varying sizes of applicant pools.  

### Summary Statistics 

### Why basketball? 

While most other literature combines both football and basketball success into their models, this paper focus exclusively on post-season basketball success. The reason for this is because college basketball is a very easy sport to analyse, especially when compared to college football. This is for several reason. Primarily, basketball is structured to be as unpredictable as possible. The game time of an NCAA basketball game is 40 minutes, compared to the hour long game time of professional basketball, allows for fewer "observations" of skill which all for more upsets where a favored team losses. Additionally, basketball features an outside three point shot which is offers a higher reward for a higher risk of not making the shot. A game plan that relies on 3-point shots is a very volatile strategy. If a lower skilled team hits a favorable streak of makes and the higher skilled team hits a streak of bad luck, the lower skilled could potentially win. The volatility of the game is what makes it exiting for both fans and economists because it offers the opportunity to observe variation. Basketball teams are also able to play more games which gives a larger number of observation. Finally, College basketball is not dominated by a select handful of teams. While there are certainly teams that perform well consistently, there are more teams who have a reasonable shot at winning the tournament in a given year when compared to college football. One concern with exclusively looking basketball is that we could be omitting an important variable in football success. However, according to @Pitts18, the football success is not indicative of basketball success. 

### Success in basketball and admissions. 

Again, teams enter into the tournament with different levels of expectations placed upon them by the college basketball audience. Some teams are happy with winning one or two games while others are expected to make deeper runs in the tournament. While it can be difficult for us to observe this variable, we can use the NCAA selection committee's seeding rankings to gauge the expectations that they have for the team. This is the most objective way to quantify the subjective feeling of expectations. 1 seeds in the tournament are the four best teams according to the selection committee and 2 seeds are the next four teams. This seeding process is repeated until their are 64 teams selected to participate in the tournament. The bracket of the tournament has four sides each with one team that is rated between 1-16 seeds. Because the tournament is structured in this manner, we can say that a teams expected wins are the number of games they would win if we assume that the higher seeded team wins every match up. We can then model a teams *performance* in the NCAA tournament as *Wins - Expected wins*. This variable give us the number of wins above or below expectation for any given School-Year. The idea of using some outside measure of team strength as a way to control for un-observed variable bias is taken from Anderson (2017). In his model, he utilizes betting spreads and posits that, "Conditional on bookmakers spreads, winning is uncorrelated to potential outcomes.”

# Methodology

The identification strategy used to find the casual relationship between success in the tournament and the quantity of applications received is the fixed-effects model. The main regression can be modeled as: 
$$
\%\Delta applications_{it} = \beta_0 + \beta_1 Performance_{it-1} + \delta school_{i} + \lambda year_{t} + \epsilon_{it}
$$
Additional variables are added to control for seeding, net price and enrollment with are also added. We also drop the 10% most selective schools by ACT score. Net price is defined as the average amount of money paid by the students after accounting for financial aid, scholarships and grants. Enrollment is measured as the total undergraduate student headcount at the beginning of the academic year. By dropping these selective schools we should see an increase in the effect size of $\beta_{1}$ because athletic prowess is less important for these schools because their is high demand for attending these schools. 

The main limitations of this model have to do with the *performance* variable. In the model a teams could win up to four more games than another team and get the same performance score. While it is ultimately subjective, it is hard to argue that these two hypothetical teams are likely to see equal results from their performance. There is also no way to determine if any of the teams are happy simply to make the tournament and would see positive results in terms of applications received.  

In accordance with previous literature, the model uses a lag period of one year. This lag period is selected because of the way that the college basketball and admissions seasons line up. Prospective students are tasked with making their decisions by the end spring of the calendar year which is a 1-3 after the conclusion of the basketball season. The way that admissions applications are reported is by incoming class which would put the reporting year of this number on year ahead of basketball results [@Pope09]. It is also unnecessary to include further lag periods in the model (See literature review).^[I did construct a regression model with further lag periods and found no statistical significance. This finding is consistent with the finding of Pope (2009)]. 

# Results 

Using the main regression, we find an effect size of .9% increase in applications received per win above expectation. After adding the additional controls of net price, seeding and enrollment we find no change is statistical significance or effect size. When we remove the top 10% most selective schools in the data set, we find a slight increase in the effect size with a 1.1% increase in applications received per win above expectation. Again, the additional controls did not change the results.


# Conclusion and implications. 

Based on the results of the fixed effect regression table, we can conclude that there is a small, but statistically significant casual relationship between the wins above expectations in the NCAA *March Madness* tournament and the number of applications that a university receives. That said, the effect size is rather small and the statistical significance is precarious with it being at the .10 level. 

These results are somewhat contradictory with, although not completely out of line with, the results from previous literature on the subject. Pope & Pope (2009), the paper most similar to this one, found that making the the final 16 (two wins) represented a 3.2% increase in applications received. Two wins above expectations in the model of this paper would represent an increase of 2.2%. My results also differ from Anderson (2017). While he found statistically significant results for multiple measure of university success, I only found that applications received were correlated with success in athletics in a statistically significant manner. 

Based on these results, it is difficult to make an argument that institutional resources should be put into improving the results of sports teams at a university. While administrators might be persuaded by a handful of schools that find success in both admissions and sports. The data suggest that there is limited marginal utility in winning additional games in basketball. Athletics should not be seen as a way to increase the national profile of a school. Instead resources should be used to invest in other facilities that increase a students likelihood in attending a particular university. 

\newpage

# Works Cited

