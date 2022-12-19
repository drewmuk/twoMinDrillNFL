# twoMinDrillNFL

I wanted to look at how (if it all) NFL teams perform differently in two-minute drill situations. In these spots, time is a factor and teams are in need of quick, long plays to score a touchdown or field goal before the clock expires. 

I'm only looking at pass plays; they can stop the clock and gain yards more quickly than runs, so they're more relevant to two-minute drills. This refers to A) two minutes or less left in the 1st half, or B) two minutes or less in the 2nd half when the offensive team is down by 8 points or less. 

Two ways of investigating the differences are 1) comparing the outcome of two-minute drill plays to normal plays, or 2) seeing if there are patterns in which players are involved. To address the first one, I'm looking at yards gained and yards after catch per completion, and location frequency of pass completions (outside, middle, short, or deep). And for the second, I'm desigining a k-mean analysis of receivers to classify them into groups, and checking the rate at which the groups are targeted in different parts of the game. 

My hypothesis was that pass completions in two-minute drills tend to result in more yards per play, and tend to be deep and outside (going out of bounds stops the clock). Furthermore, the prototypical WR1s (Cooper Kupp, Justin Jefferson, Davante Adams, etc.) will get more receptions during two-minute drills compared to running backs, tight ends, or slot receivers. But after running the analysis, it doesn't seem like there are any clear differences in teams' tendencies when they're in two-minute drills. Receivers have slightly more deep outside receptions than normal (15.4% compared to 14.6%) but fewer short outside receptions (60.3% to 63.1%), so QBs aren't necessarily aiming for the sidelines. Each cluster has a nearly identical proportion of catches (within 2 percentage points for each), perhaps surprising since common sense would dictate the 'big play' WR1s would be more impactful during this time.

The clustering was actually very successful, with the receivers being sorted into categories that match quite well with conventional wisdom about types of receivers. Kupp, Jefferson, Adams, Chase, Diggs, etc. were all in one category, while running backs and some slot WRs were in another, and TEs and big redzone WRs filled out the third one. They each had clear and definable characteristics (lots of chunk plays, high yards after catch stats, etc.)

![image](https://user-images.githubusercontent.com/102569479/208359941-ea251103-2443-41cf-86d8-824b239c7163.png)


Looking into targets instead of just catches would probably lend some more insight, as well as separating by QB, offensive scheme, etc. But the initial steps of getting the data set up and being able to classify WRs in a way that aligns with the eye test signify a good start. 
