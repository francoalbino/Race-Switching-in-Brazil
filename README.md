# Race-Switching-in-Brazil: The Project's idea

As the name makes clear, this project addresses a VERY interesting phenomenon occuring in Brazil. In this country, if you want to present your candidacy to run in the elections, you'll have to present some information about yourself. This includes some contact information, your education, your marital status, among other things. But, most importantly for our research purposes, you have to SELF DECLARE YOUR RACE. But... why is this interesting? Well, as you might have guessed from the project's title, candidates switch their self-declared race between election. Yes, you read well. Brazilian candidates are doing this. For real. If you don't believe me in this repository there are some tables that prove me right. Trust me.

Anyway, I guess this opens up a lot of questions. Don't you think? For instance, (and most importantly) why the F are candidates are doing this??? By asking this, you are asking yourself about what candidates gain by doing this. A good approximation might be that candidates care about their vote share (right?), so they think that by switching race, they might win votes because, for example, the race to which they switch is more representative of their potential voters. Also, one might think that candidates have costs of switching as, for instance, voters do not like candidates who have a "flexible" race.

All this questions and more are what we intend to address in this project. To do that, we analyze government data on candidates and their electoral outcomes for the brazilian municipal elections of 2016 and 2020. You can access the raw data in this link: https://dadosabertos.tse.jus.br/dataset/. Here, you will find a laaaarge number of datasets containing, for each year, information about the candidates, their electoral outcomes, the electorate composition, the voter turnout, etc. This being said, I can now talk about me, that is to say, my role in this project:

# My role in the Project

My role here was, in principle, to use this datasets to construct a panel data, in which each row had to represent the information about a single candidate in a specific year. The most relevant variables were the candidate's information, including his/her self-declared race, and his/her vote share. Once I had the panel data, I produced some descriptive measurements, tables and graphs, some of which you can see in this repository.

Now, as we are economists and, consequently, we are pretty smart, we couldn't stop there. We had to try to infer some causality. Therefore, I used the panel data to run regressions and in that way test if there is any relationship between switchig race and getting more or less votes, whether candidates who switched to a specific race we more or less favoured by voters, among other hypotheses. 

With the results at hand, the two main researchers in the project dedicated themselves to write a paper about what we had found. In this repository you can also find this paper and therefore check by yourself if the results are interesting or not. 

After this, we said: "You know what might be cool? To have coloured photographs of the candidates and, therefore, be able to categorize them according to their skin tone. If just this brazilian official website had exactly this." Well, it turns out the brazilian official website had exactly this, luckily. I then moved to develop a machine learning algorithm that would take a folder of photographs with the faces of brazilian candidates and place each photograph into a category within a skin tone palette. You can find a presentation of how I did this in this repository.

# What can you find in this repository?

To summarize, in this repository you will find every single thing about the Race Switching in Brazil Project. Namely, you will find the R scripts that contain the code to construct the panel data and the data analysis. Also, you will find the Python code that has part of the machine learning algorithm. Finally, you can find the most relevant outputs, in particular, the paper, the presentation of the machine learning algorithm, as well as some tables and graphs.

What about the raw databases? You can find them in this Drive: https://drive.google.com/drive/folders/10xyaP2BY-O5chC2WqqsXmy53dYiEdM-g?usp=sharing

ENJOY!!!
