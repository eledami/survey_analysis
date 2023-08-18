setwd("~/Cam Insight")

# cleaning ####

library(tidyverse)

survey <- read.csv("Survey_Cam_Insight.csv")
View(survey)
names(survey)
nrow(survey)
entries <- nrow(survey)

survey <- select(survey, -ï..UserID, -Name, -Email, -IP.Address,
                 -Unique.ID, -Started, -Ended, -Tracking.Link)

survey_2 <- rename(survey, c("background"="Q1..Please.let.us.know.in.what.capacity.you.use.Cambridgeshire.Insight.",
                           "usage_frequency"="Q2..On.average..how.often.do.you.use.the.Cambridgeshire.Insight.website.", 
                           "reason_counties"="Q3.1..Finding.data.on.Cambridgeshire.and.Peterborough.as.a.whole", 
                           "reason_districts"="Q3.2..Finding.data.on.a.district.within.Cambridgeshire.or.Peterborough",
                           "reason_smallareas"="Q3.3..Finding.data.on.small.areas.such.as.wards.or.parishes", 
                           "reason_decision"="Q3.4..Informing.decision.making..commissioning",
                           "reason_funding"="Q3.5..Supporting.bids...funding.applications",
                           "reason_maps"="Q3.6..Creating.maps",
                           "reason_rawdata"="Q3.7..Downloading.raw.data",
                           "reason_trends"="Q3.8..Looking.at.data.trends",
                           "reason_other"="Q3.9..Other..please.specify..",
                           "source_work"="Q4.1..Work.request",
                           "source_internetsearch"="Q4.2..Internet.search.for.data.about.Cambridgeshire.and.Peterborough..eg..Google..Bing.search.",
                           "source_colleague"="Q4.3..Recommended.by.a.colleague",
                           "source_academicstudy"="Q4.4..Academic.Study",
                           "source_personalinterest"="Q4.5..Personal.Interest",
                           "source_funding"="Q4.6..To.support.a.bid.for.funding",
                           "source_workshop"="Q4.7..Highlighted.in.a.workshop",
                           "source_other"="Q4.8..Other..please.specify..",
                           "primary_datasource"="Q5..Is.Cambridgeshire.Insight.the.first.place.you.look.to.find.data.on.Cambridgeshire.and.Peterborough.",
                           "other_datasource"="Q6..Where.else.would.you.go.to.look.for.data...e.g..NOMIS..ONS.",
                           "peterborough"="Q7..Do.you.look.for.Peterborough.specific.data.on.the.Cambridgeshire.Insight.website.",
                           "comments_peterborough"="Comments.",
                           "usage_opendata"="Q8..Have.you.used.the.Cambridgeshire.Open.Data.site.",
                           "usage_datastories"="Q9..Do.you.read.the.Data.stories.released.on.the.Open.data.website.",
                           "topic_ASC"="Q10.1..Adult.Social.Care",
                           "topic_CYPE"="Q10.2..Children..Young.People...Education",
                           "topic_CCS"="Q10.3..Crime...Community.Safety",
                           "topic_deprivation"="Q10.4..Deprivation",
                           "topic_economy"="Q10.5..Economy",
                           "topic_environment"="Q10.6..Environment",
                           "topic_HW"="Q10.7..Health...Wellbeing",
                           "topic_housing"="Q10.8..Housing",
                           "topic_JSNA"="Q10.9..Joint.Strategic.Needs.Assessments..JSNA.",
                           "topic_population"="Q10.10..Population",
                           "topic_other"="Q10.11..Other..please.specify..",
                           "design"="Q11..This.question.is.about.how.the.website.looks.Overall..how.satisfied.are.you.with.the.design.of.the.Cambridgeshire.Insight.website.",
                           "ease"="Q12..This.question.is.about.how.easy.it.is.to.use.the.websiteOverall..how.satisfied.are.you.with.the.ease.of.use.of.the.Cambridgeshire.Insight.website.",
                           "breadth"="Q13..This.question.is.about.the.breadth.of.data.on.the.website.Overall..how.satisfied.are.you.with.the.amount.of.data.on.the.Cambridgeshire.Insight.website.",
                           "quality"="Q14..This.question.is.about.the.quality.of.data.on.the.website.Overall..how.satisfied.are.you.with.the.quality.of.the.data.on.the.Cambridgeshire.Insight.website.",
                           "gaps"="Q15..Is.there.anything.missing.from.the.Cambridgeshire.Insight.website.which.would.help.you...This.could.include.new.data..or.changes.in.the.way.the.website.works.for.example.",
                           "value"="Q16..Going.forward..what.value.do.you.want.to.get.from.the.website."))


        View(survey_2)

# Background ####

sapply(survey_2$background, levels)

background_table <- data.frame(table(survey_2$background))

background_table_final <- background_table %>%
                        rename("Background"= Var1) %>%
                        mutate(Percentage = Freq/entries*100)

background_table_final[1,1] = NA

View(background_table_final)

background_table_label <- background_table_final %>%
                          mutate(Percentage_3 = format(background_table_final$Percentage, digits = 3)) %>%
                          mutate(Label=paste(Background, "-", Percentage_3, "%", sep =" "))

View(background_table_label)

safe_colorblind_palette <- c("#000000", "#92A1A1", "#56689F", "#6B9E73", "#C2DDA7", "#F5DD90", "#F68E5F", "#F76C5E", 
                             "#E9AFD2", "#92DDD6")

library(treemap)

treemap(background_table_label,
        index="Label",
        vSize="Percentage",
        type="index", fontfamily.labels = "sans",
        title = "Background of Cambridgeshire Insight Users",
        fontsize.labels = 12,
        title.legend = "Background",
        fontface.labels = 4,
        fontcolor.labels= "white",
        border.col = "white",
        border.lwds = 1.5, 
        palette=safe_colorblind_palette)

# Usage_frequency ####

library("scales")

sapply(survey_2$usage_frequency, levels)

usage_frequency_table <- data.frame(table(survey_2$usage_frequency))

usage_frequency_final <- usage_frequency_table %>%
        rename("Use_frequency"= Var1) %>%
        mutate(percentage = Freq/entries) %>%
        mutate(labels = percent(percentage))

usage_frequency_final[1,1] = NA

View(usage_frequency_final)

ggplot(usage_frequency_final, aes(x = "", y = percentage, fill = Use_frequency)) +
        geom_col() +
        geom_bar(stat="identity", width=1, color= "white") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar("y", start=0) +
                theme_void()

# Reasons

Pivot_reasons <- data.frame(Reasons = c("counties", "districts", "small areas", 
                                "decision", "funding", "maps", "raw data", 
                                "trends", "other"),
           Yes = c(sum(survey_2$reason_counties == 'Yes'),
                   sum(survey_2$reason_districts == 'Yes'),
                   sum(survey_2$reason_smallareas == 'Yes'), 
                   sum(survey_2$reason_decision == 'Yes'), 
                   sum(survey_2$reason_funding == 'Yes'), 
                   sum(survey_2$reason_maps == 'Yes'), 
                   sum(survey_2$reason_rawdata == 'Yes'), 
                   sum(survey_2$reason_trends == 'Yes'),
                   sum(survey_2$reason_other != 'No')),
            No = c(sum(survey_2$reason_counties == 'No'), 
                   sum(survey_2$reason_districts == 'No'),
                   sum(survey_2$reason_smallareas == 'No'), 
                   sum(survey_2$reaons_decision == 'No'), 
                   sum(survey_2$reason_funding == 'No'), 
                   sum(survey_2$reason_maps == 'No'), 
                   sum(survey_2$reason_rawdata == 'No'), 
                   sum(survey_2$reason_trends == 'No'),
                   sum(survey_2$reason_other == 'No')))

View(Pivot_reasons)

Pivot_reasons

value_1 <- seq(0, 30)
value

library(forcats)

Plot_reasons <- barplot(height = Pivot_reasons$Yes, names = Pivot_reasons$Reasons, 
                         xlab = "Reasons",
                         ylab = "Frequency", 
                         ylim = c(0, 25),
                         border="#6B9E73", col="#6B9E73")

text(Plot_reasons, Pivot_reasons$Yes+0.6, paste(Pivot_reasons$Yes), cex=1)

Pivot_reasons %>%
        mutate(Reasons = fct_reorder(Reasons, Yes)) %>%
        ggplot(aes(x=Reasons, y=Yes)) +
        geom_bar(stat="identity", fill="#6B9E73", alpha=1, width=0.7) +
        coord_flip() +
        xlab("Reasons") +
        ylab("Frequency") +
        geom_text(aes(label=Yes), nudge_x = 0, nudge_y = 0.5) + 
        theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'white', linetype = 'dotted'),
        panel.grid.minor = element_line(color = 'white', size = 2))

### Other column an peterborough comments needs Text Analysis

# Sources ####

Pivot_sources <- data.frame(Sources = c("work", "internet search", "colleague", 
                                        "academic study", "personal interest", 
                                        "funding", "workshop", "other"),
                            Yes = c(sum(survey_2$source_work == 'Yes'),
                                    sum(survey_2$source_internetsearch == 'Yes'),
                                    sum(survey_2$source_colleague == 'Yes'), 
                                    sum(survey_2$source_academicstudy == 'Yes'), 
                                    sum(survey_2$source_personalinterest == 'Yes'), 
                                    sum(survey_2$source_funding == 'Yes'), 
                                    sum(survey_2$source_workshop == 'Yes'),
                                    sum(survey_2$source_other != 'No')),
                            No = c(sum(survey_2$source_work == 'No'), 
                                   sum(survey_2$source_internetsearch == 'No'),
                                   sum(survey_2$source_colleague == 'No'), 
                                   sum(survey_2$reaons_academicstudy == 'No'), 
                                   sum(survey_2$source_personalinterest == 'No'), 
                                   sum(survey_2$source_funding == 'No'), 
                                   sum(survey_2$source_workshop == 'No'),
                                   sum(survey_2$source_other == 'No')))
View(Pivot_sources)

Pivot_sources

Pivot_sources %>%
        mutate(Sources = fct_reorder(Sources, Yes)) %>%
        ggplot(aes(x=Sources, y=Yes)) +
        geom_bar(stat="identity", fill="#6B9E73", alpha=1, width=.5) +
        coord_flip() +
        xlab("Sources") +
        ylab("Frequency") +
        geom_text(aes(label=Yes), nudge_x = 0, nudge_y = 0.5) + 
        theme(panel.background = element_rect(fill = 'white', color = 'grey'),
              panel.grid.major = element_line(color = 'white', linetype = 'dotted'),
              panel.grid.minor = element_line(color = 'white', size = 2))

### Other column needs Text Analysis

# Primary Data Source ####

Pivot_datasource <- survey_2 %>%
                        group_by(primary_datasource) %>% 
                        summarise(count = n()) %>%
                        mutate(Percentage = (count/34)*100) %>%
                        rename(Legend = primary_datasource)

Pivot_datasource[1,1] = NA

View(Pivot_datasource)

Pivot_datasource_label <- Pivot_datasource %>%
        mutate(Percentage_3 = format(Pivot_datasource$Percentage, digits = 3)) %>%
        mutate(Label=paste(Legend, "-", Percentage_3, "%", sep =" "))

View(Pivot_datasource_label)

ggplot(Pivot_datasource_label, aes(x = "", y = Percentage, fill = Legend)) +
        geom_col() +
        geom_bar(stat="identity", width=1, color= "white") +
        geom_label(aes(label = Label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar("y", start=0) +
        theme_void()

### Other column needs Text Analysis 

install.packages("wordcloud2")
install.packages("tm")
install.packages("Rtools")

library(wordcloud2)
library(tm)
library(extrafont)

# create corpus
medium.corpus <- Corpus(VectorSource(survey_2$other_datasource))

# text cleaning 

medium.corpus <- medium.corpus %>% 
        tm_map(removeNumbers) %>%
        tm_map(removePunctuation) %>% 
        tm_map(stripWhitespace) %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(removeWords, stopwords("SMART"))

View(medium.corpus)

tdm <- TermDocumentMatrix(medium.corpus) %>%
        as.matrix()

View(tdm)

words <- sort(rowSums(tdm), decreasing = TRUE)

View(words)

df <- data.frame(word <- names(words), freq = words)

View(df)

uxc.colors <- c("#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#999999", "#D55E00", "#CC79A7")

wordcloud2(df, color = uxc.colors,
           rotateRatio = 0, fontFamily = "SimSun")

# Peterborough ####

Pivot_Peterborough <- survey_2 %>%
        group_by(peterborough) %>% 
        summarise(count = n()) %>%
        mutate(Percentage = (count/34)*100) %>%
        rename(Legend = peterborough)

Pivot_Peterborough[1,1] = NA

View(Pivot_datasource)

Pivot_Peterborough_label <- Pivot_Peterborough %>%
        mutate(Percentage_3 = format(Pivot_Peterborough$Percentage, digits = 3)) %>%
        mutate(Label=paste(Legend, "-", Percentage_3, "%", sep =" "))

View(Pivot_Peterborough_label)

ggplot(Pivot_Peterborough_label, aes(x = "", y = Percentage, fill = Legend)) +
        geom_col() +
        geom_bar(stat="identity", width=1, color= "white") +
        geom_label(aes(label = Label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar("y", start=0) +
        theme_void()

# Other tools/usage ####

Pivot_Opendata <- survey_2 %>%
        group_by(usage_opendata) %>% 
        summarise(count = n()) %>%
        mutate(Percentage = (count/34)*100) %>%
        rename(Legend = usage_opendata)

Pivot_Opendata[1,1] = NA
Pivot_Opendata[2,1] = "No - but I am aware of the website"
Pivot_Opendata[3,1] = "No - never heard of it"

View(Pivot_Opendata)

Pivot_Opendata_label <- Pivot_Opendata %>%
        mutate(Percentage_3 = format(Pivot_Opendata$Percentage, digits = 3)) %>%
        mutate(Label=paste(Legend, "-", Percentage_3, "%", sep =" "))

View(Pivot_Opendata_label)

ggplot(Pivot_Opendata_label, aes(x = "", y = Percentage, fill = Legend)) +
        geom_col() +
        geom_bar(stat="identity", width=1, color= "white") +
        geom_label(aes(label = Label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar("y", start=0) +
        theme_void()

###

Pivot_datastories <- survey_2 %>%
        group_by(usage_datastories) %>% 
        summarise(count = n()) %>%
        mutate(Percentage = (count/34)*100) %>%
        rename(Legend = usage_datastories)

View(Pivot_datastories)

Pivot_datastories[1,1] = NA

View(Pivot_datastories)

Pivot_datastories_label <- Pivot_datastories %>%
        mutate(Percentage_3 = format(Pivot_datastories$Percentage, digits = 3)) %>%
        mutate(Label=paste(Legend, "-", Percentage_3, "%", sep =" "))

View(Pivot_datastories_label)

ggplot(Pivot_datastories_label, aes(x = "", y = Percentage, fill = Legend)) +
        geom_col() +
        geom_bar(stat="identity", width=1, color= "white") +
        geom_label(aes(label = Label),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar("y", start=0) +
        theme_void()

# Pivot_topic ####

Pivot_topics <- data.frame(Topics = c("Adult Social Care", 
                                       "Children, young people and education", "CCS", 
                                        "deprivation", "economy", 
                                        "environment", "Health and Wellbeing", "housing", "JSNA", 
                                       "population", "other"),
                            Yes = c(sum(survey_2$topic_ASC == 'Yes'),
                                    sum(survey_2$topic_CYPE == 'Yes'),
                                    sum(survey_2$topic_CCS == 'Yes'), 
                                    sum(survey_2$topic_deprivation == 'Yes'), 
                                    sum(survey_2$topic_economy == 'Yes'), 
                                    sum(survey_2$topic_environment == 'Yes'), 
                                    sum(survey_2$topic_HW == 'Yes'),
                                    sum(survey_2$topic_housing == 'Yes'),
                                    sum(survey_2$topic_JSNA == 'Yes'),
                                    sum(survey_2$topic_population == 'Yes'),
                                    sum(survey_2$topic_other != 'No')),
                            No = c(sum(survey_2$topic_ASC == 'No'), 
                                   sum(survey_2$topic_CYPE == 'No'),
                                   sum(survey_2$topic_CCS == 'No'), 
                                   sum(survey_2$topic_deprivation == 'No'), 
                                   sum(survey_2$topic_economy == 'No'), 
                                   sum(survey_2$topic_environment == 'No'), 
                                   sum(survey_2$topic_HW == 'No'),
                                   sum(survey_2$topic_housing == 'No'),
                                   sum(survey_2$topic_JSNA == 'No'),
                                   sum(survey_2$topic_population == 'No'),
                                   sum(survey_2$topic_other == 'No')))

Pivot_topics %>%
        mutate(Topics = fct_reorder(Topics, Yes)) %>%
        ggplot(aes(x=Topics, y=Yes)) +
        geom_bar(stat="identity", fill="#6B9E73", alpha=1, width=.5) +
        coord_flip() +
        xlab("Topics") +
        ylab("Frequency") +
        geom_text(aes(label=Yes), nudge_x = 0, nudge_y = 0.5) + 
        theme(panel.background = element_rect(fill = 'white', color = 'grey'),
              panel.grid.major = element_line(color = 'white', linetype = 'dotted'),
              panel.grid.minor = element_line(color = 'white', size = 2))

### Other column needs Text Analysis

# Likert scales ####

Pivot_design <- survey_2 %>%
        group_by(design) %>% 
        summarise(count = n())

Pivot_design_percentage <- Pivot_design %>%
        mutate(percentage = count/entries) %>%
        mutate(labels = percent(percentage))

View(Pivot_design_percentage)

barplot(Pivot_design_percentage$percentage, main = "Barchart", xlab = "Percentage",
        col = c("darkgrey", "darkblue", "red", "green"), 
        legend.text = Pivot_design_percentage$design, 
        horiz = TRUE, beside = TRUE)

?barplot

Pivot_ease <- survey_2 %>%
        group_by(ease) %>% 
        summarise(count = n())

Pivot_ease_percentage <- Pivot_ease %>%
        mutate(percentage = count/entries) %>%
        mutate(labels = percent(percentage))

Pivot_breadth <- survey_2 %>%
        group_by(breadth) %>% 
        summarise(count = n())

Pivot_breadth_percentage <- Pivot_breadth %>%
        mutate(percentage = count/entries) %>%
        mutate(labels = percent(percentage))

Pivot_quality <- survey_2 %>%
        group_by(quality) %>% 
        summarise(count = n())

Pivot_quality_percentage <- Pivot_quality %>%
        mutate(percentage = count/entries) %>%
        mutate(labels = percent(percentage))

# Gaps and Values (need text analysis) ####


