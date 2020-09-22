#########################################################################
########################### THE ATHLETIC ################################
#########################################################################

###################### LOADING PACKAGES ################################
library(magrittr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tm)
library(readr)
library(ggplot2)
library(scales) 
library(wordcloud)
library(reshape2)
library(stringr)
library(igraph)
library(ggraph)
library(textreadr)

#################### LOADING DATA  #####################################

Ken_Rosenthal <- c("
Rosenthal: Delayed trade and awkward arbitration hearing prompt more friction between players and the league
In the 45-year history of salary arbitration in baseball, no one on the players’ side could remember taking the action they did Thursday, shortly before the start of Joc Pederson’s hearing.
With an agreement reportedly in place for Pederson to be traded from the Dodgers to the Angels, the players’ union and the outfielder’s agents at Excel Sports Management filed a motion for the hearing to be delayed, contending that Pederson effectively was in limbo, a man without a team.
Major League Baseball already had rejected the union’s request. The three-person arbitration panel did the same. And thus began another odd chapter in an extraordinary week that started with two connected, captivating trade agreements and ended with the union again expressing public frustration over the way MLB treats its players.
Both the three-team blockbuster that would send star outfielder Mookie Betts to the Dodgers and a separate deal that would send Pederson to the Angels remained on hold Friday night. The trades are still alive, but likely will not be completed in their original, reported form due to concerns the Red Sox raised after viewing the medical records of Twins right-hander Brusdar Graterol, one of the two players they would receive for Betts and left-hander David Price.
The fallout from the impasse prompted union head Tony Clark to issue a scathing statement shortly after 5 p.m. ET on Friday. The statement included a reference to Pederson’s hearing, calling it a “perversion of the salary arbitration process.” Pederson, 27, lost his case, and his salary in 2020 will be the Dodgers’ proposed $7.75 million rather than his requested $9.5 million.
“The proposed trades between the Dodgers, Red Sox, Twins and Angels need to be resolved without further delay,” Clark said. “The events of this last week have unfairly put several players’ lives in a state of limbo. The unethical leaking of medical information as well as the perversion of the salary arbitration process serve as continued reminders that too often players are treated as commodities by those running the game.”
The collective-bargaining agreement says a hearing “may be postponed by the arbitration panel upon the application of either the Player or Club based upon a showing of substantial cause.” The uncertainty surrounding Pederson satisfied that requirement in the view of the union and his agents.
Instead, the Dodgers went to a hearing for the first time since winning a case against reliever Joe Beimel in 2007 and paying him $912,500. The team avoided arbitration by awarding multiyear extensions to two other current players, signing infielder Max Muncy for three years, $26 million on Thursday and utility man Chris Taylor for two years, $13.4 million on Friday.
MLB’s position on Pederson, backed by the arbitration panel, was that the hearing could proceed because the player had not yet been traded officially, and might never be. The league also was confident that the Dodgers would win their case against Pederson and perhaps feared establishing a precedent for delays would embolden other players to seek ways out of hearings in the future.
MLB and the union negotiate the arbitration schedule, with the hearings taking place between Feb. 1-20. The union offered multiple dates to reschedule Pederson’s case, but hearings generally are postponed only when a player and team are in agreement on a multiyear contract, pending a physical, sources said.
A delay of four or five days would have been fairer to Pederson, his representatives contended, allowing time for the trade negotiations to resolve. If the deal had been completed during that time, the Angels might have reached a settlement with Pederson rather than make a case against a player they had just acquired. If the trade had fallen through, Pederson would simply have gone to a hearing with the Dodgers.
Instead, Pederson’s side ending up in the awkward position of trying to prove his value for a team that was in the process of trading him. The Dodgers used outside counsel to argue its case, a common practice among clubs. The absence of top Dodgers officials from the hearing in Phoenix was more unusual.
Which, to those on the players’ side, was the point.
The salary-arbitration process is designed for players and their employers to resolve their differences either through settlement or an arbitration panel’s decision. Pederson, though, was not clear on which team employed him, sources say. He already had received text messages from members of Dodgers management wishing him well with the Angels.
Then there was the actual case, in which the Dodgers, according to sources, argued that Pederson was a platoon player, and not worthy of his requested salary. Pederson, a left-handed hitter, has only 375 plate appearances against left-handers in his career, compared to 2,004 against righties. The Dodgers, after asking him and other members of their club to sacrifice playing time for the benefit of the team, effectively held it against him.
Another problem for Pederson’s representatives was that they could not make a case for how he might help the Angels going forward — a component that arbitrators take into account. Pederson likely would play a greater role for the Angels than he did with the Dodgers, replacing Kole Calhoun as the team’s primary right fielder, at least until the promotion of top prospect Jo Adell, and even filling in at first base. He also would bring winning pedigree as a past contributor to two World Series teams and five straight division champions. The Angels have not won a postseason game since 2009 or a playoff round since 2002.
The process, those on the players’ side say, was prejudiced against Pederson, preventing him from effectively arguing his case. In their view, the postponement of the hearing was only sensible. MLB and the arbitration panel disagreed. And an unusual situation that might have resolved amicably only sparked another dispute between the two sides.
             ")

Ken_Rosenthal_df <- data_frame(line=25:42, text=Ken_Rosenthal)

Joe_Posnanski <- c("
In remembrance of ‘The Boys of Summer’ author Roger Kahn, who found beauty in the stories of underdogs
In this house, I have three different hardback copies of Roger Kahn’s “The Boys of Summer.” I also have one on my Kindle and one in my iBooks collection. And yet, not more than three or four weeks ago, I was in a used bookstore, and I saw a paperback copy of the book, and I thought to myself: “I should probably buy that.” And so I did.
I simply need that book around me at all times.
Roger Kahn died Thursday. He was 92. I never met him. I got a note from him once, a nice note, and as I wrote on Twitter, it was like getting a message from Olympus. A few years later, his book publisher, Mark Weinstein, asked me to write a blurb for Roger’s last book, “Rickey & Robinson: The True, Untold Story of the Integration of Baseball.” It was like having Julius Erving ask me to endorse one of his dunks.
This is how I began that blurb:
“Roger Kahn’s classic, ‘The Boys of Summer,’ changed my life — that and ‘Catcher in the Rye’ were the two books that made me dream of becoming a writer.”
That line is absolutely true, and I wrote it in the hopes that it will sell a few extra copies of the book — that, after all, is what blurbs are for. But more than that, it was the message I wanted to get to Roger. “The Boys of Summer” might not be the best book I have read, just like “The Princess Bride” might not be the best movie I have seen and spaghetti and meatballs might not be the best meal I have had and Stevie Wonder’s “Sir Duke” might not be the best song I have heard and chocolate cake might not be the best dessert I’ve eaten.
But it is, to me, the most perfect book, just as the rest are the most perfect examples of joy.
Roger Kahn was a longtime writer and author, not only of baseball. Through the years, he wrote eloquently about the poet Robert Frost, the violin virtuoso Jascha Heifetz, the politician Eugene McCarthy and the writer John Lardner. He wrote about his son Roger Jr.’s struggle with heroin addiction and death by suicide. He wrote being Jewish in America, and he wrote about the roaring twenties when Jack Dempsey reigned, and he wrote a not-very-good novel version of his own troubled second marriage.
Baseball, though, is what brought out his writing soul. He wrote about the artistry of pitching. He wrote about the dreams of minor leaguers. He wrote about how Jackie Robinson changed everything. He also wrote a book with Pete Rose — “Pete Rose: My Story” — that he and Rose each came to regret for any number of reasons; that was not a particularly good match.
Mostly, he wrote “The Boys of Summer.” It was simply unlike any other sports book that had ever been written and, I would argue, unlike any sports book written since. The core idea came from his own unlikely and sudden rise from rabid Brooklyn Dodgers fan to Brooklyn Dodgers beat writer just as the team jelled into something magical and cursed. He was 24 years old when he was assigned to cover the Dodgers for the New York Herald.
And that team was inarguably wonderful — Jackie, Pee Wee, Preacher, Shotgun Shuba, Campy and the Duke. That team won pennants in ’52 and ’53. That team also lost the World Series both times to the inevitable New York Yankees.
In the first half of the book, Kahn writes about his own journey. “At a point in my life,” he begins, “when one is through with boyhood, but has not yet discovered how to be a man, it was my fortune to travel with the most marvelously appealing of teams.”
In the second half of the book, Kahn went back to find them, those Boys of Summer, now 20 years older and trying to make their way in life.
When he first tried to sell “The Boys of Summer,” Kahn found no buyers. Who would want to buy a book about an old baseball team that wasn’t even good enough to win? And Kahn tried to explain to them that it was the fact those Dodgers did not win that made them so captivating.
“Their skills,” Roger wrote, “lifted everyman’s existence, a national team, with a country in thrall, irresistible and unable to beat the Yankees.”
But, beyond that, I’ve always believed that the book wasn’t about the Dodgers or winning or losing or any of that. Well, it was about those things, but it was about more. In between the journey and the return, he wrote the most beautiful and haunting part. He wrote about burying his father, Gordon Kahn.
More than anything, this was a book about fathers and sons and the space in between.
“Outside, the summer sun was taunting,” he wrote of that moment after he chose his father’s coffin. “I walked to the car, a lawyer at each elbow, wholly alone. The wrongness of things seized me. At the Parade Grounds, boys were throwing footballs. It was that season; baseball would come again. The team was broken up and with my father dead there was no one with whom I wanted to consider that tragedy, and because there was no one I recognized that the breaking of a team was not like greater tragedy: incompleteness, unspoken words, unmade music, withheld love, the failure ever to sum up or say good-bye.”
I cannot tell you how many times I have read that paragraph. And it hits me every single time.
Of course, I was hardly the only person of my generation to read “The Boys of Summer” and have it alter the course of my life.
One of the three (well, four if you count the paperback) copies of “The Boys of Summer” in this house is the original one, the one I first read. It is dog-eared and tattered and it has food stains everywhere. The spine has broken, the words on the cover faded long ago, and I believe there’s an entire section from the middle missing.
Yes, I often think about throwing it away — I do have other copies, after all — but so far I keep it because it reminds me of the first time I read it, the way the words rattled my brain and launched my own hopes. I see it, and I think about a poem that Kahn quotes in the book by Saigyō, a poem I committed to memory all those years ago and have never forgotten.
Did I ever dream
I should pass this way again
As an old man?
I have lived such a long time — 
Nakayama of the Night.
When I heard Roger died, for some reason, my mind immediately traveled to the chapter he wrote about George “Shotgun” Shuba, a relatively minor member of those Boys of Summer. Shuba was a good hitter who was famously (and self-consciously) sketchy in the field. This was 20 years after the Boys of Summer, they were both middle-aged men, and Kahn was praising Shuba for being such a natural hitter. Shuba was unimpressed by the analysis. “Ah,” he said, “you talk like a sportswriter.”
Then he took Kahn to the basement, where he had baseball bats filled with lead. He’d swing those heavy bats again and again, 600 times a night, 4,200 times a week, 47,200 swings every winter.
“You call that natural?” Shuba said.
“I wish I’d known this years ago,” Kahn said. “It would have helped my own hitting.”
“Aah,” Shuba said in the stuffy cellar. “Don’t let yourself think like that. The fastball is by the both of us. Leave it to the younger guys.”
                   ")

Joe_Posnanski_df <- data_frame(line=48:80, text=Joe_Posnanski)


Daniel_Popper <- c("
NFL Game Pass crashes for international fans during final three minutes of Super Bowl
NFL Game Pass customers in multiple countries complained of their live video feeds crashing several times Sunday during the Super Bowl, including for the final three minutes, when the 49ers and Chiefs were locked in a one-score game.
Three international Game Pass subscribers — Matti Liljaniemi, who lives in London; Lukas Pistrol, who lives in Austria; and Tiarnan Millar, who lives in Ireland — separately told The Athletic that the video feed cut out not long after Kansas City running back Damien Williams scored the go-ahead touchdown with 2:44 remaining. In the early hours of the morning in their respective European counties, all three fans were unable to watch the crucial closing minutes of the game live on the NFL’s official streaming service. Pistrol and Millar had paid the equivalent of $180 for a yearly Game Pass subscription. Liljaniemi had paid $270.
The NFL has yet to release an official statement addressing the malfunction that appears to have affected a sizable swath of its international audience during the biggest game of the season, nor has the league publicly apologized to those fans. Game Pass customer service did send out a generic copy-and-paste email Thursday to those who filed complaints about the feed crash. “We are aware that many of our fans experienced issues watching Super Bowl LIV on NFL Game Pass on Sunday, and apologize unreservedly for this service outage,” the email read, according to three screenshots. “We’re disappointed this happened and are currently investigating the reason for the outage in order to ensure it cannot happen again. We do not take your fandom for granted, and appreciate all your support as fans of the NFL this season. We will share more information directly with you within the next 72 hours around how we will make amends for this issue.”
Millar said in a message that he’s sent 10 emails to the Game Pass support team, asking for a refund each time. His request has not been addressed in any of the responses. As of Friday morning, Liljaniemi said there had been no specific mention of refunds.
The Athletic has sent multiple requests for comment to the NFL. The first was sent Tuesday morning. After a follow-up Wednesday, a league official said there was no update. Two other league officials did not respond to a request for comment.
The Game Pass streaming technology is provided by Deltatre, a company overseen by Bruin Sports Capital and OverTier, a joint venture between Bruin Sports Capital and British advertising giant WPP. The Athletic also sought comment from Bruin Sports Capital, but that request was not answered. OverTier took over full operation of NFL Game Pass in 181 countries worldwide starting in October. (Editor’s note: Bruin Sports is an investor in Courtside Ventures, which has a stake in The Athletic.)
Game Pass subscribers first started experiencing issues Sunday during the first quarter, according to Liljaniemi and Pistrol, “right after the first drive.” Millar said his feed first crashed “about two minutes into the broadcast.”
“The stream crashed and wouldn’t open again,” Pistrol said in a message. “The app/website showed an error message. Something like ‘Network error: Check your network,’ ‘You are not entitled to watch this,’ and ‘This stream is currently not available.’ Always a different error description each (time).”
Liljaniemi said the initial crash at the beginning lasted “about 20 minutes.” He sent a complaint to customer support. The response included, according to screenshots: “We apologise for the technical problems that you’ve experienced during the live game. The issue was addressed immediately by our technical provider and it was resolved within a few minutes.”
It also said customers would not be receiving refunds.
Then came the second crash that prevented subscribers from watching the conclusion of the Super Bowl on Game Pass.
Pistrol said he was able to switch to different streaming service, DAZN, which provided the tech for Game Pass before OverTier took over.
                   ")

Daniel_Popper_df <- data_frame(line=87:99, text=Daniel_Popper)

Johnson_Twomey <- c("
Inside Chelsea: The clock continues to tick on the redevelopment of Stamford Bridge – will it ever happen?
Three years ago, the project team responsible for the proposed redevelopment of Stamford Bridge left Hammersmith Town Hall on King Street in jubilant mood and headed across the road to The Salutation, the nearest pub. Having just secured unanimous council approval for the construction of a new 60,000-seater stadium inspired by Westminster Abbey and dubbed the “Cathedral of Football”, they understandably felt like celebrating a big step forward in what promised to be one of the most transformative events in Chelsea’s history.
Instead, that momentous evening in January 2017 is at real risk of going down as a monumental false dawn. Planning approval from the council for the new Stamford Bridge, subsequently endorsed by Mayor of London Sadiq Khan, expires on March 31 and cannot be extended. With less than two months to go until the deadline, the first phase of the project — demolition of the buildings around the stadium, including the Millennium and Copthorne Hotels — has not begun.
It is understood that Chelsea don’t have to physically begin working on the site before March 31. They can issue a pre-commencement application, notifying what labour is being carried out and when. As a source told The Athletic: “It is not necessarily a case of Chelsea knocking through a wall — they can file paperwork with the intention of knocking down the wall and when.” But if no communication is received by the deadline, the application process must be started all over again.
There is, as yet, no indication that Chelsea will resume the project, paused with an abrupt 49-word statement on the club’s official website in May 2018 that cited an “unfavourable investment climate”, in time to meet the planning approval deadline. Club officials insist that if it is allowed to lapse, they are confident that a second application would not prove quite as onerous as the first one. Other obstacles — most notably a “right to light” dispute with a local family affected by the proposed redevelopment — have also been overcome.
But even the revised schedule, disclosed a few months before the project was officially shelved, had Chelsea moving into their new stadium no earlier than the start of the 2024-25 season after spending as many as four years in a temporary home. With no demolition taking place, let alone the first spade in the ground, it is reasonable to question whether the new Stamford Bridge will be realised before the decade we have just entered is over — or even if it will happen at all.
Chelsea’s decision to halt the redevelopment came amid owner Roman Abramovich’s stand-off with the UK government over an extension to his Tier 1 investor visa. Contrary to noise on social media around the start of the year, sources have told The Athletic that he has not applied for a new one since withdrawing his application in the spring of 2018, and those familiar with the Russian have asked why he would fund a lavishly expensive construction project that he is not welcome to visit.
Questions about Abramovich’s broader commitment to owning Chelsea were answered emphatically by the revelation that he pumped £247 million of his personal wealth into the club during the last financial year, coupled with his move to use the club’s global platform as part of his broader efforts to eradicate antisemitism. Legitimate questions remain, however, as to just how much of the Stamford Bridge redevelopment he is prepared to pay for with his own money.
Chelsea held talks with several investment banks in 2017 about the possibility of borrowing £500 million to fund their new stadium and the estimated overall cost of the project had ballooned to around £1 billion before it was paused. Last summer, a report by New Civil Engineer claimed that the club had instructed the design team to cut costs by as much as £500 million before the project could be resumed, and were even open to the idea of building on an alternative site.
Chelsea pushed back strongly against the suggestion that a permanent departure from Stamford Bridge was being considered, insisting that they remain committed to keeping the club in its historic home on Fulham Road for the long term. Eight years have passed since the club were outbid by two Malaysian companies for Battersea Power Station, and west London is far from flush with plots of empty land or vacant property big enough to accommodate an elite modern football stadium.
There is also the fact that Chelsea cannot permanently relocate anywhere without the agreement of Chelsea Pitch Owners (CPO), the supporter-led group which has owned the freehold to Stamford Bridge since 1997. The club failed in an attempt to buy back the land from CPO at the direction of Abramovich in October 2011, falling well short of a required 75 per cent majority. There is no desire to revisit an episode that sparked considerable hostility from fans, as well as accusations that those acting on the club’s behalf had purchased shares to try to sway the vote in their favour.
CPO remain adamant that they will not support any attempt to take Chelsea away from Stamford Bridge permanently. The club have also moved to strengthen ties with the group since 2011 and the two parties now enjoy a good working relationship. Details of how to become a CPO shareholder can be found on Chelsea’s official website, while supporters who take tours of the stadium can purchase shares in the museum reception, as well as at a variety of events. There are now around 13,000 CPO shareholders globally, owning approximately 21,000 shares in total.
But while Chelsea may be committed to Stamford Bridge — by choice and by circumstance — for the long term, the design challenges of the project are at a different level of complexity to those faced by Arsenal or Tottenham with their new stadiums. Bordered by train lines on two sides, building out to accommodate 18,000 extra seats was not an option. Plans for the new Stamford Bridge required digging down, resulting in a pitch below ground level, and building up, including a raised walkway over the railway lines to reduce the number of fans accessing the ground from Fulham Road.
The proposed demolition and construction phases present a myriad of logistical difficulties. Chelsea officials have privately expressed concerns about ease of access for heavy machinery and large trade vehicles to the stadium site. Building the walkways over the railway lines would need to be accomplished without disrupting National Rail or Transport for London services. Vibration levels from heavy plant machinery must also be limited to avoid disrupting the catacombs in Brompton Cemetery.
When the full nature of the undertaking is considered, Chelsea’s estimate that they would need to spend four full years elsewhere — more than double the length of time that Tottenham spent as tenants of Wembley — begins to seem almost optimistic. There are legitimate fears that the club’s supporters may not have the stomach for such a prolonged exile, and it is also likely that their absence would have a profound impact on the area surrounding Stamford Bridge.
Hammersmith and Fulham Council are convinced that a redeveloped Stamford Bridge would be a significant long-term benefit to the borough but many businesses nearby fear the more immediate pain caused by the project. Many of the pubs within walking distance of the stadium estimate that between 25 and 30 per cent of their overall revenue comes from match-days. “If they did move, most of the pubs around here would end up closing,” admits Scott Kirwan, owner of the Broadway Bar and Grill on Fulham Road.
Business rates and rents in the borough are high even by London standards and when the stadium redevelopment was first approved in 2017, there was no indication from the council of reductions while Chelsea would be away. “I know that when Tottenham’s new stadium opened, pubs in the area increased their trade by 3000 per cent,” Kirwan adds, though others are less convinced of the benefits. “How much better can it be though?” asks David Nahmad, GM of the Tommy Tucker. “We’re fully booked on match days, with a waiting list as well.”
But the successful transition that Tottenham have made to a state-of-the-art home looms large over Chelsea. Many at Stamford Bridge are bracing themselves for some grim reading when their bitter London rivals publish their latest financial results in the coming weeks, bolstered for the first time by the increased match-day revenue made possible by their shiny new stadium.
“Match days are probably where clubs can make the most difference financially, especially going forward,” Kieran Maguire, Football Finance lecturer at Liverpool University, tells The Athletic. “If you look to see what Spurs have done, going from a 35,000 capacity stadium to one that fits 62,000, they will be able to increase their match-day income from around about £35-40 million (per season) to close to £100 million. That will make a significant impact.
“At present, Chelsea are reliant on Abramovich. There is no questioning his generosity — he is still putting the money in — but they’re also reliant on player sales to help them get through Financial Fair Play [FFP]. With FIFA restricting the amount of players going out on loan and subsequently being sold, Chelsea could be hit quite hard because that has been part of their business model.
“Unless they can generate more match-day revenue, there is going to be a gap: Arsenal are 60,000 capacity; West Ham are 60,000; Manchester United are 75,000; Manchester City are 55,000 going to 60,000; Liverpool are 54,000 and are planning to take it to 61,000. Chelsea are sticking out like a sore thumb.
“If Chelsea are able to redevelop Stamford Bridge to make it more competitive, they will be able to offer more add-ons, such as we are seeing at Spurs, which is a multi-functional stadium. Supporters are prepared to spend four hours there. Obviously, the longer you stay in the stadium, the more money they can make off you. Spurs have been really smart in the route they’ve taken and that will make a financial difference to them.”
Chelsea posted a pre-tax loss of £101.8 million in January, their largest since 2005. The results covered a year in which the club won the Europa League, and their stagnant match-day revenue failed to paper over the cracks caused by the absence of Champions League participation. The financial pressure on Frank Lampard to secure fourth spot this season is considerable.
“Without the benefits of a bigger ground, it makes finishing in the top four more vital for Chelsea,” Maguire adds. “The way that UEFA split the prize money means roughly 80 per cent goes to the Champions League clubs and 20 per cent to the Europa League. Chelsea and Liverpool both won their respective competitions last season but Chelsea earned just €39 million for the Europa League and Liverpool got €107 million.
“Liverpool had a further advantage though because they could charge a higher price for match day last season because of the superior opposition. Everything racks up. The difference between making and not making it can be £80-100 million when you factor everything in.”
The “unfavourable investment climate” cited by Chelsea when halting the stadium project was also impacted by the prolonged political and financial uncertainty that followed the Brexit vote in 2016. Now that the UK has officially left the European Union, there is at least a measure of clarity to the situation but Maguire does not expect recent events to do much to limit the already-spiralling costs of such an expansive project.
“The price has gone up to an extent because the pound has fallen in value since the Brexit vote, although there has been a recovery in the last few months,” he explains. “If you look at the cost of building anything, it’s a combination of material, labour and overheads. It could be a case that raw material prices will go up if originally importing from the European Union and now there are going to be complications in terms of logistics and supply line.
“It’s fair to say the construction cost will rise because there is certainly evidence that construction workers from the EU are returning there, so that will push up domestic prices for staff. Overheads in general will equally rise because if goods are being imported and there is not a free trade deal, then those costs will be passed on to the buyer, which in this case will be the football club.
“I don’t think it will be a deal-breaker, though. It’s just not going to help on the cost front.”
The key to Chelsea resuming the stadium project post-Brexit could well be Abramovich securing more favourable terms on bank loans, or partnering with outside investors.
“Chelsea will be looking for some third-party funding,” Maguire says. “If you look at the Spurs deal, what they’ve been smart in doing is borrowing money on a very long-term interest-only mortgage of about 2.5 per cent. So let’s say Chelsea’s stadium costs £1.5 billion, of which Abramovich puts up half and the banks put up the other half — the interest cost per year at 2.5 per cent will work out at £17-18 million a year.
“That’s nothing compared to the additional match-day revenue you hope to generate and also, Chelsea will be able to potentially get extra hospitality income and start to pitch for events that Wembley and Tottenham go for, like outdoor concerts and the NFL. As for spending four years away, Spurs covered that relatively easily when they played at Wembley. West Ham pay £2.5-3 million to rent the London Stadium. That’s not going to cause Abramovich to blink.”
The problem for Chelsea is that their stadium redevelopment has become something of a staring contest. If next month’s planning approval deadline is allowed to lapse, the prospect of a new elite stadium fit for a modern European giant will recede beyond the horizon — and until it is brought firmly back into view, Chelsea are a club on hold.
                    ")

Johnson_Twomey_df <- data_frame(line=105:137, text=Johnson_Twomey)

Bill_Shea <- c("
Michael Redd keenly remembers the night he put up 45 points on Kobe Bryant and the Lakers.
It was November 2006 at Staples Center in Los Angeles. Redd, the silky left-handed sharp-shooting guard out of Ohio State, had finally come into his own with the Bucks, who had taken him in the second round of the 2000 NBA Draft and only to put him on the bench behind Ray Allen.
By this night, Allen was long gone, and Redd had developed into a star himself. Just a couple weeks earlier, he had gashed the Jazz for what would be a career-high 57 points.
Redd would score 18 points in the fourth quarter to help fend off the Lakers, who were mired in an interregnum between Bryant’s first three and final two NBA titles. Despite Redd’s abilities, the Bucks remained a mostly mediocre franchise during his career.
Hence, beating Bryant was memorable.
“That was the best game of my career. He demanded the best from you. Whenever he played you, it was sink or swim. He was a tremendous defensive player at the time. He demanded you bring your absolute best. He was the standard in the NBA. I was laser locked in. He inspired me, really, to take it seriously,” Redd said. “He brought the best out in me.”
For Redd, who spoke by phone about the death of his friend Bryant and about his own growing post-NBA career as a venture capitalist, that night in 2006 was one of three times in 18 head-to-head meetings he would ever beat the man who nicknamed himself “the Black Mamba.”
It would be in 2008 that Redd and Bryant became close, or at least as close as the single-minded and often mercurial and ultra-competitive Bryant would allow an opposing player to become. That was the year they were both on the Team USA squad that won the gold medal at the Summer Olympics in Beijing.
“He was much more walled-off back then. I have the privilege of being accepted by him. If you were welcomed in his circle, it was a special thing,” Redd said. “We spent a lot of time together, talking about life, family, spirituality. We had really, really cool, interesting conversations, I had a chance to know this guy, and he was brilliant in the way he thinks, away from the jersey.”
It was in China that a legendary pop-a-shot game happened. While living at the Olympic village as teammates, Redd and Bryant ended up competing in an arcade basketball game – for hours, both eventually pouring sweat, according to an account from Team USA teammate Chris Bosh.
“I won. If he were here, he would say he won,” Redd said, chuckling. “We were just ultra-competitive. He discovered that about me. That was a unique time. That’s who he was. He was not going to lose. I refused to lose. He had a competitive nature I had never seen before.”
Redd said he and Bryant would continue to talk over the years after their time in China. Bryant had become a successful businessman and storyteller, launching a $100 million venture capital fund months after his 2016 retirement. And there was even some discussion of possibly doing some business together.
That possibility ended when Bryant, 41, was killed along with 13-year-old daughter Gianna and seven others in a helicopter crash in California on Jan. 26.
“I had a conversation last summer with Kobe about potential opportunities, but it never happened,” he said. “It’s heartbreaking it will never happen.”
Redd was wistful about losing his friend.
“We just gravitated to each other. He’s going to be missed,” Redd said.
Bryant entered the NBA as a teenager out of high school in 1996, so while he was a year older than Redd – their birthdays are one day apart – he had been in the league for four full seasons when Redd was drafted.
The accident blindsided Redd.
“I got the notification on my phone, like most people. I thought it was a hoax at first. Like the rest of the world, I was shocked. I think it’s still a shock to all of us. I think it will bring a lot of guys closer together,” he said.
While still processing the shock, Redd continues with his second career. His professional basketball life slowed dramatically after a series of major knee injuries in 2009-10 sidelined him. He retired in 2013, spending a final season with the Suns after 11 in Milwaukee, with 11,972 career points, an average of 19 per game.
His abilities earned him a pretty good payday: He earned $101.9 million in career pretax NBA salary, according to data from Basketball-Reference.com. Redd is using some of that wealth to become a venture capitalist.
He launched his own investment company not long after retirement, and joined the $50 million ADvantage Sports Tech Fund as a venture partner last month. The early-stage investment fund was jointly started in 2018 by Berlin-based leAD Sports, an investment firm run by the family that launched Adidas, and by Israel-based OurCrowd, which bills itself as an platform built for accredited investors and institutions to invest in startups and venture funds.
Redd will work out of his native Columbus, Ohio, where he still lives in the suburbs, with a focus on sports and data companies, particularly those focused on health, wellness and recovery for athletes. His role will include mentorship and advice for the investment companies and using his deep network built from his NBA and Olympic careers to help the firms make useful connections.
“They felt that I could add value for the fund. For me, as a former athlete, I completely understand the need for sports tech, so it was a no brainer to join,” Redd said.
Jeremy Pressman, a partner at ADvantage and basketball player himself, cited Redd’s resume for the decision to have him join the firm.
“Through his athletic achievements and experience over the last six years in venture investing, Michael has built an extensive global network and deep expertise in the business of sports and technology,” he said in a statement. “We’re excited to be working with him to identify and build out the next generation of great sports tech companies that can benefit from his unique perspectives.”
Redd isn’t a rookie at investing.
Over the past six years, he has put money into more than 85 companies with his own fund that he launched several years ago, he said. He typically invests in the five- to six-figure range in young startup companies that need early investors, called angel investors, and are seeking their first, or Series A, round of funding.
“Series A is always a good time to get in for me,” he said. “I won’t give a specific amount because it varies. I don’t put all my eggs in one basket. It depends on how close they are to an inflection point.”
In investing, an inflection point is a moment that a company makes a dramatic change, ideally toward profitability, expansion and exposure. They are usually a dramatic turn for a company and investors try to predict when such inflection points occur – although infection points can also be negative, such as a stock market crash or event that damages a company.
“It’s a matter of tolerance for risk,” Redd said, adding that diversifying his small investments is a common-sense shield against losing it all.
“Whenever I encourage others, I tell them to diversify as much as possible. And the risk is mitigated if you surround yourself with quality people.”
Because he puts his money mostly into newer companies still developing and bringing products to market, his investing hasn’t yet been profitable. He doesn’t expect payoffs until further into the future.
And he knows some investments will be lost, as is common in venture capital. His venture capital career so far has been educating himself about the business and learning when and when not to invest.
“It’s been fun, a great process to see companies grow, and a learning process to watch some fail,” he said. “If you’re engaged in business, you’re going to lose money at some point. Just don’t throw good money after bad.”
While it’s painful to see a company fail, it’s always an opportunity to learn for the next investment.
“I love to show people my scars more than my trophies,” he said.
Redd does expect to make money eventually, although his life isn’t contingent on investment income.
“I don’t need to make more money,” he said. “It’s a long play. That’s why your investment dollars don’t have to be massive. They can be small bets. It’s fun to continue to grow it and build. There are some (in his personal portfolio) fund that have a chance to be unicorns.”
A unicorn is a term for companies that have the potential to top $1 billion in value.
Another lesson he has learned as a budding venture capitalist is to know when to say no.
“I’ve said no more than I’ve said yes. That’s a huge weapon in your arsenal,” Redd said.
When not immersed in the financial world, Redd said he’s still an athlete and still works out hard – just not on the basketball court, apart from playing with his preteen son and the occasional pickup game. His intense basketball life is over.
“I don’t miss playing it at all,” he said. “I keep up to date with what’s going on. It was one chapter in my life, and I have moved onto the next chapter. It’s what I did, it wasn’t who I was. The NBA is an incredible springboard to the next phase of your life.”
And the knee that ended his career early?
  “My knee is good. It’s been 10 years since I tore it. I continue to work out, train. I make sure it’s strong enough to do things I want to do, so I can chase my kids around,” he said.
Redd and his wife, Achea, who is a mental health advocate, have two children — 12-year-old Michael II and 9-year-old Ardyn.
“I am very domestic these days. I am enjoying being a girl-dad and boy-dad,” Redd said. “I am enjoying my family now that I’m retired. Everything I am doing is centered around them. That’s my focus. Our family is all about how we can help people in society.”
Redd’s kids know their dad was an NBA player, mostly from YouTube clips. His son plays basketball, but there is no pressure to follow in his dad’s footsteps.
“I’m here to support him in whatever endeavor he wants to do,” he said. “I just want him to be a kid as long as he can.”
               ")

Bill_Shea_df <- data_frame(line=143:192, text=Bill_Shea)



The_Athletic <- bind_rows(mutate(Ken_Rosenthal_df, author="Ken Rosenthal"),
                          mutate(Joe_Posnanski_df, author= "Joe Posnanski"),
                          mutate(Daniel_Popper_df, author="Daniel Popper"),
                          mutate(Johnson_Twomey_df, author="Johnson Twomey"),
                          mutate(Bill_Shea_df, author="Bill Shea"))

my_junk <- data_frame(word=c("it's"), lexicon="junk")
The_Athletic_tokens <- The_Athletic %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(word, sort=T) 

#############################################
###### N-grams and tokenizing ###############
#############################################

The_Athletic_bigrams <- The_Athletic %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

The_Athletic_bigrams #We want to see the bigrams (words that appear together, "pairs")

The_Athletic_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
The_Athletic_separated <- The_Athletic_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

The_Athletic_filtered <- The_Athletic_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
The_Athletic_counts <- The_Athletic_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
The_Athletic_counts

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################

The_Athletic_quadrogram <- The_Athletic %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

The_Athletic_quadrogram

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

The_Athletic_united <- The_Athletic_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

The_Athletic_bigram_tf_idf <- The_Athletic_united %>%
  count(author, bigram) %>%
  bind_tf_idf(bigram, author, n) %>%
  arrange(desc(tf_idf))

The_Athletic_bigram_tf_idf

##### lets do the same for a quadrogram

The_Athletic_quadrogram_united <- The_Athletic_quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

The_Athletic_quadrogram_tf_idf <- The_Athletic_quadrogram_united %>%
  count(author, quadrogram) %>%
  bind_tf_idf(quadrogram, author, n) %>%
  arrange(desc(tf_idf))

The_Athletic_quadrogram_tf_idf

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

The_Athletic_bigram_graph <- The_Athletic_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()

The_Athletic_bigram_graph

ggraph(The_Athletic_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##########################################################
################ tf_idf Analysis#########################
##########################################################

tf_idf_athletic <- bind_rows(mutate(Ken_Rosenthal_df, author="Ken Rosenthal"), 
                             mutate(Joe_Posnanski_df, author= "Joe Posnanski"),
                             mutate(Daniel_Popper_df, author="Daniel Popper"), 
                             mutate(Johnson_Twomey_df, author="Johnson Twomey"),
                             mutate(Bill_Shea_df, author="Bill Shea")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  count(author, word, sort=TRUE) %>%
  ungroup()

total_words <- tf_idf_athletic %>%
  group_by(author) %>%
  summarize(total=sum(n))

athletic_words <- left_join(tf_idf_athletic, total_words)

print(athletic_words)

ggplot(athletic_words, aes(n/total, fill = author))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.1) +
  facet_wrap(~author, ncol=2, scales="free_y")

#############################################################

freq_by_rank <- athletic_words %>%
  group_by(author) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

# plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=author))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

athletic_words_idf <-athletic_words %>%
  bind_tf_idf(word, author, n) 

athletic_words_idf %>%
  arrange(desc(tf_idf))

athletic_words_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=author))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~author, ncol=2, scales="free")+
  coord_flip()

# emphysize the importance of words by reducing the weight of word frequency

athletic_sentiment <- bind_rows(mutate(Ken_Rosenthal_df, author="Ken Rosenthal"), 
                                mutate(Joe_Posnanski_df, author= "Joe Posnanski"),
                                mutate(Daniel_Popper_df, author="Daniel Popper"), 
                                mutate(Johnson_Twomey_df, author="Johnson Twomey"),
                                mutate(Bill_Shea_df, author="Bill Shea")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  mutate(linenumber=row_number())%>%
  inner_join(get_sentiments('nrc')) %>%
  count(author, index=linenumber, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

ggplot(athletic_sentiment, aes(index, sentiment, fill=author))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~author, ncol=2, scales='free_x')

Ken_Rosenthal_counts <- Ken_Rosenthal_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Ken_Rosenthal_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Joe_Posnanski_counts <- Joe_Posnanski_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Joe_Posnanski_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Daniel_Popper_counts <- Daniel_Popper_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Daniel_Popper_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Johnson_Twomey_counts <- Johnson_Twomey_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Johnson_Twomey_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Bill_Shea_counts <- Bill_Shea_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

Bill_Shea_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#################################################################
### WordCloud

cloud1 <- Ken_Rosenthal_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments('nrc')) %>%  #bing is positive/negative and nrc is more sentiments
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words=50, scale=c(0.8, 0.8),
                   title.size=1.5)

cloud2 <- Joe_Posnanski_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments('nrc')) %>%  #bing is positive/negative and nrc is more sentiments
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words=50, scale=c(0.8,0.8),
                   title.size=1.5)

cloud3 <- Daniel_Popper_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments('nrc')) %>%  #bing is positive/negative and nrc is more sentiments
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words=50, scale=c(0.8,0.8),
                   title.size=1.5)

cloud4 <- Johnson_Twomey_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments('nrc')) %>%  #bing is positive/negative and nrc is more sentiments
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words=50, scale=c(0.8,0.8),
                   title.size=1.5)

cloud5 <- Bill_Shea_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk) %>%
  inner_join(get_sentiments('nrc')) %>%  #bing is positive/negative and nrc is more sentiments
  count(word, sentiment, sort=TRUE) %>% #token per sentiment
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words=50, scale=c(0.8,0.8),
                   title.size=1.5)


