# make your own adventure
# based on http://boomeria.org/chemlectures/qual/macdonalds.jpg

# make story based on question and interactive choices, images (TODO)
make.story<-function(question,options,image=NULL,image.filepath=FALSE){
	HS<-list()
	HS$question<-question
	HS$options<-options
	HS$choice<-readline(paste(c(HS$question,"\n",paste(paste("#",1:length(HS$options)," ", HS$options,sep=""),collapse="\n"),"\n"),collapse=""))
	HS$message<-HS$options[as.numeric(HS$choice)]
	if(image.filepath==TRUE){HS$image<-paste('file://', file.path(getwd(),image), sep='')} else {HS$image<-image}
	return(HS)
}

#choose image
image.story<-function(input){
	return(switch(input,
			"travel" 									= "travel.png",
			"go to college" 							= "college.png",
			"stay at home and sponge off your parents" 	= "stayhome.png",
			"join the army" 							= "army.png",
			"become an entrepreneur" 					= "entrepreneur.png",
			"rock band" 								= "rockband.png",
			"burglary ring" 							= "burglary.png",
			"lawn care business" 						= "lawncare.png",
			"they croak" 								= "croak.png",
			"you get kicked out" 						= "kickedout.png",
			"they start to cramp your style" 			= "crampstyle.png",
			"Mexico" 									= "mexico.png",
			"Japan" 									= "japan.png",
			"Germany" 									= "germany.png",
			"business"									= "buisness.png",	
			"liberal arts" 								= "liberalarts.png",
			"get hired" 								= "rejected.png",
			"get rejected" 								= "rejected.png",
			"become broke" 								= "broke.png",
			"get lonely" 								= "lonely.png")	)	
					
}

# write story 
end.story<-function(story,end="The end.",image=NULL){
	
	if(is.null(image)){
		knit.story<-paste(paste(unlist(lapply(1:length(story),function(i){

				paste0(story[[i]]$question," ",story[[i]]$message,".","<br />","![image](",story[[i]]$image,")","<br />")
		})), collapse=" "),end)
	} else {
		knit.story<-paste(paste(unlist(lapply(1:length(story),function(i){

				paste0(story[[i]]$question," ",story[[i]]$message,".","<br />","![image](",story[[i]]$image,")","<br />")
		})), collapse=" "),end,"<br />","![image](",image,")")
	}
	
		#create .Rmd
		fileConn<-file("story.rmd")
		writeLines(knit.story, fileConn)
		close(fileConn)
		#create md and HTML
		require(knitr) # required for knitting from rmd to md
		require(markdown) # required for md to html 
		knit('story.rmd', 'story.md') # creates md file
		# knit2html('story.md', markdown.HTML.template = system.file("misc", "docco-template.html", package = "knitr"))
		markdownToHTML('story.md', 'story.html',stylesheet="custom.css") # alternative CSS system.file("misc", "docco-template.html", package = "knitr")
		browseURL(paste('file://', file.path(getwd(),'story.html'), sep=''))
	}
	
#interactively make story
interactive.story<-function(){
	story<-list()
	HS<-make.story("You go to high school, and",c("graduate", "drop out"),image="")
	if(HS$message=="graduate"){HS$image<-"graduate.png"} else {HS$image<-"dropout.png"}
	story<-c(story,list(HS))
if(HS$message=="graduate"){
		HS1<-make.story("After graduating you decide to",c("travel","go to college","stay at home and sponge off your parents"))
		HS2<-NULL
	
		HS1$image<-image.story(HS1$message)
		
			
		story<-c(story,list(HS1))
	} else {
		HS1<-NULL
		HS2<-make.story("You",c("join the army","become an entrepreneur","stay at home and sponge off your parents"))
		HS2$image<-image.story(HS2$message)
		story<-c(story,list(HS2))
	} 
	
if(is.null(HS1)){
	
	HS3<-switch(HS2$message,
		"join the army" 							= return(end.story(story,"You serve 4 years in Afghanistan and become addicted to heroine and an experimental 'Top secret' government drug. When you get back to the states you become a junkie who suffers daily PTSD induced hallucinations. This makes you perfectly qualified to work for Mcdonalds, inside of a Walmart!",image="mcdonalds3.png")),
		"become an entrepreneur" 					= make.story("You start a", c("rock band", "burglary ring", "lawn care business")),
		"stay at home and sponge off your parents" 	= make.story("You stay at your parents until", c("they croak", "you get kicked out", "they start to cramp your style")))
	HS4<-NULL
	
	HS3$image<-image.story(HS3$message)
					
	story<-c(story,list(HS3))
} else {
	
	HS4<-switch(HS1$message,
		"travel" 			 							= make.story("You move to",c("Mexico", "Japan", "Germany")),
		"stay at home and sponge off your parents" 	 	= make.story("You stay at your parents until", c("they croak", "you get kicked out", "they start to cramp your style")),
		"go to college"									= make.story("In college you major in", c("'science'", "business", "liberal arts")))
	HS3<-NULL
	
	if(HS4$message == "'science'"){HS4<-make.story("In college you major in 'science', but after meeting too many minimum wage paid PhDs, you quickly decide to switch to a more lucrative major. You switch majors to", c("business", "liberal arts"))}	
	
	HS4$image<-image.story(HS4$message)
	
	story<-c(story,list(HS4))
}	

if(is.null(HS3)){
		HS5<-switch(HS4$message,
			"Mexico" 							= make.story("You take your dad's old job which moved south and pays $3.00 a week until you", c("become broke", "get lonely")),
			"Japan" 							= make.story("You pay $75,000 a month for a studio apartment until you", c("become broke", "get lonely")),
			"Germany" 							= make.story("You give 3/4 of your pay check to taxes until you" , c("become broke", "get lonely")),
			"they croak" 						= make.story("This leaves no one to pay the bills, forcing you to start your own", c("rock band", "burglary ring", "lawn care business")),
			"you get kicked out" 				= make.story("Your parents remember to change the locks this time, forcing you to move out and start your own", c("rock band", "burglary ring", "lawn care business")),
			"they start to cramp your style" 	= make.story("This forces you to move out and start your own", c("rock band", "burglary ring", "lawn care business")),
			"business" 							= make.story("You learn what college Grads used to do before the recession. After 4 years you graduate and apply for a decent paying job which you think you", c("get hired", "get rejected")),
			"liberal arts" 						= make.story("You learn cleaver phrases to help you get dates. After 4 years of college and multiple STDs you graduate and apply for a decent paying job which you think you", c("get hired", "get rejected")))
		HS6<-NULL
		

		HS5$image<-image.story(HS5$message)
			
		story<-c(story,list(HS5))
		if(HS5$message == "get hired" | HS5$message == "get rejected") {
			return(end.story (story, "Don't kid yourself, of course you get rejected. This happens repeatedly until you get dejected and stop searching (removing yourself from the USA unemployment statics). Luckily your college degree perfectly qualifies for a manager position at at Mcdonalds, inside of a Walmart!",image="mcdonalds5.png"))}
		
		
		
	} else {
		HS5<-NULL
		HS6<-switch(HS3$message,
			"join the army" 					= return(end.story(story,"You serve 4 years in Afghanistan and become addicted to heroine and an experimental 'Top secret' government drug. When you get back to the states you become a junkie who suffers daily PTSD induced hallucinations. This makes you perfectly qualified to work for Mcdonalds, inside of a Walmart!",image="mcdonalds3.png")),
			"rock band" 						= return(end.story(story,"After which you get screwed by the record company, shattering all of your dreams of stardom and groupies. Because you have no real skills you have to get a job at Mcdonalds, inside of a Walmart!",image="mcdonalds4.png")),
			"burglary ring" 					= return(end.story(story,"On your first robbery attempt you try to mug an old lady who is also a Muay Thai kickboxer. She proceeds to kick you in the gut, shattering your spleen following which you pass out from the pain and are arrested. After a brief stay in the hospital, you spend the next 2 years in jail. Upon your release you have to disclose your felon status to future employers and are faced by crippling hospital bills. In desperation you go to work for Mcdonalds, inside of a Walmart!",image="mcdonalds.png")),
			"lawn care business" 				= return(end.story(story,"To maximize profits, you hire illegal immigrants. Your 12 year old competitor, Jimmy, rats you out to the fuzz and you are arrested. After spending 2 years in jail, upon your release you have to disclose your felon status to future employers, and the only job you can get is working for Mcdonalds, inside of a Walmart!",image="mcdonalds4.png")),
			"they croak" 						= make.story("This leaves no one to pay the bills, forcing you to start your own", c("rock band", "burglary ring", "lawn care business")),
			"you get kicked out" 				= make.story("Your parents remember to change the locks this time, forcing you to move out and start your own", c("rock band", "burglary ring", "lawn care business")),
			"they start to cramp your style" 	= make.story("This forces you to move out and start your own", c("rock band", "burglary ring", "lawn care business")))
		
		HS6$image<-image.story(HS6$message)
		
		story<-c(story,list(HS6))
	}
	
	if(is.null(HS5)){ 
			HS7<-switch(HS6$message,
				"rock band" 					= return(end.story(story,"After which you get screwed by the record company, shattering all of your dreams of stardom and groupies. Because you have no real skills you get a job at Mcdonalds, inside of a Walmart!",image="mcdonalds4.png")),
				"burglary ring" 				= return(end.story(story,"On your first robbery attempt you try to mug an old lady who is also a Muay Thai kickboxer. She proceeds to kick you in the gut, shattering your spleen following which you pass out from the pain and are arrested. After a brief stay in the hospital, you spend the next 2 years in jail. Upon your release you have to disclose your felon status to future employers and are faced by crippling hospital bills. In desperation you go to work for Mcdonalds, inside of a Walmart!",image="mcdonalds.png")),
				"lawn care business" 			= return(end.story(story,"Unbeknownst to you, you hire illegal immigrants, for which you are arrested and spend 2 years in jail. Upon your release you have to disclose your felon status to future employers, and the only job you can get is working for mcdonalds, inside of a Walmart!",image="mcdonalds4.png")))
			
				
			story<-c(story,list(HS7))
	} else {
	
			HS7<-switch(HS5$message,
			"become broke" 			= return(end.story(story, "Unable to speak the native language, you dejectedly wander the streets until you see the familiar golden arches and your only beacon of hope. Luckily only visiting Americans eat here, so language is not a problem. You end up getting a job at McDonalds (lucky for you Walmart is boycotted here).",image="mcdonalds2.png")),
			"get lonely" 			= return(end.story(story, "Unable to speak the native language, you dejectedly wander the streets until you see the familiar golden arches and your only beacon of hope. Luckily only visiting Americans eat here, so language is not a problem. You end up getting a job at McDonalds (lucky for you Walmart is boycotted here).",image="mcdonalds2.png")),
			"rock band" 			= return(end.story(story,"After which you get screwed by the record company, shattering all of your dreams of stardom and groupies. Because you have no real skills you get a job at Mcdonalds, inside of a Walmart!",image="mcdonalds4.png")),
			"burglary ring" 		= return(end.story(story,"On your first robbery attempt you try to mug an old lady who is also a Muay Thai kickboxer. She proceeds to kick you in the gut, shattering your spleen following which you pass out from the pain and are arrested. After a brief stay in the hospital, you spend the next 2 years in jail. Upon your release you have to disclose your felon status to future employers and are faced by crippling hospital bills. In desperation you go to work for Mcdonalds, inside of a Walmart!",image="mcdonalds.png")),
			"lawn care business" 	= return(end.story(story,"To maximize profits, you hire illegal immigrants. Your 12 year old competitor, Jimmy, rats you out to the fuzz and you are arrested. After spending 2 years in jail, upon your release you have to disclose your felon status to future employers, and the only job you can get is working for Mcdonalds, inside of a Walmart!",image="mcdonalds4.png")))
			
	}
}

##create story
# interactive.story()
	