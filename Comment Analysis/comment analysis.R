library("RODBC")
library("plyr")
library("stringr")

odbcChannel <- odbcConnect("trademe")

job <- sqlQuery(odbcChannel, "SQL code ??????")

pos <- scan('TM-file??????/positive-words.txt', what='character', comment.char=';')
neg <-scan('TM-file??????/negative-words.txt', what='character', comment.char=';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

analysis <- score.sentiment(job$Message, pos, neg)

#job_score <- sqldf("SELECT * FROM job a inner join analysis b on a.Message= b.text")

job$score <- analysis$score

write.csv(job, file = "FrendFeedback.csv")