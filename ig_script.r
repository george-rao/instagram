# Group 28: Merritt Cozby, Anna Giesler, Will Jacobe, Andrew Lisi, George Rao
# This R script: ig_script.R
# Accompanying dataset: ig.csv



# Prepping up our code
library(mosaic)
ig = read.csv("ig.csv")



# 01: Added variables

# We're measuring likes as a proxy for positive reception of a post,
# so we decided to combine likes and comments for our main response variable.
ig$interactions = ig$likes + ig$comments + max(ig$views, 0)

# LikesPerFollower is a proxy for follower engagement, which also matters.
# Another measure of engagement: commentsPerLike.
ig$likesPerFollower = ig$likes / ig$followers
ig$commentsPerLike = ig$comments / ig$likes

# Why not, let's see how long their usernames and captions are.
ig$lengthAccount = nchar(ig$account)
ig$lengthCaption = nchar(ig$caption)

# Finally, a few more
ig$frames = ig$photos + ig$videos
ig$peopled = ig$people > 0



# 02: Notable plots

# A strong association between interactions and followers, as expected.
xyplot(log(interactions) ~ log(followers), data = ig, main = "Exhibit A")

# Strong association with posts, but they're confounded by followers.
xyplot(log(interactions) ~ log(posts), data = ig, main = "Exhibit B")
xyplot(log(posts) ~ log(followers), data = ig, main = "Exhibit C")

# Personal accounts have fewer interactions,
# but this seems to be confounded by followers.
bwplot(log(interactions) ~ personal, data = ig, main = "Exhibit D",
       xlab = "personal")
bwplot(log(followers) ~ personal, data = ig, main = "Exhibit E",
       xlab = "personal")

# On the other hand, personal accounts get much higher likesPerFollower,
# a proxy for follower engagement.
bwplot(likesPerFollower ~ personal, data = ig, main = "Exhibit F",
       xlab = "personal")

# LikesPerFollower also seems to be predicted by posts and followers.
# It makes sense - as you gain followers over time,
# followers later in your IG career tend to be less loyal.
# And the more you post, the less notable each post tends to be.
xyplot(log(likesPerFollower) ~ log(followers), data = ig, main = "Exhibit G")
xyplot(log(likesPerFollower) ~ log(posts), data = ig, main = "Exhibit H")



# 03: Notable models

# Even alone, followers predicts interactions with a high r-squared.
interactionsSingle = lm(log(interactions) ~ log(followers), data = ig)
summary(interactionsSingle)
interactionsSingleBoot = do(1000) * {
  lm(log(interactions) ~ log(followers), data = resample(ig))
}
confint(interactionsSingleBoot)

# Out of all our variables,
# only verified had any significant effect on the model.
# Moreover, being verified seems to decrease interactions,
# after adjusting for followers.
interactionsMulti = lm(log(interactions) ~ log(followers)
                + verified, data = ig)
summary(interactionsMulti)
interactionsMultiBoot = do(1000) * {
  lm(log(interactions) ~ log(followers)
     + verified, data = resample(ig))
}
confint(interactionsMultiBoot)

# Surprisingly, changing the response to likesPerFollower
# caused posts and personal to become good predictors in the model.
ratioMulti = lm(log(likesPerFollower) ~ log(followers) + log(posts)
                + personal + verified, data = ig)
summary(ratioMulti)
ratioMultiBoot = do(1000) * {
  lm(log(likesPerFollower) ~ log(followers) + log(posts)
     + verified + personal, data = resample(ig))
}
confint(ratioMultiBoot)



# 04: Summary code

# Simple linear regression for interactions
interactionsSingle
confint(interactionsSingleBoot)[c(2, 4), ]

# Multiple linear regression for interactions
interactionsMulti
confint(interactionsMultiBoot)[c(2, 3, 5), ]
confint(interactionsMultiBoot, level = 0.99)[3, ]

# Multiple linear regression for likesPerFollower
ratioMulti
confint(ratioMultiBoot)[c(2, 3, 4, 5, 7),]



# Just for fun: Predictive modeling

# set.seed(371)
# igErrors = do(10) * {
#   igShuffle = subset(shuffle(ig), select = (-orig.id))
#   trainSize = 0.8
#   nTrain = round(trainSize * nrow(igShuffle))
#   igTrain = igShuffle[1:nTrain,]
#   igTest = igShuffle[(nTrain + 1):nrow(igShuffle),]
# 
#   interactionsAll = lm(log(interactions) ~ . - id - collector - account
#                        - likes - views - comments - following - caption
#                        - self - interactions - likesPerFollower
#                        - commentsPerLike, data = ig)
#   interactionsStepBig = step(interactionsAll, trace = 0)
#   interactionsStepSmall = step(interactionsSingle, scope = interactions ~
#                              (log(posts) + log(followers)
#                              + tags + mentions + hashes + location
#                              + photos + videos + cover + sound + emoji
#                              + gender + age + verified + aspect
#                              + people + personal + lengthAccount
#                              + lengthCaption)^2, trace = 0)
# 
#   yHatInteractionsSingle = predict(interactionsSingle, newdata = igTest)
#   yHatInteractionsMulti = predict(interactionsMulti, newdata = igTest)
#   yHatInteractionsStepSmall = predict(interactionsStepSmall,
#                                       newdata = igTest)
#   yHatInteractionsStepBig = predict(interactionsStepBig, newdata = igTest)
# 
#   c(
#     rmspeInteractionsSingle =
#       sqrt(mean((log(igTest$interactions) - yHatInteractionsSingle)^2)),
#     rmpseInteractionsMulti =
#       sqrt(mean((log(igTest$interactions) - yHatInteractionsMulti)^2)),
#     rmpseInteractionsStepSmall =
#       sqrt(mean((log(igTest$interactions) - yHatInteractionsStepSmall)^2)),
#     rmpseInteractionsStepBig =
#       sqrt(mean((log(igTest$interactions) - yHatInteractionsStepBig)^2))
#     )
# }
# summary(igErrors)
