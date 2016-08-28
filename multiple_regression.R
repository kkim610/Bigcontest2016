# multiple regression

movieDatas = read.csv("movie_data.csv", stringsAsFactors = FALSE)

movieDatas$cnt_vote = as.numeric(movieDatas$cnt_vote)
movieDatas$cnt_review_before = as.numeric(movieDatas$cnt_review_before)
movieDatas$cnt_review_after = as.numeric(movieDatas$cnt_review_after)
movieDatas$total = as.numeric(movieDatas$total)

str(movieDatas)
summary(movieDatas)

cntDatas = nrow(movieDatas)
idx = sample(cntDatas, round(cntDatas * 0.7))
trainDatas = movieDatas[idx, ]
validateDatas = movieDatas[-idx, ]

model <- lm(total~ cnt_vote + cnt_review_before + cnt_review_after + director_weight + distributor_weight, data = trainDatas)
summary(model)
model2 <- lm(total~ cnt_vote + cnt_review_before + cnt_review_after + director_weight, data = trainDatas)
summary(model2)

result = predict(model2, validateDatas, interval = "prediction")
result[result < 0] = 0
rmse = sqrt(sum((result[, "fit"] - validateDatas$total)^2, na.rm = TRUE) / nrow(validateDatas))


