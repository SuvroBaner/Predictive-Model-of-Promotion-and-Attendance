# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)

install.packages("car")  # special functions for linear regression
install.packages("lattice") # graphics package

library(car)
library(lattice)

# read data and create a dataframe called dodgers

dodgers = read.csv("dodgers.csv")
dodgers[1:3,]

print(str(dodgers))  # check the structure of the dataframe.

### Define an ordered day-of-week variable for plots and data summaries.

attach(dodgers)

dodgers$ordered_day_of_week = with(data = dodgers,
	ifelse ((day_of_week == "Monday"), 1,
	ifelse ((day_of_week == "Tuesday"), 2,
	ifelse ((day_of_week == "Wednesday"), 3,
	ifelse ((day_of_week == "Thursday"), 4,
	ifelse ((day_of_week == "Friday"), 5,
	ifelse ((day_of_week == "Saturday"), 6, 7)))))))

dodgers$ordered_day_of_week = factor(dodgers$ordered_day_of_week, levels = 1:7,
	labels = c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun"))

# Exploratory data analysis with standard graphics: attendance by day of week

with(data = dodgers, plot(ordered_day_of_week, attend/1000,
xlab = "Day of Week",
ylab = "Attendance (thousands)",
col = "violet", las = 1))

# when do the Dodgers use bobblehead promotions the maximum
with(dodgers, table(bobblehead, ordered_day_of_week))  # it is on Tuesday, 6 times.

# From the box plot it can be also seen that on Tuesday the average attendance is more than any other day.

### Define an ordered month variable for plots and data summaries

dodgers$ordered_month = with(data = dodgers,
	ifelse ((month == "APR"), 4,
	ifelse ((month == "MAY"), 5,
	ifelse ((month == "JUN"), 6,
	ifelse ((month == "JUL"), 7,
	ifelse ((month == "AUG"), 8,
	ifelse ((month == "SEP"), 9, 10)))))))

dodgers$ordered_month = factor(dodgers$ordered_month, levels = 4:10,
	labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

# Exploratory data analysis with standard R graphics: attendance by month

with(data = dodgers, plot(ordered_month, attend/1000,
xlab = "Month",
ylab = "Attendance (thousands)",
col = "light blue", las = 1))

# Exploratory data analysis displaying many variables looking at attendance and conditioning
# on day/night the skies and whether or not fireworks are displayed.
# We'll be using the library lattice.

## Let us prepare a graphical summary of the dodgers data
# xyplot produces bivariate scatterplots or time-series plots

group.labels = c("No Fireworks", "Fireworks")
group.symbols = c(21, 24)
group.colors = c("black", "black")
group.fill = c("black", "red")

xyplot(attend/1000 ~ temp | skies + day_night,
	data = dodgers, groups = fireworks, pch = group.symbols,
	aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
	layout = c(2, 2), type = c("p", "g"),
	strip = strip.custom(strip.levels = TRUE, strip.names = FALSE, style = 1),
	xlab = "Temperature (Degrees Fahrenheit)",
	ylab = "Attendance (thousands)",
	key = list(space = "top",
		text = list(rev(group.labels), col = rev(group.colors)),
		points = list(pch = rev(group.symbols), col = rev(group.colors),
		fill = rev(group.fill))))

## Attendance by opponent and day/night game

# bwplot produces box-and-whisker plots

group.labels = c("Day", "Night")
group.symbols = c(1, 20)
group.symbols.size = c(2, 2.75)

bwplot(opponent ~ attend/1000,
	data = dodgers, groups = day_night,
	xlab = "Attendance (thousands)",
	panel = function(x, y, groups, subscripts, ...)
		{panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
		 panel.stripplot(x, y, groups = groups, subscripts = subscripts,
		 cex = group.symbols.size, pch = group.symbols, col = "darkblue")
		},
		key = list(space = "top", text = list(group.labels, col = "black"),
		points = list(pch = group.symbols, cex = group.symbols.size, col = "darkblue")))

# Employ training and test regimen for model validation.

set.seed(1234) # set seed for repeatability of training-and-test split.
training_test = c(rep(1, length = trunc((2/3)*nrow(dodgers))),
			rep(2, length = (nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))

dodgers$training_test = sample(training_test)  # random permutation

dodgers$training_test = factor(dodgers$training_test, levels = c(1, 2), labels = c("TRAIN", "TEST"))
dodgers.train = subset(dodgers, training_test == "TRAIN")
dodgers.test = subset(dodgers, training_test == "TEST")

# Specify a simple regression model-

train.model.fit = lm(attend ~ ordered_month + ordered_day_of_week + bobblehead, data = dodgers.train)
summary(train.model.fit)

dodgers.train$predict_attend = predict(train.model.fit)

dodgers.test$predict_attend = predict(train.model.fit, newdata = dodgers.test)

# Compute the proportion of response variance accounted for when predicting out-of-sample i.e. R-squared for test set.

cat("\n", "Proportion of Test Set Variance Accounted for: ",
	round((with(dodgers.test, cor(attend, predict_attend)^2)),
	digits = 3), "\n", sep = "")
# The R-squared value is 0.462

# Merge the training and test set sets for plotting-

dodgers.plotting.frame = rbind(dodgers.train, dodgers.test)  # row bind as opposed to cbind (column bind)


# Generate predictive modeling visual for the business.

group.labels = c("No Bobleheads", "Bobleheads")
group.symbols = c(21, 24)
group.colors = c("black", "black")
group.fill = c("black", "red")

xyplot(predict_attend/1000 ~ attend/1000 | training_test,
	data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
	pch = group.symbols, col = group.colors, fill = group.fill,
	layout = c(2, 1), xlim = c(20, 65), ylim = c(20, 65),
	aspect = 1, type = c("p", "g"),
	panel = function(x, y, ...)
			{panel.xyplot(x, y, ...)
			 panel.segments(25, 25, 60, 60, col = "black", cex = 2)
			},
	strip = function(...) strip.default(..., style = 1),
	xlab = "Actual Attendance (thousands)",
	ylab = "Predicted Attendance (thousands)",
	key = list(space = "top",
		text = list(rev(group.labels), col = rev(group.colors)),
		points = list(pch = rev(group.symbols),
		col = rev(group.colors),
		fill = rev(group.fill))))

### Use the full data set to obtain an estimate of increase in attendance due to bobbleheads, controlling for other factors

my.model.fit = lm(attend ~ ordered_month + ordered_day_of_week + bobblehead, data = dodgers)
summary(my.model.fit)

# Tests statistical significance of the bobblehead promotion.
# Type I anova computes sums of squares for sequential tests.

anova(my.model.fit)

cat("\n", "Estimated Effect of Bobblehead promotion on attendance: ",
	round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
	digits = 0), "\n", sep = "")

plot(my.model.fit)

residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))
