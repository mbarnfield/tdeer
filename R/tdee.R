#' @rdname tdee
#' @title A TDEE calculator for R.
#' @description Calculate your basal metabolic rate (BMR), total daily energy expenditure (TDEE), and calorie goals to lose, maintain, or gain weight. Also calculate adjusted values for every kg interval between your current and goal weight.
#' @param height Your height in centimetres. Returns a warning message to check unit of measurement if over 250 or under 100.
#' @param weight Your weight in kilograms. Returns a warning message to check units of measurement if over 250 or under 30.
#' @param age Your age in years. Returns a warning message to check units of measurement if over 100.
#' @param sex Your biological sex. Throws an error if not 'male' or 'female'.
#' @param activity Your activity level. Are you 'sedentary', or 'lightly', 'moderately', 'very' or 'extremely' active? Throws an error if not one of these options (do not include the word 'active', just the adverbs).
#' @param aim Your weight aim. Do you want to 'lose', 'maintain', or 'gain' weight? Throws an error if not one of these options
#' @param goal An optional argument specifying a goal weight. If specified, `tdee` runs calculations at each weight between your current and goal weight to show how calorie consumption needs to be adapted over time.
#' @return A dataframe with columns weight, bmr, tdee, and calories
#' @details \code{tdee} calculates your BMR, TDEE, and calorie requirements based on your height, weight, age, sex, activity levels and weight aims. The calculations are based on this post: https://steelfitusa.com/2018/10/calculate-tdee/
#' @import tidyverse
#' @export
#' @examples
#' # my own calculation
#' tdee(height = 182,
#'      weight = 84,
#'      age = 23,
#'      sex = "male",
#'      activity = "moderately",
#'      aim = "gain",
#'      goal = "89")
#' # alternatively without arg names
#' tdee(182, 84, 23, "male", "moderately", "gain", 89)
#' # a 150 cm, thirty-year-old woman weighing ninety kilograms, who is
#' # sedentary and wants to lose weight
#' tdee(height = 150,
#'      weight = 90,
#'      age = 30,
#'      sex = "female",
#'      activity = "sedentary",
#'      aim = "lose")
#' # plot adjustments towards goal weight
#' tdee_track <- tdee(182, 84, 23, "male", "moderately", "gain", 89)
#' ggplot(tdee_track, mapping = aes(x = weight, y = calories)) +
#'   geom_line() +
#'   geom_point()
#'

tdee <- function(height,
                 weight,
                 age,
                 sex,
                 activity,
                 aim,
                 goal = NULL) {

  if(height >= 250 | height <= 100) {
    warning("Make sure you have input your height in cm")
  }
  if(weight >= 250 | weight <= 30) {
    warning("Make sure you have input your weight in kg")
  }
  if(age >= 100) {
    warning("Make sure you have input your age in years")
  }
  if(sex != "male" & sex != "female") {
    stop("Sex must be either 'male' or 'female'")
  }
  if(activity != "sedentary" & activity != "lightly" & activity != "moderately"
     & activity != "very" & activity != "extremely") {
    stop("Activity must be one of: 'sedentary', 'lightly', 'moderately',
         'very', or 'extremely'")
  }
  if(aim != "gain" & aim != "maintain" & aim != "lose") {
    stop("Aim must be one of: 'gain', 'maintain' or 'lose'")
  }

  if(is.null(goal)) {
  #calculate basal metabolic rate
  bmr <- dplyr::case_when(
    sex == "female" ~
      655 + 9.6*weight + 1.8*height - 4.7*age,
    sex == "male" ~
      66 + 13.7*weight + 5*height - 6.8*age,
    TRUE ~ 0
  )

  #calculate total daily energy expenditure
  tdee <- dplyr::case_when(
    activity == "sedentary" ~ floor(bmr*1.2),
    activity == "lightly" ~ floor(bmr*1.375),
    activity == "moderately" ~ floor(bmr*1.55),
    activity == "very" ~ floor(bmr*1.725),
    activity == "extremely" ~ floor(bmr*1.9),
    TRUE ~ 0
  )

  #calculate calorie goals
  calories <- dplyr::case_when(
    aim == "gain" ~ tdee + 300,
    aim == "lose" ~ floor(tdee*0.8),
    aim == "maintain" ~ tdee,
    TRUE ~ 0
  )

  #dataframe of results
  data.frame(weight, bmr, tdee, calories)
  }

  else {

    # vector of weights from current to one away from goal
    one_away_gain <- goal-1
    one_away_lose <- goal+1

    if(aim == "gain") {
      kgs <- weight:one_away_gain
    }

    if(aim =="lose") {
      kgs <- weight:one_away_lose
    }

    #calculate basal metabolic rate
    bmrs <- dplyr::case_when(
      sex == "female" ~
        655 + 9.6*kgs + 1.8*height - 4.7*age,
      sex == "male" ~
        66 + 13.7*kgs + 5*height - 6.8*age,
      TRUE ~ 0
    )

    #calculate total daily energy expenditure
    tdees <- dplyr::case_when(
      activity == "sedentary" ~ floor(bmrs*1.2),
      activity == "lightly" ~ floor(bmrs*1.375),
      activity == "moderately" ~ floor(bmrs*1.55),
      activity == "very" ~ floor(bmrs*1.725),
      activity == "extremely" ~ floor(bmrs*1.9),
      TRUE ~ 0
    )

    #calculate calorie goals
    calories <- dplyr::case_when(
      aim == "gain" ~ tdees + 300,
      aim == "lose" ~ floor(tdees*0.8),
      TRUE ~ 0
    )

    #calculate maintenance at goal weight
    bmr_goal <- dplyr::case_when(
      sex == "female" ~
        655 + 9.6*goal + 1.8*height - 4.7*age,
      sex == "male" ~
        66 + 13.7*goal + 5*height - 6.8*age,
      TRUE ~ 0
    )

    tdee_goal <- dplyr::case_when(
      activity == "sedentary" ~ floor(bmr_goal*1.2),
      activity == "lightly" ~ floor(bmr_goal*1.375),
      activity == "moderately" ~ floor(bmr_goal*1.55),
      activity == "very" ~ floor(bmr_goal*1.725),
      activity == "extremely" ~ floor(bmr_goal*1.9),
      TRUE ~ 0
    )

    calories_goal <- tdee_goal

    weight <- c(kgs, goal)
    bmr <- c(bmrs, bmr_goal)
    tdee <- c(tdees, tdee_goal)
    calories <- c(calories, calories_goal)

    #dataframe of results
    data.frame(weight, bmr, tdee, calories)
  }
}

