#
# age_depth_model <- function(df, formula = age ~ depth + location + age_err,
#                             interpolate = approx, extrapolate = TRUE) {
#   formula <- stats::as.formula(formula)
#   response <- all.vars(formula[[2]])
#   terms <- setdiff(all.vars(formula[[3]]), "+")
#   if(length(response) != 1) stop("formula must be in the form age ~ depth + location")
#   if(length(terms) < 2) stop("formula must be in the form age ~ depth + location")
#
#   # assign vars for each col
#   age_col <- response
#   depth_col <- terms[1]
#   location_col <- terms[2]
#   age_err_col <- terms[3]
#   if(is.na(age_err_col)) {
#     age_err_col <- NULL
#     age_err_col_name <- NULL
#   } else if(!(age_err_col %in% names(df))) {
#     age_err_col <- NULL
#     age_err_col_name <- NULL
#   } else {
#     age_err_col_name <- "age_err"
#   }
#
#   # check for missing columns
#   missing_cols <- setdiff(c(age_col, depth_col, location_col, age_err_col), names(df))
#   if(length(missing_cols)) stop("df is missing required cols: ",
#                                 paste(missing_cols, collapse = ", "))
#
#   # subset and sort df
#   data <- df[c(location_col, depth_col, age_col, age_err_col)] %>%
#     as_tibble() %>%
#     setNames(c("location", "depth", "age", age_err_col_name)) %>%
#     arrange(location, depth, age)
#
#   # return list of data, interpolate, and extrapolate funs
#   structure(
#     list(
#       data = data,
#       interpolate = match.fun(interpolate),
#       extrapolate = extrapolate
#     ),
#     class = "adm"
#   )
# }
#
# predict.adm <- function(object, newdata) {
#   if(!("location" %in% names(newdata))) stop("newdata is missing column 'location'")
#   if(("depth" %in% names(newdata)) && ("age" %in% names(newdata))) {
#     stop("newdata contains both 'age' and 'depth': need exactly one")
#   }
#
#   # check all locations are in object$data$location
#   missing_locations <- setdiff(newdata$location, object$data$location)
#   if(length(missing_locations) > 0) warning("model is missing locations: ",
#                                             paste(missing_locations, collapse = ", "))
#
#   # save copy of original data
#   original_newdata <- newdata
#
#   # setup the transformation variables
#   if("depth" %in% names(newdata)) {
#     # predict age
#     predict_data <- object$data %>%
#       rename(location = location, x = depth, y = age)
#     newdata$.new_x <- newdata$depth
#     x_col <- "depth"
#     out_col <- "age"
#   } else if("age" %in% names(newdata)) {
#     # predict depth
#     predict_data <- object$data %>%
#       rename(location = location, x = age, y = depth)
#     newdata$.new_x <- newdata$age
#     x_col <- "age"
#     out_col <- "depth"
#   } else {
#     stop("newdata must have colunn 'depth' or 'age'")
#   }
#
#   # setup interpolator function
#   interpolator <- function(loc, x_out) {
#     pdata <- predict_data %>% filter(location == unique(loc))
#     # if location is missing, return NAs
#     if(nrow(pdata) == 0) return(rep_len(NA_real_, length(x_out)))
#     object$interpolate(pdata$x, pdata$y, x_out)$y
#   }
#
#   # do interpolation
#   out <- newdata %>%
#     ungroup() %>%
#     select(location, matches("age|depth"), .new_x) %>%
#     distinct() %>%
#     group_by(location) %>%
#     mutate(.new_y = interpolator(location, .new_x))
#
#   if(object$extrapolate && any(is.na(out$.new_y))) {
#     # fit models
#     ex_models <- predict_data %>%
#       group_by(location) %>%
#       summarise(exmodel = list(lm(y ~ x, data = .)),
#                 slope = purrr::map_dbl(exmodel, ~coefficients(.x)[2]),
#                 first_x = head(x, 1), first_y = head(y, 1),
#                 last_x = tail(x, 1), last_y = tail(y, 1))
#
#     # define extrapolator function
#     extrapolator <- function(loc, x_out) {
#       model <- ex_models %>% filter(location == unique(loc))
#       # if location is missing, return NAs
#       if(nrow(model) == 0) return(rep_len(NA_real_, length(x_out)))
#
#       with(
#         model,
#         if_else(
#           x_out > last_x,
#           last_y + slope * (x_out - last_x),
#           if_else(
#             x_out < first_x,
#             first_y + slope * (x_out - first_x),
#             NA_real_
#           )
#         )
#       )
#     }
#
#     # extrapolate NA values in new_y
#     out <- out %>%
#       group_by(location) %>%
#       mutate(.ext_y = extrapolator(location, .new_x)) %>%
#       mutate(.new_y = coalesce(.new_y, .ext_y))
#   }
#
#   # rename out column
#   out[[out_col]] <- out$.new_y
#   out <- out %>%
#     select(-.new_y, -matches("\\.ext_y"), -.new_x)
#
#   # if output is age, join the original data to get errors
#   if(("depth" %in% names(newdata)) && ("age_err" %in% names(object$data))) {
#     out <- out %>%
#       left_join(object$data[c("location", "age", "age_err")],
#                 by = c("location", "age"))
#   }
#
#   # join to newdata to ensure identical row ordering
#   left_join(original_newdata, out, by = c("location", x_col))
# }
#

