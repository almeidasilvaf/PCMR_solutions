
#' Fit regression on phylogenetically independent contrasts (PICs)
#' 
#' @param tree An object of class \strong{phylo} with the tree.
#' @param data A data frame with species names in row names. Species
#' names must be the same as tip labels in \strong{tree}.
#' @param y Character specifying the column to be used as response variable.
#' @param x Character specifying the column to be used as predictor variable.
#' @param return Character indicating what to return. One of "plot" or
#' "model". Default: "plot".
#' 
#' @return A ggplot object with data and model fit along with model parameters.
#' 
#'
fit_pics <- function(tree, data, x, y, return = "plot") {
    
    # Get variables as named vectors
    var_x <- log(setNames(data[[x]], rownames(data)))
    var_y <- log(setNames(data[[y]], rownames(data)))
    
    # Compute PICs
    pic_x <- pic(var_x, tree)
    pic_y <- pic(var_y, tree)
    
    # Fit linear model to PICs without intercept
    pic_model <- summary(lm(pic_y ~ pic_x + 0))
    
    output <- pic_model
    if(return == "plot") {
        
        # Get estimates
        r2 <- signif(pic_model$r.squared, 1)
        slope <- signif(pic_model$coefficients[1], 2)
        p <- format(pic_model$coefficients[4], scientific = TRUE)
        
        subtitle <- bquote(beta[1] == .(slope) * ", " ~ R^2 == .(r2) * ", " ~ P == .(p))
        
        # Visualizing the model
        output <- data.frame(
            X = pic_x, 
            Y = pic_y
        ) |>
            ggplot(aes(x = X, y = Y)) +
            geom_point(size = 3, color = "deepskyblue4") +
            geom_vline(xintercept = 0, linetype = 3) +
            geom_hline(yintercept = 0, linetype = 3) +
            geom_smooth(method = "lm", se = FALSE, formula = "y ~ x+0") +
            labs(
                title = paste0("Regression of ", y, " on ", x),
                subtitle = subtitle,
                x = paste0("PICs for log(", x, ")"),
                y = paste0("PICs for log(", y, ")")
            ) +
            theme_classic()
    }
    
   return(output)
}