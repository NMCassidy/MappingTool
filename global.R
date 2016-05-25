##Global file
##Would be able to undertake the data loading and data cleaning in this portion
#might just source the data cleaning file form shared drive (although this would be slower)

#Load Functions -------------------------------------------------------
#Load Nice Cuts function
niceCuts <- function(variable, cuts = 10, thousands.separator = FALSE) {
  
  # Load required packages (useful when used independently of context)
  Vectorize(require)(package = c("gsubfn", "Hmisc", "scales"),
                     character.only = TRUE)
  
  # Destring this variable
  destring <- function(x) {
    ## convert factor to strings
    if (is.character(x)) {
      as.numeric(x)
    } else if (is.factor(x)) {
      as.numeric(levels(x))[x]
    } else if (is.numeric(x)) {
      x
    } else {
      stop(c(head(x), " - could not convert to numeric"))
    }
  }
  
  # Apply function
  variable <- destring(variable)
  
  # Check whether to disable scientific notation
  if (mean(variable, na.rm = TRUE) > 100) {
    options(scipen = 999)
  } else {
    options(scipen = 0)
  }
  
  # Create pretty breaks
  cut_breaks <- pretty_breaks(n = cuts)(variable)
  
  # Round it two decimal places
  variable <- round(variable, digits = 2)
  
  # Develop cuts according to the provided object
  cuts_variable <- cut2(x = variable, cuts = cut_breaks)
  
  cuts_labels <- levels(cuts_variable)
  
  # Check if variable is total or with decimals
  if (all(cut_breaks %% 1 == 0)) {
    # Variable is integer
    cuts_labels <- gsubfn('\\[\\s*(\\d+),\\s*(\\d+)[^0-9]+',
                          ~paste0(x, '-',as.numeric(y)-1),
                          as.character(cuts_labels))
  } else {
    # Variable is not integer
    # Create clean cuts
    cuts_labels <- gsubfn('\\[\\s*([0-9]+\\.*[0-9]*),\\s*(\\d+\\.\\d+).*',
                          ~paste0(x, '-', as.numeric(y)- 0.01),
                          as.character(cuts_labels))
  }
  
  # Clean Inf
  cuts_labels <- gsub("Inf", max(variable), cuts_labels)
  
  # Clean punctuation
  cuts_labels <- sub("\\[(.*), (.*)\\]", "\\1 - \\2", cuts_labels)
  
  # Replace strings with spaces
  cuts_labels <- gsub("-"," - ",cuts_labels, fixed = TRUE)
  
  # Trim white spaces
  cuts_labels <- trimws(cuts_labels)
  
  
  if (thousands.separator == TRUE) {
    cuts_labels <- sapply(strsplit(cuts_labels, " - "),
                          function(x) paste(prettyNum(x,
                                                      big.mark = ",",
                                                      preserve.width = "none"),
                                            collapse = " - "))
  }
  
  levels(cuts_variable) <- cuts_labels
  cuts_variable
}

# Rounding function
roundUp <- function(x, to = 10) {
  to*(x %/% to + as.logical(x %% to))
}

computeNetPay <- function(x) {
  
  # Attempt do destring variable if character
  if (!is.numeric(x)) {
    x <- as.character(x)
    x <- gsub(",", "", x = x, fixed = TRUE)
    x <- gsub("([[:digit:]]*)", "\\1", x = x)
    x <- as.numeric(x)
  }
  
  # Compute net pay
  y <- ifelse(test = x > 41866,
              yes = ((x - ((x - 41866) * 0.4)) - ((41866 - 10000) * 0.2)),
              no = ifelse(test = x > 10000,
                          yes = x - x * 0.2,
                          no = x)
  )
  
  # Substract national insurance
  y <- ifelse(test = x > 41444,
              yes = y - ((x - 41444) * 0.02) - ((41444 - 7448) *0.12),
              no = ifelse(test = x > 7748,
                          yes = y - ((x - 7448) * 0.12),
                          no = y)
  )
  
  # Return new values
  return(y)
}

# Destring
destring <- function(x) {
  ## convert factor to strings
  if (is.character(x)) {
    as.numeric(x)
  } else if (is.factor(x)) {
    as.numeric(levels(x))[x]
  } else if (is.numeric(x)) {
    x
  } else {
    stop("could not convert to numeric")
  }
}

#Generic Data Loading
shpsDzs01fort <- readRDS("S:/G - Governance & Performance Mngmt/Research Team/Generic R Tools/objects/dzs01_shps_f.RDS")