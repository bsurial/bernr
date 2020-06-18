
#' Framingham Risk Calculator (10 Year Cardiovascular risk)
#'
#' This functions calculates the 10-year cardiovascular risk based on the Framingham Risk Score. I used data from
#' \href{https://en.wikipedia.org/wiki/Framingham_Risk_Score}{Wikipedia}.
#'
#' @param gender "male" or "female"
#' @param age numeric
#' @param cholesterol numeric, default is mg/dL
#' @param smoke logical whether is smoking or not
#' @param hdl numeric, default is mg/dL
#' @param sysbp numeric, systolic blood pressure
#' @param bp_treated logical indicating whether received treatment for blood pressure
#' @param mg_dl default is TRUE, can be set to FALSE to calculate mmol/L
#'
#' @return character oject indicating 10% cardiovascular risk
#' @export
#'
#' @examples
#' fhrisk_10y(gender = "male", age = 60, cholesterol = 7,
#'            hdl = 1.2, sysbp = 120, bp_treated = TRUE,
#'            mg_dl = FALSE)
#'
fhrisk_10y <- function(gender, age, cholesterol, smoke, hdl, sysbp, bp_treated, mg_dl = TRUE) {

  if(mg_dl == TRUE) {
    hdl <- hdl
    cholesterol <- cholesterol
  } else {
    hdl <- 38.941 * hdl
    cholesterol <- 38.941 * cholesterol
  }

  # Age ---------------------------------------------------------------------

  fhrisk_age <- function(gender, age) {
    points <- 0
    if(gender == "female"){
      if(age <20 | age >= 80) {
        points <- NA
      } else if(age >= 20 & age < 35){
        points <- -7
      } else if(age >= 35 & age < 40) {
        points <- -3
      } else if(age >= 40 & age <45) {
        points <- 0
      } else if(age >= 45 & age <50) {
        points <- 3
      } else if(age >= 50 & age <55) {
        points <- 6
      } else if(age >= 55 & age <60) {
        points <- 8
      } else if(age >= 60 & age <65) {
        points <- 10
      } else if(age >= 65 & age <70) {
        points <- 12
      } else if(age >= 70 & age <75) {
        points <- 14
      } else if(age >= 75 & age <80) {
        points <- 16
      }

    } else if(gender == "male") {
      if(age <20 | age >= 80) {
        points <- NA
      } else if(age >= 20 & age < 35){
        points <- -9
      } else if(age >= 35 & age < 40) {
        points <- -4
      } else if(age >= 40 & age <45) {
        points <- 0
      } else if(age >= 45 & age <50) {
        points <- 3
      } else if(age >= 50 & age <55) {
        points <- 6
      } else if(age >= 55 & age <60) {
        points <- 8
      } else if(age >= 60 & age <65) {
        points <- 10
      } else if(age >= 65 & age <70) {
        points <- 11
      } else if(age >= 70 & age <75) {
        points <- 12
      } else if(age >= 75 & age <80) {
        points <- 13
      }
    }
    return(points)
  }



  # Cholesterol -------------------------------------------------------------

  fhrisk_chol <- function(gender, age, cholesterol) {
    points <- 0

    if(gender == "female"){
      if(age <20 | age >= 80) {
        points <- NA
      } else if(age >= 20 & age < 40){
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 4
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 8
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 11
        } else if(cholesterol >= 280) {
          points <- 13
        }

      } else if(age >= 40 & age <50){
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 3
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 6
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 8
        } else if(cholesterol >= 280) {
          points <- 10
        }

      } else if(age >= 50 & age <60){
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 2
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 4
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 5
        } else if(cholesterol >= 280) {
          points <- 7
        }

      } else if(age >= 60 & age <70) {
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 1
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 2
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 3
        } else if(cholesterol >= 280) {
          points <- 4
        }

      } else if(age >= 70 & age <80) {
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 1
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 1
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 2
        } else if(cholesterol >= 280) {
          points <- 2
        }
      }


    } else if(gender == "male") {
      if(age <20 | age >= 80) {
        points <- NA
      } else if(age >= 20 & age < 40){
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 4
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 7
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 9
        } else if(cholesterol >= 280) {
          points <- 11
        }

      } else if(age >= 40 & age <50){
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 3
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 5
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 6
        } else if(cholesterol >= 280) {
          points <- 8
        }

      } else if(age >= 50 & age <60){
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 2
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 3
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 4
        } else if(cholesterol >= 280) {
          points <- 5
        }

      } else if(age >= 60 & age <70) {
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 1
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 1
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 2
        } else if(cholesterol >= 280) {
          points <- 3
        }

      } else if(age >= 70 & age <80) {
        if(cholesterol < 160) {
          points <- 0
        } else if(cholesterol >= 160 & cholesterol < 200) {
          points <- 0
        } else if(cholesterol >=200 & cholesterol <240) {
          points <- 0
        } else if(cholesterol >= 240 & cholesterol <280) {
          points <- 1
        } else if(cholesterol >= 280) {
          points <- 1
        }
      }
    }
    return(points)
  }



  # Smoke -------------------------------------------------------------------
  fhrisk_smoke <- function(gender, age, smoke) {
    points <- 0
    if(gender == "female" & smoke == TRUE) {
      if(age <20 | age >= 80) {
        points <- NA
      } else if(age >= 20 & age < 40) {
        points <- 9
      } else if(age >= 40 & age < 50) {
        points <- 7
      } else if(age >= 50 & age < 60) {
        points <- 4
      } else if(age >= 60 & age <70) {
        points <- 2
      } else if(age >= 70 & age < 80) {
        points <- 1
      }

    } else if(gender == "male" & smoke == TRUE) {
      if(age <20 | age >= 80) {
        points <- NA
      } else if(age >= 20 & age < 40) {
        points <- 8
      } else if(age >= 40 & age < 50) {
        points <- 5
      } else if(age >= 50 & age < 60) {
        points <- 3
      } else if(age >= 60 & age <70) {
        points <- 1
      } else if(age >= 70 & age < 80) {
        points <- 1
      }
    }
    return(points)
  }



  # hdl ---------------------------------------------------------------------

  fhrisk_hdl <- function(hdl) {
    points <- 0
    if(hdl >= 60) {
      points <- -1
    } else if(hdl >= 50 & hdl <60) {
      points <- 0
    } else if(hdl >= 40 & hdl <50) {
      points <- 1
    } else if(hdl < 40) {
      points <- 2
    }
    return(points)
  }



  # sys_bp ------------------------------------------------------------------

  fhrisk_sbp <- function(gender, sysbp, bp_treated) {
    points <- 0
    if(gender == "female") {
      if(bp_treated == FALSE) {
        if(sysbp >= 120 & sysbp <130) {
          points <- 1
        } else if(sysbp >= 130 & sysbp <140) {
          points <- 2
        } else if(sysbp >= 140 & sysbp <160) {
          points <- 3
        } else if(sysbp > 160) {
          points <- 4
        }

      } else if(bp_treated == TRUE) {
        if(sysbp >= 120 & sysbp <130) {
          points <- 3
        } else if(sysbp >= 130 & sysbp <140) {
          points <- 4
        } else if(sysbp >= 140 & sysbp <160) {
          points <- 5
        } else if(sysbp > 160) {
          points <- 6
        }
      }
    }
    if(gender == "male") {
      if(bp_treated == FALSE) {
        if(sysbp >= 120 & sysbp <130) {
          points <- 0
        } else if(sysbp >= 130 & sysbp <140) {
          points <- 1
        } else if(sysbp >= 140 & sysbp <160) {
          points <- 1
        } else if(sysbp > 160) {
          points <- 2
        }

      } else if(bp_treated == TRUE) {
        if(sysbp >= 120 & sysbp <130) {
          points <- 1
        } else if(sysbp >= 130 & sysbp <140) {
          points <- 2
        } else if(sysbp >= 140 & sysbp <160) {
          points <- 2
        } else if(sysbp > 160) {
          points <- 3
        }
      }
    }
    return(points)
  }






  risk <- ""
  age_points <- fhrisk_age(gender = gender, age = age)

  chol_points <- fhrisk_chol(gender = gender, age = age, cholesterol = cholesterol)

  hdl_points <- fhrisk_hdl(hdl)

  sbp_points <- fhrisk_sbp(gender = gender, sysbp = sysbp, bp_treated = bp_treated)

  tot_points <- age_points + chol_points + hdl_points + sbp_points

  if(is.na(tot_points)) {
  risk <- "Not valiadated for age >= 80!"
  } else if(gender == "female") {
    if (tot_points < 9) {
      risk <- "<1%"
    } else if(tot_points >= 9 & tot_points <=12) {
      risk <- "1%"
    } else if(tot_points >= 13 & tot_points <=14) {
      risk <- "2%"
    } else if(tot_points == 15) {
      risk <- "3%"
    } else if(tot_points == 16) {
      risk <- "4%"
    } else if(tot_points == 17) {
      risk <- "5%"
    } else if(tot_points == 18) {
      risk <- "6%"
    } else if(tot_points == 19) {
      risk <- "8%"
    } else if(tot_points == 20) {
      risk <- "11%"
    } else if(tot_points == 21) {
      risk <- "14%"
    } else if(tot_points == 22) {
      risk <- "17%"
    } else if(tot_points == 23) {
      risk <- "22%"
    } else if(tot_points == 24) {
      risk <- "27%"
    } else if(tot_points >= 25) {
      risk <- ">30%"
    }
  }

  else if(gender == "male") {
    if (tot_points == 0) {
      risk <- "<1%"
    } else if(tot_points >= 1 & tot_points <=4) {
      risk <- "1%"
    } else if(tot_points >= 5 & tot_points <=6) {
      risk <- "2%"
    } else if(tot_points == 7) {
      risk <- "3%"
    } else if(tot_points == 8) {
      risk <- "4%"
    } else if(tot_points == 9) {
      risk <- "5%"
    } else if(tot_points == 10) {
      risk <- "6%"
    } else if(tot_points == 11) {
      risk <- "8%"
    } else if(tot_points == 12) {
      risk <- "10%"
    } else if(tot_points == 13) {
      risk <- "12%"
    } else if(tot_points == 14) {
      risk <- "16%"
    } else if(tot_points == 15) {
      risk <- "20%"
    } else if(tot_points == 16) {
      risk <- "25%"
    } else if(tot_points >= 17) {
      risk <- ">30%"
    }
  }

  return(risk)
}


