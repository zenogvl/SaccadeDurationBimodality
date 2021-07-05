#' gazepath
#'
#' Uses the gazepath function on all the raw eye-movement files in a folder.
#' These files have to be csv files with the following column names: LEFT_GAZE_X, LEFT_GAZE_Y, RIGHT_GAZE_X, RIGHT_GAZE_Y, Trial and Distance.
#'
#' @param PP Vector of participants numbers that corresponds with the raw eye-tracking data csv files.
#' @param dirIn The directory to the raw eye-tracking data csv files
#' @param dirResults The directory to the saccades and fixations.
#' @param filename The filename of the results
#' @param height_px Height of the stimuli in pixels. This is a gazepath argument. The defeault is 1024.
#' @param height_mm Height of the stimuli in mm. This is a gazepath argument. The defeault is 270.
#' @param width_px Width of the stimuli in pixels. This is a gazepath argument. The defeault is 1280.
#' @param width_mm Width of the stimuli in mm. This is a gazepath argument. The defeault is 330.
#' @param thres_dur The duration threshold. This is a gazepath argument. The defeault is 100.
#'
#' @return Saves a .csv file to the results directory
#' @export
#'
#' @import gazepath
useGazepath <- function(PP,
                        dirIn,
                        dirResults = dirIn,
                        filename,
                        height_px = 1024,
                        height_mm = 270,
                        width_px = 1280,
                        width_mm = 330,
                        thres_dur = 100){

  Fixations <- numeric()

  for(pp in PP){
    print(pp)
    Input <- read.csv(paste0(dirIn, pp, '.csv'))
    valid_trials <- rle(Input$Trial)$values[rle(Input$Trial)$lengths > 4000]
    data <- Input[Input$Trial %in% valid_trials,]
    data <- Input

    test <- gazepath(data, x1 = 'LEFT_GAZE_X', y1 = 'LEFT_GAZE_Y',
                     x2 = 'RIGHT_GAZE_X', y2 = 'RIGHT_GAZE_Y',
                     trial = 'Trial', d1 = 'Distance',
                     height_px = height_px, height_mm = height_mm,
                     width_px = width_px, width_mm = width_mm,
                     method = 'gazepath', thres_dur = thres_dur)

    ## save all fixations and saccades
    end_time <- rle(data$Trial)$len * 2
    sall <- summary(test)
    sall$onset_time <- rep.int((end_time - 8000)[rle(sall$Trial)$val], rle(sall$Trial)$len)
    sall$PP <- pp

    Fixations <- rbind(Fixations, sall)

  }

  write.csv(Fixations, file =  paste0(dirResults, filename, '.csv'))
}

