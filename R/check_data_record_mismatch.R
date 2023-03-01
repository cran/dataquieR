#' Identify data sets data record mismatch
#'
#' @description
#' At first the intersect of id values of 2 datasets will be achived and if this list length is zero it means that datasets need a concatenation and possible error will be data element error . Otherwise the data record mismatch will be assesed.
#'
#' export
#' @noRd
#'
#' @param d1 [data.frame] first dataframe
#' @param d2 [data.frame] second dataframe
#' @param id [character] identification category name
#' @return A [integer] the output is not clear yet

check_data_record_mismatch <- function(d1, d2, id){

    valuesd1 = as.vector( d1[,c(id)] )
    valuesd2 = as.vector( d2[,c(id)] )
    nrow_d1 = nrow(d1)
    nrow_d2 = nrow(d2)


    dmerged = merge(x=d1,y=d2,by=id,all=TRUE)
    nrow_dm = nrow(dmerged)


    if (nrow_dm > nrow_d1){
        print('Data record mismatch')

        set_d1_min_d2 = list(setdiff(valuesd1,valuesd2))
        set_d2_min_d1 = list(setdiff(valuesd2,valuesd1))
        results = list( set_d1_min_d2, set_d2_min_d1)
        #print(c('mismatched ids in input_1: ', set_d1_min_d2))
        #print('"""""""""""""""""""')
        #print(c('mismatched ids in input_2: ', set_d2_min_d1))
    }
    else {
        print("No data record mismatch")
        set_d1_min_d2 = list(NULL)
        set_d2_min_d1 = list(NULL)
        results = list( set_d1_min_d2, set_d2_min_d1)
    }


    return(results)
}

