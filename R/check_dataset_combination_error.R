#' Identify data sets combination errors including data record mismatch and data element mismatch errors
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

check_dataset_combination_error <- function(d1, d2, id){

    valuesd1 = as.vector( d1[,c(id)] )
    valuesd2 = as.vector( d2[,c(id)] )
    nrow_d1 = nrow(d1)
    nrow_d2 = nrow(d2)

    intersect_ids = intersect(valuesd1,valuesd2)

    if (length(intersect_ids)>0 & nrow_d1==nrow_d2 ){
        dmerged = merge(x=d1,y=d2,by=id,all=TRUE)
        nrow_dm = nrow(dmerged)


        if (nrow_dm > nrow_d1){
            print('Error! Data record mismatch.')

            set_d1_min_d2 = list(setdiff(valuesd1,valuesd2))
            set_d2_min_d1 = list(setdiff(valuesd2,valuesd1))

            results = list(set_d1_min_d2, set_d2_min_d1)
            #print(c('mismatched ids in input_1: ', set_d1_min_d2))
            #print('"""""""""""""""""""')
            #print(c('mismatched ids in input_2: ', set_d2_min_d1))
        }
        else {
            print("No data record mismatch")
            set_d1_min_d2 = list(NULL)
            set_d2_min_d1 = list(NULL)
            results = list(set_d1_min_d2, set_d2_min_d1)
        }

    } else if (length(intersect_ids)==0){
        dmerged = merge(d1,d2)
        col_merged = names(dmerged)
        col_data1 = names(d1)
        col_data2 = names(d2)

        if (length(col_merged)>length(col_data1)){
            print('Error. Data element mismatch!')
            set_c1_min_c2 = list(setdiff(col_data1,col_data2))
            set_c2_min_c1 = list(setdiff(col_data2,col_data1))
            results = list(set_c1_min_c2, set_c2_min_c1)
            #print(c('mismatched columns in input_1: ', set_c1_min_c2))
            #print('"""""""""""""""""""')
            #print(c('mismatched columns in input_2: ', set_c2_min_c1))
        }
        else{
            print('No data element mismatch')
            set_c1_min_c2 = list(NULL)
            set_c2_min_c1 = list(NULL)
            results = list(set_c1_min_c2, set_c2_min_c1)
        }
    } else {
        print('None of errors could be the case!')
        results = list(list(NULL), list(NULL))
    }



    return(results)
}

