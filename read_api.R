require(dplyr)
require(reshape2)
require(ggplot2)
require(lubridate)
require(jsonlite)
require(rjstat)
require(RCurl)
require(scales)
require(stringr)

## function to take the name of a dataset from Irelands CSO
## and return a data frame with correct dates, labels, values
## and good names for the fields
getJSONstat <-function (ds = "CD504"){
    ## return a data frame containing the json object from
    ## the usrl supplied
    ## this is designed to work with Irelands CSO web API
    url_cso <- paste0("http://www.cso.ie/StatbankServices/StatbankServices.svc/jsonservice/responseinstance/", ds)
    
    ## read the dataset from the cso website
    raw_txt  <- getURLContent(url_cso)

    ## converts the dataset into a json and a rjstat data.frame
    raw_json <- fromJSON(raw_txt)
    raw_json_stat <- fromJSONstat(raw_txt, use_factors=FALSE)
    ## clean up the names
    ## get the proper names from the json
    ds_raw_names <- raw_json$dataset$dimension$id
    ## make the lower case
    ds_raw_names <- tolower(ds_raw_names)
    ## add a name for the value field
    ds_raw_names[[length(ds_raw_names)+1]] <- "value"
    ## convert the list into a data.frame
    raw_df <- as.data.frame(raw_json_stat)
    ## update the names and replace spaces with underscores
    raw_names    <- gsub('\\s', '_', ds_raw_names)
    names(raw_df) <- raw_names
    
    ## make the time field a Date object
    ## find the name of the time role
    time_role <- gsub('\\s', '_', tolower(raw_json$dataset$dimension$role$time))
    ## for months need to make this work for all values year census year, quater, month week? half
    raw_df[[time_role]] <- ymd(paste0(gsub('[^0-9]*', '', raw_df[[time_role]]), "01"))
     ##   raw_df[[time_role]] <- ymd(paste0(raw_df[[time_role]], "0101"))
    ## make all the fields besides value and the time_role factors
    for(i in 1:length(raw_names)){

        if(raw_names[i] %in% c(time_role,"value"))
        {
            ## do nothing
        }else
        {

            raw_df[[raw_names[i]]] <- as.factor(raw_df[[raw_names[i]]])
        }
        }
    return(raw_df)
}

cso_time <- function(time_str){
    ## take a vector with time strings that Irelands cso.ie uses
    ## return a lubridate object
    ## starts and ends with 4 numbers and contains only numbers
    ## so 2015, 2010 
    if(grepl('^[0-9]{4}$', time_str)){
        ## year
        return(ymd(paste0(time_str, "0101")))
        ## month
        ## starts with 4 numbers then an 'M' ends with 2 numbers
        ## so 2014M03 3054M67
    } else if (grepl('^[0-9]{4}M[0-9]{2}$', time_str)){
        return(ymd(paste0(gsub('[^0-9]*', '', time_str), "01")))

        ## Quaters
        ## starts with 4 numbers then a 'Q' and ending with 1 number
    } else if (grepl('^[0-9]{4}Q[0-9]$', time_str)){
        ## extract the quater number which is the number the string ends with
        qtr <- str_extract(time_str, '[0-9]$')
        if(qtr == "1"){ # first quater YYYY0101            
            return(ymd(paste0(str_extract(time_str, '^[0-9]{4}'), "0101")))
        }else if(qtr == "2"){ # second quater YYYY0401
            return(ymd(paste0(str_extract(time_str, '^[0-9]{4}'), "0401")))
        }else if(qtr == "3"){ # third quater YYYY0701
            return(ymd(paste0(str_extract(time_str, '^[0-9]{4}'), "0701")))
        }else if(qtr == "4"){ # forth quater YYYY1001
            return(ymd(paste0(str_extract(time_str, '^[0-9]{4}'), "1001")))
        }
        
    } else if(grepl('^[0-9]{4}H[12]$', time_str)){
        half <- str_extract(time_str, '[12]$')
        if(half == "1"){
            return(ymd(paste0(str_extract(time_str, '^[0-9]{4}'), "0101")))
        } else if(half == "2"){
            return(ymd(paste0(str_extract(time_str, '^[0-9]{4}'), "0601")))
        }
    }
    cat("Error ", time_string, " dose not match a date in the format\n2015, 2015M01, 2015Q1, 2015H1\n")
}

## I fould my self copying code for the third time
## today so make it a function

clean_names <- function(bad_names){
    good_names <- gsub('\\s', '_', tolower(bad_names))
    return(good_names)
}

people_per_house <- getJSONstat()
## extract the number of persons per house from
## the label
num_per_house <-
    str_extract(as.character(people_per_house$persons_per_household),
                '[0-9]+')
## num_per_house <-
##     substr(as.character(people_per_house$persons_per_household),
##        regexpr('[0-9]+', as.character(people_per_house$persons_per_household)),
##        regexpr('[0-9]+', as.character(people_per_house$persons_per_household)) +
##        attr(regexpr('[0-9]+', as.character(people_per_house$persons_per_household)),'match.length')-1)
## converts it to a integer for ordering
people_per_house <- people_per_house %>% mutate(person_per_house = as.integer(num_per_house))

## order the factors so they appear in the legend in a sinsibal order
people_per_house$persons_per_household <-
    factor(people_per_house$persons_per_household,
           levels=c(
               "Persons - 1 person household"         
             , "Persons - 2 person household"         
             , "Persons - 3 person household"         
             , "Persons - 4 person household"         
             , "Persons - 5  person household"        
             , "Persons - 6 person household"         
             , "Persons - 7 person household"         
             , "Persons - 8 person household"         
             , "Persons - 9 person household"         
             , "Persons - 10 person household"        
             , "Persons - 11 person household"        
             , "Persons - 12 or more person household"
             , "All persons in private households"    
             , "All private households"))



people_per_house %>%
    arrange(person_per_house) %>%
    filter(persons_per_household !=
           "All persons in private households" ,
           persons_per_household !=
           "All private households") %>%               
    ggplot(aes(x=as.factor(person_per_house),
               y=value,
               fill =persons_per_household ))+
    geom_bar(stat="identity", position="dodge") +
    scale_y_continuous(labels = comma) +
    ggtitle("Number of Persons Per Household, Ireland (Census 2011)") +
    ylab("Number of Households") + xlab("Number of Persons per Household")
## just for fun we are going to find the top ten
## towns where we might find the love of our lifes

singles <- getJSONstat("CD521")
## we can filter on age sex and material status
## we'll keep both sexs to get 
for_me <- singles %>% filter(age_group == "25 - 44 years",
                             detailed_marital_status %in%
                             c("Single", "Widowed",
                               "Separated (including deserted)",
                               "Divorced"))

## what happens if we sum values with NA's
## don't even want to know
## make all NA'S 0
for_me$value <- ifelse(is.na(for_me$value), 0, for_me$value)
## get rid of all the featurs we are
## not intrested by summing them
slim_set <- for_me %>%
    group_by(sex, towns_by_size) %>%
    summarise(value = sum(value))

## make the data set wide so we can calculate
## the % Female and Male
wide_sex <- slim_set %>%
    dcast(towns_by_size~sex, value.var="value")

names(wide_sex) <- clean_names(names(wide_sex))
wide_sex <- wide_sex %>%
    mutate(per_male = male/both_sexes,
           per_female = female/both_sexes)
tester <- getJSONstat("HPM06")
