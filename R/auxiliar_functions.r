test_path_arguments<- function(root_path, file){
  #Check for inconsistency in parameters
  if(!is.null(root_path) & !is.null(file)){
    status = 0
    message(paste0("\nPlease, do not specify both the 'root_path' and 'file' parameters to the function. You can:\n",
                   "1) Specify neither the 'root_path' nor the 'file' argument, in this case we will assume that data is in your working directory and the files are named exactly as  they have been downloaded from the source.\n",
                   "2) Specify only the 'root_path' argument, in this case we will assume that data is in the directory specified and it is exactly as it have been downloaded from the source.\n"),
            "3) Specify only the 'file' argument, in this case we will assume that data is in a .txt or .csv file stored in the adress specified by the 'file' parameter.")
  }else{
    if(is.null(root_path) & is.null(file)){
      status = 1
      message(paste0("You haven't specified neither the 'root_path' nor ther 'file' parameters to the function. in this case we will assume that data is in your working directory and the files are named exactly as  they have been downloaded from the source.\n"))
    }else{
      if(is.null(file)){
        status = 2
        message(paste0("You have specified the 'root_path' argument, in this case we will assume that data is in the directory specified and it is exactly as it have been downloaded from the source.\n"))


      }else{
        status = 3
        message(paste0("You have specified the 'file' argument, in this case we will assume that data is in a .txt or .csv file stored in the adress specified by the 'file' parameter.\n"))
        if (!file.exists(file)) { stop("Data not found. Check if you have provided a valid address in the 'file' parameter" )  }
      }
    }
  }
  return(status)
}

#' Function to search and solve overlaping problems, returning a list of dictionaries.
#' @param dic The dictionary to be worked on.
#' @param init_pos The name of the column which contains the initial positions.
#' @param fin_pos The name of the column which contains the final positions.
#' @export
nodic_overlap <- function(dic, init_pos = "int_pos", fin_pos = "fin_pos"){
  dic = arrange(.data = dic, dic[[init_pos]])
  # Primeiro teste de overlap
  overlap.pos = which(dic[[init_pos]][-1] - dic[[init_pos]][-length(dic[[init_pos]])] < dic[[fin_pos]][-length(dic[[fin_pos]])] - dic[[init_pos]][-length(dic[[init_pos]])] + 1)
  print(overlap.pos)
  if(length(overlap.pos) > 0){
    dic.pos = dic
    dic.lis = list()
    dic.lis[[1]] = dic[-overlap.pos,]
    for(i in 1:length(overlap.pos)){
      dic.lis[[i+1]] = dic[overlap.pos[i],]
    }
  } else {
    dic.lis = list()
    dic.lis[[1]] = dic
  }
  i = 1:length(dic.lis)
  names(dic.lis) = paste("V", i, sep = "")

  return(dic.lis)
}


#' Returns available datasets int the package
#' @importFrom utils tail
#' @export
get_available_datasets <- function(){
  datasets_list<- list.files(system.file("extdata", package = "microdadosBrasil"), full.names = TRUE) %>%
    (function(x) return(grep("metadata_harmonization",x, value = T))) %>%
    str_split("/") %>%
    lapply(tail, c(n = 1)) %>%
    unlist %>%
    str_replace(pattern = "_.+", replacement = "")
  #datasets_list<- data(package = "microdadosBrasil")$results[,"Item"] %>%
  #gsub(pattern = "_dics", replacement = "")
  return(datasets_list)
}

#' Returns the periods for wich we have information about a dataset in the package
#'
#' @param  dataset name of the dataset. See get_available_datasets() for options.
#' @param  fwfonly (optional) TRUE/FALSE if TRUE returns only the periods for wich the dataset is distributed as a fixed width file.
#' @export
get_available_periods <- function(dataset, fwfonly = FALSE){
  md = is.data.frame(dataset)
  if(!md){
    dataset  = read_metadata(dataset)
  }
  if(!"period" %in% names(dataset)){
    warning("metadata in wrong format")
    return(NULL)
  }
  if(fwfonly){
    dataset = dataset %>% filter(format == "fwf")
  }
  periods = dataset$period
  return(periods)
}


#' Returns available filetypes for a dataset in a given period.
#'
#' @param  dataset name of the dataset. See get_available_datasets() for options.
#' @param  period  See get_available_periods(dataset) for options
#' @export
get_available_filetypes <- function(dataset, period){
  md = is.data.frame(dataset)
  if(!md){
    dataset  = read_metadata(dataset)
  }
  if(all(!grepl(pattern = "^ft_",names(dataset)))){
    warning("metadata in wrong format")
    return(NULL)
  }
  filetypes = dataset[ dataset$period == period,]
  filetypes = subset(filetypes, select = !is.na(filetypes)[1,]) %>% names
  filetypes = subset(filetypes, grepl(filetypes, pattern = "^ft_"))
  filetypes = gsub(filetypes, pattern = "^ft_", replacement = "")
  return(filetypes)
}

remove_comments<- function(data, keepLabels = F){
  remove_inline<- ifelse(keepLabels, I,
                         function(x) { x %>% gsub(pattern = "/\\*.+?\\*/" , replacement = "") %>%
                             gsub(pattern = ".+?\\*/" , replacement = "") %>%
                             gsub(pattern = "/\\*.+$" , replacement = "") })

  data %>% gsub(pattern = "^\\s*/\\*.+?\\*/\\s*$" , replacement = "") %>% remove_inline %>% return
}


#' @importFrom stringr str_trim
#' @importFrom stringr str_extract
#' @export
parse_SAS_import_dic <- function(file, keepLabels=F){

  #trick for NSE to pass CRAN check
  a <- int_pos <- decimal_places <- x <- NULL

  dic_sas   <- readLines(file) %>% remove_comments(keepLabels) %>% stringr::str_trim() %>% as.data.frame(stringsAsFactors = FALSE)

  names(dic_sas) <- 'a'
  dic_sas<-  dic_sas %>% filter(grepl("^@",a))

  labels<- stringr::str_extract(dic_sas$a, pattern = "/\\*.+?\\*/")

  dic_sas <- dic_sas  %>%
    tidyr::extract_("a", into=c('int_pos', 'var_name', 'x', 'label'),
                    "[[:punct:]\\s+](\\d+)\\s+(\\S+)(?:\\s+([[:graph:]$]+)())?")  %>%
    mutate_(int_pos= ~as.numeric(int_pos),
            length= ~gregexpr("[[:digit:]]+(?=\\.)",x,perl = TRUE) %>% regmatches(x = x) %>% as.numeric,
            decimal_places= ~gregexpr("(?<=\\.)[[:digit:]]+",x,perl = TRUE) %>% regmatches(x = x) %>% as.numeric) -> dic
  dic %>% mutate(
    decimal_places=ifelse(is.na(decimal_places),0,decimal_places),
    fin_pos= int_pos+length -1,
    col_type=ifelse(is.na(x),'c',
                    ifelse(grepl("\\$",x),'c',
                           ifelse(length<=9 & decimal_places==0,'i','d'))) ,
    CHAR=ifelse(grepl("\\$",x),TRUE,FALSE)
  ) -> dic

  estimated_length<- dic$int_pos %>% diff %>% c(0)
  dic$length[is.na(dic$length)]<- estimated_length
  estimated_final<- dic$int_pos + dic$length
  dic$fin_pos[is.na(dic$fin_pos)]<- estimated_final[is.na(dic$fin_pos)]

  dic<- dic %>% mutate(label = labels)

  dic %>% return
}

as.object_size <- function(x) structure(x, class = "object_size")
