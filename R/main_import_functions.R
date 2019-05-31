# This file contains the main import functions

#' Return a data.frame with metadata about a dataset
#'
#' @param dataset name of the dataset. Use get_available_datasets() to see options.
#' @return a data.frame containing metadata about the dataset.
#'
#' @export
read_metadata <- function(dataset){
  read.csv2(system.file("extdata",
                        paste0(dataset,'_files_metadata_harmonization.csv'),
                        package = "microdadosBrasil"),
            stringsAsFactors = FALSE, check.names = F) %>% data.frame

}


read_var_translator <- function(dataset, ft){
  read.csv2(system.file("extdata",
                        paste0(dataset,'_',ft,'_varname_harmonization.csv'),
                        package = "microdadosBrasil"), stringsAsFactors = FALSE, check.names =F)
}

count_occurrences <- function(str, expr) {
  return(lengths(regmatches(str, gregexpr(expr, str))))
}

#' @import readr
aux_read_fwf <- function(f,dic, nrows = -1L, na = "NA"){

  dict = nodic_overlap(dic)

  #print(f)

  aux_read<- function(f, dic,nrows = -1L, na = "NA"){

    f %>% read_fwf(fwf_positions(start=dic$int_pos,end=dic$fin_pos,col_names=dic$var_name),
                   col_types=paste(dic$col_type,collapse =''),n_max = nrows, na = na) -> d
    return(d)
  }

  lapply(dict, aux_read, f = f, nrows = nrows, na = na) %>% dplyr::bind_cols() -> d


  return(d)
}

#' Reads files (fwf or csv).
#'
#' Main import function. Parses metadata and import diciontaries (in case of fwf files) to obtain import parameters for the desired subdataset and period. Then imports based on those parameters. Should not be aceessed directly, unless you are trying to extend the package, but rather though the wrapper funtions (read_CENSO, read_PNAD, etc).
#' @param dataset .
#' @param ft file type. Indicates the subdataset within the dataset. For example: "pessoa" (person) or "domicílio" (household) data from the "CENSO" (Census). For a list of available ft for the period just type an invalid ft (Ex: ft = 'aasfasf')
#' @param i period. Normally period in YYY format.
#' @param metadata (optional) metadata for the dataset. If not provided it is obtained using the function read_metadata(dataset)
#' @param var_translator (optional) a data.frame containing a subdataset (ft) specific renaming dictionary. Rows indicate the variable and the columuns the periods.
#' @param root_path (optional) a path to the directory where dataset was downloaded
#' @param file (optional) file to read, ignore all metadata in this case
#' @param vars_subset (optional) read only selected variables( named on the dictionary for fwf files or in the first row for delimited files)
#' @param nrows (optional) read only n first rows
#' @param source_file_mark (optional) TRUE/FALSE , if T create a variable with the filename that the observation was imported from, useful for datasets with lots of separated files( CENSO and RAIS)
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @importFrom data.table data.table setnames rbindlist :=
#' @importFrom stats setNames
#' @export
read_data <- function(dataset,ft,i, metadata = NULL,var_translator=NULL,root_path=NULL, file=NULL, vars_subset = NULL, nrows = -1L, source_file_mark = F){

  # CRAN check
  if(F){
  . <- NULL
  source_file <- NULL
  }
  #Check for inconsistency in parameters
  # status:
  # 0 - Both root_path and file, error
  # 1 - No root_path ,no file
  # 2 - Only root_path
  # 3 - Only file
  status<-  test_path_arguments(root_path, file)
  if(status == 0){ stop()}

  if (!(dataset %in% get_available_datasets())) {
    stop(paste0(dataset, " is not a valid dataset. Available datasets are: ", paste(get_available_datasets(), collapse = ", "))) }


  if(is.null(metadata)){metadata<- read_metadata(dataset)}


  i_range<- get_available_periods(metadata)
  if (!(i %in% i_range)) { stop(paste0("period must be in ", paste(i_range, collapse = ", "))) }

  ft_list<- get_available_filetypes(metadata, i)
  if (!(ft %in% ft_list ))    { stop(paste0('ft (file type) must be one of these: ',paste(ft_list, collapse=", "),
                                            '. See table of valid file types for each period at "http://www.github.com/lucasmation/microdadosBrasil'))  }

  #names used to subset metadata data.frame
  ft2      <- paste0("ft_",ft)
  ft_list2 <- paste0("ft_",ft_list)

  var_list <- names(metadata)[ !(names(metadata) %in% ft_list2)]

  #subseting metadata and var_translator
  md <- metadata[metadata$period == i,] %>% select_(.dots =c(var_list,ft2))  %>% rename_(.dots=setNames(ft2,ft))
  if (!is.null(var_translator)) {
    vt <- var_translator %>% rename_( old_varname = as.name(paste0('varname',i)))
    vt <- vt[!is.na(vt$old_varname), c("std_varname", "old_varname")]
  }

  a <- md %>% select_(.dots = ft) %>% collect %>% .[[ft]]
  file_name <- unlist(strsplit(a, split='&'))[2]
  delim <- unlist(strsplit(a, split='&'))[1]  # for csv files
  format <- md %>% select_(.dots = 'format') %>% collect %>% .[['format']]
  missing_symbol <- md %>% select_(.dots = 'missing_symbols') %>% collect %>% .[['missing_symbols']]
  missing_symbol <- ifelse(test = is.na(missing_symbol), no = strsplit(missing_symbol,split = "&"), yes = "NA") %>% unlist
  # data_path <- paste0(root_path,"/",md$path,'/',md$data_folder)
  data_path <-  paste(c(root_path,md$path,md$data_folder)[!is.na(c(root_path,md$path,md$data_folder))] ,collapse = "/")
  if(data_path == ""){data_path <- getwd()}

  print(file_name)
  print(data_path)
  files <- list.files(path=data_path,recursive = TRUE, full.names = TRUE) %>% grep(pattern = paste0(file_name, "$"), value = T, ignore.case = T)
  print(files)


  if (!any(file.exists(files)) & status != 3) { stop("Data not found. Check if you have unziped the data" )  }


  #Importing
  if(status == 3){
    files = file
    if (!any(file.exists(file)) & status != 3) { stop("Data not found. Check if you have unziped the data" )  }
  }
  #print(format)
  t0 <- Sys.time()
  if(format=='fwf'){

    dic <- get_import_dictionary(dataset, i, ft)
    if(!is.null(vars_subset)){
      dic<- dic[dic$var_name %in% vars_subset,]
      if(dim(dic)[1] == 0){

        stop("There are no valid variables in the provided subset")
      }
    }


    lapply(files,function(x,...) aux_read_fwf(x, ...)%>% data.table %>% .[, source_file:= x], dic=dic, nrows = nrows, na = missing_symbol) %>% rbindlist  -> d
    #It could be removed after pull request https://github.com/tidyverse/readr/pull/632 be accepted
    if(any(dic$decimal_places) & dataset == "CENSO"){
      sapply(which(as.logical(dic$decimal_places)), function(x){
        if(dic$col_type[x] == "d"){
          var <- dic$var_name[x]

          d[, (var):= (d[, var,with = F] /(10**dic$decimal_places[x]))]
        }
      })
    }

  }
  if(format=='csv'){

    if(!is.null(vars_subset)){warning("You provided a subset of variables for a dataset that doesn't have a dictionary, make sure to provide valid variable names.", call. = FALSE)

      d <- lapply(files, function(x,...) data.table::fread(x,...) %>% .[, source_file := x], sep = delim, na.strings = c("NA",missing_symbol), select = vars_subset, nrows = nrows) %>% rbindlist(use.names = T)

    }else{

      d <- lapply(files, function(x,...) data.table::fread(x,...) %>% .[, source_file := x], sep = delim, na.strings = c("NA",missing_symbol), nrows = nrows) %>% rbindlist(use.names = T)

    }


  }


  t1 <- Sys.time()
  print(t1-t0)
  print(object.size(d), units = "Gb")

  #adjusting var names
  if (!is.null(var_translator)) {


    vt <- vt[vt$old_varname %in%  names(d) & vt$old_varname != "" & vt$std_varname != "",]
    setnames(d, vt$old_varname, vt$std_varname)

  }
  if(!source_file_mark & "source_file" %in% names(d)){

    d[, source_file:= NULL]
  }


  return(d)
}


#' Reads Censo Escolar .csv files with ff
#' @param ft file type. Indicates the subdataset within the dataset. For example: "pessoa" (person) or "domicílio" (household) data from the "CENSO" (Census). For a list of available ft for the period just type an invalid ft (Ex: ft = 'aasfasf')
#' @param i period: yyyy format
#' @param vars_subset string vector -- each element refers to a dataframe column
#' @param root_path  a path to the directory where dataset was downloaded
#' @import dplyr
#' @importFrom data.table data.table setnames rbindlist :=
#' @importFrom stats setNames
#' @export
read_data_censoescolar_ff <- function(ft, i, vars_subset = NULL, root_path){

  dataset <- "CensoEscolar"
  metadata<- read_metadata(dataset)
  ft_list<- get_available_filetypes(metadata, i)

  #names used to subset metadata data.frame
  ft2      <- paste0("ft_",ft)
  ft_list2 <- paste0("ft_",ft_list)
  var_list <- names(metadata)[ !(names(metadata) %in% ft_list2)]

  # subseting metadata and var_translator
  md <- metadata[metadata$period == i,] %>% select_(.dots =c(var_list,ft2))  %>% rename_(.dots=setNames(ft2,ft))

  # get files list
  a <- md %>% select_(.dots = ft) %>% collect %>% .[[ft]]
  file_name <- unlist(strsplit(a, split='&'))[2]
  delim <- unlist(strsplit(a, split='&'))[1]  # for csv files
  format <- md %>% select_(.dots = 'format') %>% collect %>% .[['format']]
  missing_symbol <- md %>% select_(.dots = 'missing_symbols') %>% collect %>% .[['missing_symbols']]
  missing_symbol <- ifelse(test = is.na(missing_symbol), no = strsplit(missing_symbol,split = "&"), yes = "NA") %>% unlist
  data_path <-  paste(c(root_path,md$path,md$data_folder)[!is.na(c(root_path,md$path,md$data_folder))] ,collapse = "/")
  if(data_path == ""){data_path <- getwd()}
  files <- list.files(path=data_path,recursive = TRUE, full.names = TRUE) %>% grep(pattern = paste0(file_name, "$"), value = T, ignore.case = T)

  # ensure the correct filetype gets opened
  if (ft == "docente") files <- files[count_occurrences(files, "DOCENTES_NORDESTE") > 0]
  if (ft == "matricula") files <- files[count_occurrences(files, "MATRICULA_NORDESTE") > 0]

  # some debugging
  print(delim)
  print(file_name)
  print(data_path)
  print(files)

  # Checking if microdados are extracted
  if (!any(file.exists(files))) { stop("Data not found. Check if you have unziped the data" )  }

  # load ff
  options(fftempdir=root_path)
  require(ff)
  require(ffbase)

  # read csv file with ff capabilities
  # some csv files will not be read with first.rows < 2e4...
  df <- read.csv2.ffdf(file = files[1], header=TRUE, VERBOSE=TRUE, sep=delim, first.rows=240000)

  # subset vars
  if (!is.null(vars_subset)) df <- df[vars_subset]

  return(df)
}










