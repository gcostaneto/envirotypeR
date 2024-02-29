#==================================================================================================
# Title.    : Estimates the potential environmental types and computs its frequency
# Author.   : G Costa-Neto
# Created at: 2022-10-01
# Updated at: 2023-11-28 (envirotypeR version 0.1.3)
# Current Version: 0.1.1 (envirotypeR)
#
# process_synthetic(), based on nasapower::get_power() from Sparks et al 20218
#==================================================================================================


#' @title  Estimates the potential environmental types and computs its frequency


fit_frequency <-function(env.data,
                         env.id = 'env',
                         var.id,
                         day.id = 'daysFromStart',
                         time.unit='f',
                         interaction.var=NULL,
                         as.frequency=TRUE,
                         quality.control=TRUE,
                         rm.duplicates=TRUE,
                         rm.lowMFF=TRUE,
                         duplicate.rate=TRUE,
                         digits=3,
                         percentile=0.05,
                         print.duplicates=FALSE,
                         seed=9182)
{



  if (!requireNamespace("parallel", quietly = TRUE)) {
    utils::install.packages("parallel")
    require(parallel)
  }



  message("---------------------------------------------------------------")
  message('fit_frequency - Calculating the frequency of environmental types')
  message('                over a given time period or geographical area')
  message("---------------------------------------------------------------")


  startTime <- Sys.time()
  message(paste0('Start at...........................', format(startTime, "%a %b %d %X %Y"),'\n'))
  #message(paste0('Number of Environmental Units ........',length( envs_to_pull)))

  output_features = .featureClassifier(env.data = env.data,env.id = env.id,var.id = var.id,day.id = day.id)

  output_features = .featureInteractions(dataInput = output_features, interaction_features = interaction.var)
  output_features = .featureByTimeWindow(dataInput = output_features,var.id = var.id,interaction_features = interaction.var,time.unit = time.unit)
  output_features = .featureFrequency(InputData = output_features,as.frequency = as.frequency,digits = digits)

  if(isTRUE(quality.control))
  {
    output_features = .featureQualityControl(InputMatrix = output_features,
                                             rm.duplicates = rm.duplicates,rm.lowMFF = rm.lowMFF,
                                             duplicate.rate = duplicate.rate,seed = seed,quality.control = quality.control,
                                             percentile = percentile,print.duplicated = print.duplicates)
  }

  message(paste0('\nEnvironmental Features...............',  ncol( output_features )))

  endTime <- Sys.time()
  message(paste0('Done!..............................', format(endTime, "%a %b %d %X %Y")))
  message(paste0('Total Time.........................',  round(difftime(endTime,startTime,units = 'secs'),2),'s'))

  output_features <- output_features[,sort(colnames(output_features))]
  return( output_features )
}


.featureClassifier<-function(env.data,env.id,var.id,day.id)
{
  # uses sub-function 1 outputs

  variables_under_study <- var.id

  require(foreach)
  output_typology =
    foreach::foreach(ID_VARIABLE = 1:length(variables_under_study),  .combine = "rbind") %dopar%
    {
      data.input = data.frame(
        env.data[,which(colnames(env.data)%in%day.id)],
        env.data[,which(colnames(env.data)%in%env.id)],
        env.data[,which(colnames(env.data)%in%variables_under_study[[ ID_VARIABLE ]]$name)],
        var_name = variables_under_study[[ID_VARIABLE]]$name)

      names(data.input) = c('day_id','env_id','env_variable','var_name')

      cardinal_breaks = NULL

      if(isTRUE(unique(any(grepl(names(variables_under_study[[ID_VARIABLE]]),pattern = 'breaks')))))
      {
        idLIST = grepl(names(variables_under_study[[ID_VARIABLE]]),pattern = 'breaks')
        idLIST = which(names(variables_under_study[[ID_VARIABLE]]) %in% names(variables_under_study[[ID_VARIABLE]])[idLIST])
        names(variables_under_study[[ID_VARIABLE]])[idLIST] = 'breaks'
        cardinal_breaks = variables_under_study[[ID_VARIABLE]]$breaks

      }

      output = envClassifier_0(dataInput = data.input,cardinal_breaks = cardinal_breaks)
      return(output)
    }
  return(output_typology)
}

.featureInteractions<-function(dataInput,interaction_features=NULL)
{

  if(is.null(interaction_features))
  {
    message(paste0('No feature interactions considered'))
    dataOut = data.frame(day_id=dataInput$day_id,env_id=dataInput$env_id,var_name=dataInput$var_name,bin=dataInput$bin)
    output_typology = data.frame( dataOut, source='raw')
    return(output_typology)

  }
  require(foreach)

  Features_under_interaction = c()
  for(i in 1:length(interaction_features))  Features_under_interaction=c(Features_under_interaction,interaction_features[[i]]$name)
  Features_under_interaction = unique(Features_under_interaction)

  output_typology =
    foreach::foreach(Intera = 1:length(interaction_features) ,   .combine = "rbind") %dopar%
    {
      #if(isTRUE(unique(any(grepl(names(interaction_features[[Intera]]),pattern = 'name')))))
      #{
      #  idLIST = grepl(names(interaction_features[[Intera]]),pattern = 'name')
      #  idLIST = which(names(interaction_features[[Intera]]) %in% names(interaction_features[[Intera]])[idLIST])
      #  names(interaction_features[[ID_VARIABLE]])[idLIST] = 'name'
      #  message('done1')
      # }


      if(length(interaction_features[[Intera]]$name[unique(!interaction_features[[Intera]]$name %in% dataInput$var_name )]) >0)
      {
        if(length(unique(dataInput$var_name %in% interaction_features[[Intera]]$name)) > 1)
        {

          missV=unique(!interaction_features[[Intera]]$name %in% dataInput$var_name)
          if(length(missV) == 1)
          {
            message(paste0('Not possible to compute the following interaction: ',intOut))
            message(paste0('Please give detail on the following variable on your input list:'))
            message('--------------------------')
            message(unique(interaction_features[[Intera]]$name )[missV])
            message('--------------------------')
          }
          if(length(missV) > 1)
          {
            intOut = interaction_features[[Intera]]$name[1]
            for(i in 2:length(interaction_features[[Intera]]$name)) intOut = paste(intOut,interaction_features[[Intera]]$name[i],sep = '_&_')
            message(paste0('Not possible to compute the following interaction: ',intOut))
            message(paste0('Please give detail on the following variables on your input list:'))
            message('--------------------------')
            for(i in 1:length(missV)) message(unique(interaction_features[[Intera]]$name )[missV][i])
            message('--------------------------')

          }

          dataOut = data.frame(day_id=dataInput$day_id,env_id=dataInput$env_id,var_name=dataInput$var_name,bin=dataInput$typology)
          return(dataOut)

        }
      }

      dataOut = dataInput[which(dataInput$var_name %in% interaction_features[[Intera]]$name),]
      dataOut = droplevels(dataOut)


      dataOut = reshape2::melt(dataOut,id.vars = c('env_id','day_id','var_name','bin'))
      dataOut  = reshape2::dcast(unique(dataOut  ,by=cols),env_id+day_id~var_name,fun.aggregate =function(x) paste(x[1]),value.var = "bin")


      idName = which(colnames(dataOut) %in% interaction_features[[Intera ]]$name[1])
      dataOut$combined = dataOut[,idName]
      featureName = colnames(dataOut)[idName]



      for(Factor in 2:length(interaction_features[[Intera ]]$name))
      {
        idName = which(colnames(dataOut) %in% interaction_features[[Intera]]$name[Factor])

        dataOut$combined = paste(dataOut$combined,dataOut[,idName],sep = '_&_')

        featureName =paste(featureName,colnames(dataOut)[idName],sep = '_&_')
      }

      names(dataOut)[which(names(dataOut) %in% 'combined')] = featureName
      dataOut = reshape2::melt(dataOut,id.vars=c('day_id','env_id'),value.name='bin',variable.name='var_name')

      return(dataOut)
    }

  # incorporing the past features and the features under interaction
  dataOut = dataInput[which(!dataInput$var_name %in%   Features_under_interaction),]
  dataOut = droplevels(dataOut)
  dataOut = data.frame(day_id=dataInput$day_id,env_id=dataInput$env_id,var_name=dataInput$var_name,bin=dataInput$bin)

  output_typology = rbind(output_typology,dataOut)

  output_typology$source = 'raw'
  output_typology$source[grepl(output_typology$var_name,pattern = '_&_')] = 'interaction'



  return(output_typology)

}


.intInteraction = function(x)
{
  featureNamesOut = c()
  for(i in 1:length(x))
  {
    featureName = x[[i]]$name[1]
    for(Factor in 2:length(x[[i]]$name)) featureName =paste(featureName,x[[i]]$name[Factor],sep = '_&_')
    featureNamesOut = c(featureNamesOut, featureName)
  }

  return(featureNamesOut)
}

.featureByTimeWindow<-function(dataInput,var.id,interaction_features=NULL,time.unit = 'DAE')
{


  check_out = interactionCheck(x = dataInput)

  varName     = check_out[[2]]
  sourceName  = check_out[[1]]


  variables_under_study <- var.id
  namesInpListRaw = names_InputList(variables_under_study)

  if(!is.null(interaction_features)) namesInpListInt = .intInteraction(interaction_features)

  require(foreach)

  output_typology =
    foreach::foreach(ID_VARIABLE = 1:length(varName),  .combine = "rbind") %dopar%
    {


      if(sourceName[ID_VARIABLE] == 'raw')
      {
        idL          = which(namesInpListRaw %in% varName[ID_VARIABLE])
        inputListBin = variables_under_study[[idL]]

      }
      if(sourceName[ID_VARIABLE] == 'interaction')
      {
        idL          = which(namesInpListInt %in% varName[ID_VARIABLE])
        interaction_features[[idL]]$name = varName[ID_VARIABLE]
        inputListBin = interaction_features[[idL]]

      }


      dataOut = dataInput[dataInput$var_name %in%  inputListBin$name,]
      dataOut = droplevels(dataOut)
      MaxDat = max(dataOut$day_id,na.rm=T)
      MinDat = min(dataOut$day_id,na.rm=T)


      checkWindow=inputWindowCheck(x = inputListBin,
                                   maxDayValue = MaxDat,minDayValue = MinDat ,window = 10)
      byInterval  = checkWindow[[2]]
      time_window = checkWindow[[1]]


      if(isTRUE(byInterval))
      {
        time_window = inputListBin$time.window
        if(is.null(time_window)) time_window = seq(from=MinDat,to=MaxDat,by=10)

        dataOut2   = dataOut[which(dataOut$day_id <= max(time_window,na.rm=T)),]
        dataOutput = data.frame(dataOut2,time_window=cut(dataOut2$day_id,breaks = time_window ,include.lowest = T))
        dataOutput$time_window = gsub(dataOutput$time_window,pattern = ')',replacement = ']',fixed = TRUE)
        dataOutput$time_window = gsub(dataOutput$time_window,pattern = '(',replacement = '[',fixed = TRUE)
        dataOutput$time_window = as.factor(paste('time_',dataOutput$time_window,'_',time.unit,sep = ''))
        dataOutput$environmental_bin = as.factor(paste(dataOutput$bin,dataOutput$time_window,sep = '_'))

      }


      if(isFALSE( byInterval))
      {
        dataOutput= dataOut
        dataOutput$time_window = 'No Interval'
        dataOutput$environmental_bin = as.factor(dataOutput$bin)

      }

      return(dataOutput)
    }


  return(output_typology)
}


.featureFrequency<-function(InputData, as.frequency=TRUE,digits=3)
{


  variables_in_df = unique(InputData$var_name)
  window_in_df    = unique(InputData$time_window)


  frequency_bins =
    foreach::foreach(IVAR= 1:length(variables_in_df),  .combine = "rbind") %:%
    foreach::foreach(IWIN = 1:length(window_in_df),  .combine = "rbind") %dopar%
    {
      data_selected = droplevels(InputData[InputData$var_name %in% variables_in_df[IVAR],])
      data_selected = droplevels(data_selected[data_selected$time_window %in% window_in_df [IWIN],])

      count_table = table(data_selected$env_id,data_selected$environmental_bin)
      if(isTRUE(as.frequency)) outputData = reshape2::melt(round(count_table/rowSums(count_table),digits = digits))
      if(isFALSE(as.frequency)) outputData = reshape2::melt(count_table)


      return(outputData)
    }
  frequency_bins = reshape2::acast( frequency_bins ,Var1~Var2,mean,value.var = 'value')

  return(frequency_bins)
}

.featureQualityControl<- function(InputMatrix, rm.duplicates=TRUE,rm.lowMFF=TRUE, duplicate.rate = 1, seed=8172,
                                  quality.control = TRUE,percentile = 0.01,print.duplicated=FALSE)
{
  cv = function(x) sd(x,na.rm=T)/mean(x,na.rm=T)
  output = InputMatrix
  if(isTRUE(rm.duplicates))
  {
    output_v1 = output
    old_ncol = ncol(output_v1)
    output_v2 = reshape2::dcast(reshape2::melt(output_v1),Var1~Var2,value.var = 'value')
    duplicated_names = colnames(output_v2 )[duplicated(as.list(output_v2 ))]
    if(length(duplicated_names) < 1)
    {
      message(paste0('Number of Removed Duplicated Features at DR of ',100*duplicate.rate,' % :',0,' from ',old_ncol,' Features'))

    }
    if(length(duplicated_names) >= 1)
    {
      set.seed(seed)
      duplicated_names = sample(duplicated_names,size = length(duplicated_names)*duplicate.rate,replace = F)
      output_v1 = output_v1[,-which(colnames(output_v1) %in% duplicated_names)]
      message(paste0('Number of Removed Duplicated Features at DR of ',100*duplicate.rate,' % :',length(duplicated_names),' from ',old_ncol,' Features'))
      output = output_v1

      if(isTRUE(print.duplicated)) message(paste0('\n',duplicated_names,'\n'))
    }


  }
  if(isTRUE(rm.lowMFF ))
  {
    old_ncol2 = ncol(output)
    MFF = quantile(apply(output ,2,cv),percentile)
    output = output[,apply(output ,2,cv) >= MFF]
    message(paste0('Number of Removed Features with Minor CV% <',round(MFF,3),' = percentile ',100*percentile,'%: ',old_ncol2-ncol(output),' from ',old_ncol2,' Features'))

  }

  return(output)
}


envClassifier_0<-function(dataInput,cardinal_breaks=NULL)
{
  if(is.null(cardinal_breaks)) cardinal_breaks <- unique(quantile(dataInput[,'env_variable'],c(0,0.1,0.25,0.5,0.75,0.90,1)))

  dataOutput = data.frame(day_id=dataInput[,'day_id'],env_id=dataInput[,'env_id'],var_name = dataInput[,'var_name'])

  #dataOutput$bin = as.character(cut(dataInput[,'env_variable'],breaks = cardinal_breaks,include.lowest = T))
  dataOutput$bin = cut(dataInput[,'env_variable'],breaks = cardinal_breaks,include.lowest = T)
  levels(  dataOutput$bin ) <- gsub(levels(  dataOutput$bin ),pattern = ')',replacement = ']',fixed = TRUE)
  levels(  dataOutput$bin ) <- gsub(levels(  dataOutput$bin ),pattern = '(',replacement = '[',fixed = TRUE)
  bin_levels <- levels(  dataOutput$bin )
  if(nlevels(dataOutput$bin) < 10)
  {
    seq_num <- paste0('0',1:nlevels(dataOutput$bin),sep='')
    levels(  dataOutput$bin ) <-  paste(   seq_num,levels(  dataOutput$bin ),sep = '_')

  }
  if(nlevels(dataOutput$bin) >= 10)
  {
    seq_num <- 1:nlevels(dataOutput$bin)
    levels(  dataOutput$bin ) <-  paste(seq_num,levels(  dataOutput$bin ),sep = '_')

  }

  levels(  dataOutput$bin ) <-  paste(unique(dataOutput$var_name),levels(  dataOutput$bin ),sep = '_t')

  #dataOutput$bin = gsub(dataOutput$bin,pattern = ')',replacement = ']',fixed = TRUE)
  #dataOutput$bin = gsub(dataOutput$bin,pattern = '(',replacement = '[',fixed = TRUE)
  #dataOutput$bin = paste(dataOutput$var_name,dataOutput$bin,sep = '_t')


  return(dataOutput)
}

interactionCheck<-function(x)
{
  .Summary=unique(data.frame(var_name = x$var_name,source=x$source))

  return(list(source=as.character(.Summary$source),var_name=as.character(.Summary$var_name)))

}

names_InputList<-function(x)
{
  names = c()
  for(i in 1:length(x)) names = c(names,x[[i]]$name)
  return(names)
}

inputWindowCheck<-function(x,maxDayValue,minDayValue,window=10)
{
  time_window = assist_function_timwindowcheck(x)
  byinterval = assist_function_intervalcheck(x)

  if(!is.null(time_window)) byinterval=TRUE

  if(isTRUE(byinterval)) # if by interval = TRUE,
  {
    if(is.null( time_window )) time_window = seq(from=minDayValue,to=maxDayValue,by=window) # but time.window is NULL

  }

  return(list(window=time_window,interval=byinterval))
}

assist_function_timwindowcheck<-function(x)
{

  timeWindow = NULL

  if(isTRUE(unique(any(grepl(names(x),pattern = 'window')))))
  {
    idLIST = grepl(names(x),pattern = 'window')
    idLIST = which(names(x) %in% names(x)[idLIST])
    names(x)[idLIST] = 'time.window'
    timeWindow = x$time.window
  }
  if(!isTRUE(unique(any(grepl(names(x),pattern = 'window')))))
  {
    timeWindow = NULL
  }

  return(timeWindow)

}



assist_function_intervalcheck<-function(x)
{

  byInterval = FALSE

  if(isTRUE(unique(any(grepl(names(x),pattern = 'interval')))))
  {
    idLIST = grepl(names(x),pattern = 'interval')
    idLIST = which(names(x) %in% names(x)[idLIST])
    names(x)[idLIST] = 'by.interval'
    byInterval = x$by.interval
  }
  if(!isTRUE(unique(any(grepl(names(x),pattern = 'interval')))))
  {
    byInterval = FALSE
  }

  return(byInterval)

}

featureFrequency<-function (InputData, as.frequency = TRUE, digits = 3)
{
  variables_in_df = unique(InputData$var_name)
  window_in_df = unique(InputData$time_window)
  frequency_bins = foreach::foreach(IVAR = 1:length(variables_in_df),
                                    .combine = "rbind") %:% foreach::foreach(IWIN = 1:length(window_in_df),
                                                                             .combine = "rbind") %dopar% {
                                                                               data_selected = droplevels(InputData[InputData$var_name %in%
                                                                                                                      variables_in_df[IVAR], ])
                                                                               data_selected = droplevels(data_selected[data_selected$time_window %in%
                                                                                                                          window_in_df[IWIN], ])
                                                                               count_table = table(data_selected$env_id, data_selected$environmental_bin)
                                                                               if (isTRUE(as.frequency))
                                                                                 outputData = reshape2::melt(round(count_table/rowSums(count_table),
                                                                                                                   digits = digits))
                                                                               if (isFALSE(as.frequency))
                                                                                 outputData = reshape2::melt(count_table)
                                                                               return(outputData)
                                                                             }
  frequency_bins = reshape2::acast(frequency_bins, Var1 ~ Var2,
                                   mean, value.var = "value")
  return(frequency_bins)
}



featureQualityControl<-function (InputMatrix, rm.duplicates = TRUE, rm.lowMFF = TRUE,
                                 duplicate.rate = 1, seed = 8172, quality.control = TRUE,
                                 percentile = 0.01, print.duplicated = FALSE)
{
  cv = function(x) sd(x, na.rm = T)/mean(x, na.rm = T)
  output = InputMatrix
  if (isTRUE(rm.duplicates)) {
    output_v1 = output
    old_ncol = ncol(output_v1)
    output_v2 = reshape2::dcast(reshape2::melt(output_v1),
                                Var1 ~ Var2, value.var = "value")
    duplicated_names = colnames(output_v2)[duplicated(as.list(output_v2))]
    if (length(duplicated_names) < 1) {
      message(paste0("Number of Removed Duplicated Features at DR of ",
                     100 * duplicate.rate, " % :", 0, " from ", old_ncol,
                     " Features"))
    }
    if (length(duplicated_names) >= 1) {
      set.seed(seed)
      duplicated_names = sample(duplicated_names, size = length(duplicated_names) *
                                  duplicate.rate, replace = F)
      output_v1 = output_v1[, -which(colnames(output_v1) %in%
                                       duplicated_names)]
      message(paste0("Number of Removed Duplicated Features at DR of ",
                     100 * duplicate.rate, " % :", length(duplicated_names),
                     " from ", old_ncol, " Features"))
      output = output_v1
      if (isTRUE(print.duplicated))
        message(paste0("\n", duplicated_names))
    }
  }
  if (isTRUE(rm.lowMFF)) {
    old_ncol2 = ncol(output)
    MFF = quantile(apply(output, 2, cv), percentile)
    output = output[, apply(output, 2, cv) >= MFF]
    message(paste0("Number of Removed Features with Minor CV% <",
                   round(MFF, 3), " = percentile ", 100 * percentile,
                   "%: ", old_ncol2 - ncol(output), " from ", old_ncol2,
                   " Features"))
  }


  return(output)
}



