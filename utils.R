compile_book <- function(){
  bookdown::render_book('index.Rmd', 'all')
  #Creating a file called .nojekyll for github pages
  file.create('./docs/.nojekyll')
  generate_tables()
}



create_table_model <- function(stanfit, pars, renamepars){
  if(length(pars)!=length(renamepars)) print("ERROR different size vectors")
  hpdi <- get_HPDI_from_stanfit(stanfit)
  print("hpdi")
  print(hpdi)
  stanfit_summary <- as_tibble(summary(stanfit)$summary, rownames="Parameter")
  
  print("Stanfit summary")
  print(stanfit_summary)
  
  t1<-stanfit_summary %>% 
    dplyr::filter(Parameter %in% pars) %>% 
    select(Parameter)
  print("t1")
  print(t1)
  t2<-hpdi %>% 
    dplyr::filter(Parameter %in% pars)
  print("t2")
  print(t2)
  t<-left_join(x=t2,y=t1, by=c("Parameter")) %>% 
    dplyr::select(Parameter,Mean, everything()) %>% 
    dplyr::mutate(Parameter=renamepars)
  print(t)
  colnames(t)<-c("Parameter", "Mean", "HPDI.higher", "HPDI.lower", "$n_{eff}}$","\\hat{R}")
  return(t)
}

create_one_csv_from_folder<-function(savename, loadpath='./', savepath='./')
{
  all_files <- list.files(path=loadpath, full.names = T)
  dat_csv <- plyr::ldply(all_files, read_csv,
                         col_types = cols() #suppress the bunch of messages it generates
  )
  
  mergedcsv <-glue::glue(savepath,savename)
  write_csv(dat_csv,mergedcsv)
}

create_index <- function(x){
  index <- as.integer(as.factor(x))
  return(index)
}

get_index_names_as_array <- function(x){
  arr <- as.array(as.character(levels(as.factor(x))))
  return(arr)
}

print_stan_code <- function(filename)
{
  sourcecode <- paste(readLines(filename), collapse="\n")
  cat(sourcecode)
}


get_HPDI_from_stanfit<- function(stanfit)
{
  require(coda)
  hpdi<-coda::HPDinterval(coda::as.mcmc(as.data.frame(stanfit)))
  estimate<-as.data.frame(summary(stanfit)$summary)$mean
  df<-tibble::rownames_to_column(as.data.frame(hpdi), "Parameter")
  print("df")
  print(df)
  df.hpdi<-mutate(df,
                  Mean=as.data.frame(summary(stanfit)$summary)$mean,
                  Rhat=as.data.frame(summary(stanfit)$summary)$Rhat,
                  n_eff=as.data.frame(summary(stanfit)$summary)$n_eff) %>%
    rename(HPDI.lower=lower, HPDI.higher=upper)
  print("df.hpdi")
  print(df.hpdi)
  return(df.hpdi)
}

save_fig <- function(p, name, type="two-column"){
  path <- glue::glue('',name)
  
  if(type=="two-column")
  {
    ggsave(filename = path,
           width = 210,
           height = 70,
           units = "mm",
           plot = p,
           device = 'pdf')
  }
  
  if(type=="single-column")
  {
    ggsave(filename = path,
           width = 105,
           height = 80,
           units = "mm",
           plot = p,
           device = 'pdf')
  }
}

save_parameter_table <- function(in_path, out_path, caption="Estimated parameters of the model and the respective HPD interval", label, table.env="table", colnames,digits=2){
  kable(readRDS(in_path), 
        "latex", 
        table.envir = table.env,
        caption=caption, 
        booktabs=T,
        label=label,
        format.args = list(scientific = FALSE), 
        digits = digits,
        linesep = "") %>% 
    kable_styling(full_width = F, font_size = 7) %>% 
    readr::write_lines(out_path)
}

save_data_table <- function(in_path, out_path, caption, label,table.env="table", digits=2){
  kable(readRDS(in_path), 
        "latex", 
        table.envir = table.env,
        caption=caption, 
        booktabs=T,
        label=label,
        format.args = list(scientific = FALSE), digits = digits,
        linesep = "") %>% 
    kable_styling(full_width = F, font_size = 7) %>% 
    readr::write_lines(out_path)
}


generate_tables<-function(){
  
  print('Probability of success tables')
  save_data_table(in_path = './statscomp-paper/tables/datafortables/probsuccessmodeldata.RDS', 
                  out_path = './statscomp-paper/tables/probsuccessmodeldata.tex', 
                  caption = "Illustrating the data used in probability of success model (sample of four rows)",
                  label = 'probsuccessmodeldata',
                  table.env = "table")
  #THis one will need to be manually defined
  kable(readRDS('./statscomp-paper/tables/datafortables/probsuccess-par-table.RDS'), 
        "latex", 
        table.envir = 'table',
        caption='Estimated parameters of the model. OR indicates the odds ratio of the respective parameter',
        booktabs=T,
        label='probsuccesspartable',
        format.args = list(scientific = FALSE), digits = 2,
        linesep = "") %>% 
    column_spec(1,width = "1.4cm") %>% 
    column_spec(2,width = "0.7cm") %>% 
    column_spec(3,width = "0.7cm") %>% 
    column_spec(4,width = "0.7cm") %>% 
    column_spec(5,width = "0.7cm") %>% 
    column_spec(6,width = "0.7cm") %>% 
    column_spec(7,width = "0.7cm") %>% 
    kable_styling(full_width = F, font_size = 7) %>% 
    readr::write_lines('./statscomp-paper/tables/probsuccess-par-table.tex')
  
  print('Relative improvement tables')
  save_data_table(in_path = './statscomp-paper/tables/datafortables/relativeimprovementmodeldata.RDS', 
                  out_path = './statscomp-paper/tables/relativeimprovementmodeldata.tex', 
                  caption = "Illustrating the data used in relative improvement model (sample of four rows)",
                  label = 'relativeimprovementmodeldata',
                  table.env = "table")
  
  save_parameter_table(in_path =  './statscomp-paper/tables/datafortables/relativeimprovement-par-table.RDS',
                       out_path =  './statscomp-paper/tables/relativeimprovement-par-table.tex',
                       caption = "Estimated parameters of the relative improvement model and the respective HPD intervals",
                       label = 'relativeimprovementpartable' )
  
  print('Ranking data')
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/rankingtmodeldata.RDS', 
                  out_path = './statscomp-paper/tables/rankingtmodeldata.tex' , 
                  caption = "Illustrating the data used in the Bradley Terry model for ranking (sample of 6 rows)" ,
                  label =  'rankingtmodeldata',
                  table.env = "table")
  
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/rankingalgorithmsresults.RDS', 
                  out_path = './statscomp-paper/tables/rankingalgorithmsresults.tex' , 
                  caption = "Ranking the algorithms based on the reward difference taking accounting for the benchmarks" ,
                  label =  'rankingalgorithmsresults')
  
  save_parameter_table(in_path =  './statscomp-paper/tables/datafortables/ranking-par-table.RDS',
                       out_path =  './statscomp-paper/tables/ranking-par-table.tex',
                       caption = "Estimated parameters of the ranking model and the respective HPD intervals",
                       label = 'rankingpartable' )
  
  # 
  print('time to converge')
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/timetoconvergedata.RDS', 
                  out_path = './statscomp-paper/tables/timetoconvergedata.tex', 
                  caption = "Illustrating the data used in the Cox Proportional Hazards model time to converge to a solution (sample of 6 rows)",
                  label =  'timetoconvergedata',
                  table.env = "table")
  
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/hr_table.RDS', 
                  out_path = './statscomp-paper/tables/hr.tex', 
                  caption = "Average baseline coefficient and the average noise hazard ratio",
                  label =  'hr', digits = 3)
  
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/averagetimetoconverge.RDS', 
                  out_path = './statscomp-paper/tables/averagetimetoconverge.tex', 
                  caption = "Average time to converge in the noiseless scenario with in the average null impact of the benchmark function",
                  label =  'averagetimetoconverge', digits = 0)
  
  save_parameter_table(in_path =  './statscomp-paper/tables/datafortables/timetoconverge-par-table.RDS',
                       out_path =  './statscomp-paper/tables/timetoconverge-par-table.tex',
                       caption = "Estimated parameters of the time to converge model and the respective HPD intervals",
                       label = 'timetoconvergepartable' )
  
  
  #THis one will need to be manually defined
  kable(readRDS('./statscomp-paper/tables/datafortables/averagetimetoconverge_hr_table.RDS'), 
        "latex", 
        table.envir = 'table',
        caption='Average iteration to converge and the Hazard Ratio for the time to converge model',
        booktabs=T,
        label='averagetimetoconverge_hr_table',
        format.args = list(scientific = FALSE),
        digits = c(1,0,0,0,3,3,3),
        linesep = "") %>% 
    # column_spec(1,width = "1.2cm") %>% 
    # column_spec(2,width = "0.7cm") %>% 
    # column_spec(3,width = "0.7cm") %>% 
    # column_spec(4,width = "0.7cm") %>% 
    # column_spec(5,width = "0.7cm") %>% 
    # column_spec(6,width = "0.7cm") %>% 
    # column_spec(7,width = "0.7cm") %>% 
    add_header_above(c(" ", "Avg. iteration" = 3, "Hazard Ratio" = 2)) %>%
    kable_styling(full_width = F, font_size = 7) %>% 
    readr::write_lines('./statscomp-paper/tables/averagetimetoconverge_hr_table.tex')
  
  
  #Multiple groups comparison
  print('Multiple groups comparison')
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/multiplegroupsdata.RDS', 
                  out_path = './statscomp-paper/tables/multiplegroupsdata.tex', 
                  caption ="Illustrating the data used in the roobust multiple groups comparison (sample of 6 rows)",
                  label =  'multiplegroupsdata',
                  table.env = "table")
  
  save_parameter_table(in_path =  './statscomp-paper/tables/datafortables/multiplegroupsdifference-par-table.RDS', 
                       out_path = './statscomp-paper/tables/multiplegroupsdifference-par-table.tex', 
                       caption = "Estimated parameters of the multiple group comparison model and the respective HPD intervals",
                       label =  'multiplegroupsdifferenceartable')
  
  save_data_table(in_path =  './statscomp-paper/tables/datafortables/multiplegroupsdifference.RDS',
                  caption= "HPD interval for the difference between the groups",
                  out_path =  './statscomp-paper/tables/multiplegroupsdifference.tex',
                  label = 'multiplegroupsdifference' )
}
