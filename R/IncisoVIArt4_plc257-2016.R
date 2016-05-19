library(relatorios);library(execucao);library(reest);library(data.table);library(magrittr)

desp <- exec_desp[ANO %in% 2010:2015 & primario_desp(exec_desp) & !(GRUPO_COD %in% 4:5)] %>% 
  .[,  sum(VL_EMP), ANO] %>% 
    .$V1

capital <- exec_desp[ANO %in% 2010:2015 & primario_desp(exec_desp) & GRUPO_COD %in% 4:5] %>% 
  .[,  sum(VL_EMP), ANO] %>% 
  .$V1

intra <- exec_desp[ANO %in% 2010:2015 & primario_desp(exec_desp) & !(GRUPO_COD %in% 4:5) & MODALIDADE_COD == 91] %>% 
  .[,  sum(VL_EMP), ANO] %>% 
    .$V1

transf_mun <- exec_desp[ANO %in% 2010:2015 & primario_desp(exec_desp) & !(GRUPO_COD %in% 4:5) & ACAO_COD == 7844] %>% 
  .[,  sum(VL_EMP), ANO] %>% 
    .$V1

rcl <- exec_rec[ANO %in% 2010:2015 & rcl(exec_rec)] %>% 
  .[, sum(VL_EFET_AJUST), ANO] %>% 
    .$V1

rec <- exec_rec[ANO %in% 2010:2015 & primario_rec(exec_rec)] %>% 
  .[, sum(VL_EFET_AJUST), ANO] %>% 
    .$V1

servico_divida <- exec_desp[ANO %in% 2010:2015 & GRUPO_COD %in% c(2, 6)] %>% 
  .[,  sum(VL_EMP), ANO] %>% 
    .$V1

dat <- rbind(desp = desp, 
             intra = intra, 
             transf_mun = transf_mun,
             rcl = rcl,
             rec = rec,
             servico_divida = servico_divida,
             capital = capital)

colnames(dat) <- 2010:2015

dat <- data.table(dat, keep.rownames = TRUE)

copia(dat)
