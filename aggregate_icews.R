aggregate_icews = function(df){
	require(lubridate)
	require(dplyr)
	require(reshape2)
	
	var_actors = c('gov','opp','soc','ios','usa','reb')
	var_types = list(vercp = 1, matcp = 2, vercf = 3, matcf = 4)
	variables = c('gov_gov_vercp', 'gov_gov_matcp', 'gov_gov_vercf', 'gov_gov_matcf',
             'gov_gov_gold', 'gov_opp_vercp', 'gov_opp_matcp', 'gov_opp_vercf',
             'gov_opp_matcf', 'opp_gov_vercp', 'opp_gov_matcp', 'opp_gov_vercf',
             'opp_gov_matcf', 'opp_gov_gold', 'gov_reb_vercp', 'gov_reb_matcp',
             'gov_reb_vercf', 'gov_reb_matcf', 'gov_reb_gold', 'reb_gov_vercp',
             'reb_gov_matcp', 'reb_gov_vercf', 'reb_gov_matcf', 'reb_gov_gold',
             'gov_soc_vercp', 'gov_soc_matcp', 'gov_soc_vercf', 'gov_soc_matcf',
             'gov_soc_gold', 'soc_gov_vercp', 'soc_gov_matcp', 'soc_gov_vercf',
             'soc_gov_matcf', 'soc_gov_gold', 'gov_ios_vercp', 'gov_ios_matcp', 
             'gov_ios_vercf', 'gov_ios_matcf', 'gov_ios_gold', 'ios_gov_vercp',
             'ios_gov_matcp', 'ios_gov_vercf', 'ios_gov_matcf', 'ios_gov_gold',
             'gov_usa_vercp', 'gov_usa_matcp', 'gov_usa_vercf', 'gov_usa_matcf',
             'gov_usa_gold', 'usa_gov_vercp', 'usa_gov_matcp', 'usa_gov_vercf', 
             'usa_gov_matcf', 'usa_gov_gold', 'opp_reb_vercp', 'opp_reb_matcp', 
             'opp_reb_vercf', 'opp_reb_matcf', 'opp_reb_gold', 'reb_opp_vercp', 
             'reb_opp_matcp', 'reb_opp_vercf', 'reb_opp_matcf', 'reb_opp_gold', 
             'opp_opp_vercp', 'opp_opp_matcp', 'opp_opp_vercf', 'opp_opp_matcf',
             'opp_opp_gold', 'reb_reb_vercp', 'reb_reb_matcp', 'reb_reb_vercf', 
             'reb_reb_matcf', 'reb_reb_gold', 'opp_soc_vercp', 'opp_soc_matcp', 
             'opp_soc_vercf', 'opp_soc_matcf', 'opp_soc_gold', 'soc_opp_vercp', 
             'soc_opp_matcp', 'soc_opp_vercf', 'sco_opp_matcf', 'soc_opp_gold',
             'opp_ios_vercp', 'opp_ios_matcp', 'opp_ios_vercf', 'opp_ios_matcf',
             'opp_ios_gold', 'ios_opp_vercp', 'ios_opp_matcp', 'ios_opp_vercf', 
             'ios_opp_matcf', 'ios_opp_gold', 'opp_usa_vercp', 'opp_usa_matcp', 
             'opp_usa_vercf', 'opp_usa_matcf', 'opp_usa_gold', 'usa_opp_vercp', 
             'usa_opp_matcp', 'usa_opp_vercf', 'usa_opp_matcf', 'usa_opp_gold',
             'soc_ios_vercp', 'soc_ios_matcp', 'soc_ios_vercf', 'soc_ios_matcf',
             'soc_ios_gold', 'ios_soc_vercp', 'ios_soc_matcp', 'ios_soc_vercf', 
             'ios_soc_matcf', 'ios_soc_gold', 'soc_usa_vercp', 'soc_usa_matcp', 
             'soc_usa_vercf', 'soc_usa_matcf', 'soc_usa_gold', 'gov_gov_vercp', 
             'usa_soc_matcp', 'usa_soc_vercf', 'usa_soc_matcf', 'usa_soc_gold',
             'soc_soc_vercp', 'soc_soc_matcp', 'soc_soc_vercf', 'soc_soc_matcf',
             'soc_soc_gold')
    colnames(df) = c('date','iso1','cow1','agent1','iso2','cow2','agent2','cameo','goldstein','quad')
    df = filter(df, as.character(df$iso1) == as.character(df$iso2) | df$iso1 == 'USA' | df$iso2 == 'USA')
    all_actors = as.vector(unique(df$iso1))
    all_actors = all_actors[all_actors != 'USA' & all_actors != '---' & nchar(all_actors) == 3]
	df$Actor1Code = paste(df$iso1, df$agent1, sep='')
	df$Actor2Code = paste(df$iso2, df$agent2, sep='')
	df$date = as.Date(df$date)
	df$year = year(df$date)
	df$month = month(df$date)
	df = df[c(13,14,11,12,10,9)]
	
	# generating actor types #
	df$actor1type = NA
	df$actor2type = NA
	check1 = substr(df$Actor1Code, 4, 6) # pulling second 3 letter code
	check2 = substr(df$Actor2Code, 4, 6) # pulling second 3 letter code
	df$actor1type[check1 == 'OPP'] = 'OPP'
	df$actor2type[check2 == 'OPP'] = 'OPP'
	df$actor1type[check1== 'GOV' | check1=='MIL' | check1=='JUD' | 
	check1=='PTY'] = 'GOV'
	df$actor2type[check2== 'GOV' | check2=='MIL' | check2=='JUD' |
	check2=='PTY'] = 'GOV'
	df$actor1type[check1== 'REB' | check1=='INS'] = 'REB'
	df$actor2type[check2== 'REB' | check2=='INS'] = 'REB'
	df$actor1type[check1== 'EDU' | check1=='MED' | check1=='HLH' | 
	check1=='CVL' | check1=='BUS'] = 'SOC'
	df$actor2type[check2== 'EDU' | check2=='MED' | check2=='HLH' |
	check2=='CVL' | check2=='BUS'] = 'SOC'
	df$actor1type[check1=='NGO' | check1=='IGO'] = 'IOS'
	df$actor2type[check2=='NGO' | check2=='IGO'] = 'IOS'
	df$actor1type[check1=='USA'] = 'USA'
	df$actor2type[check2=='USA'] = 'USA'
	df = na.omit(df)
	
	print('Creating count variables...')
	for(name1 in var_actors){
		for(name2 in var_actors){
			for(var_type in names(var_types)){
				var_name = paste(name1, name2, var_type, sep='_')
				var_gold = paste(name1, name2, 'gold', sep='_')
				if(is.element(var_name, variables)){
					print(paste(var_name, 'is a variable', sep=' '))
					check1 = df$actor1type
					check2 = df$actor2type
					check3 = df$quad
					df[var_name] = 0
					df[[var_name]][check1 == toupper(name1) & check2 == toupper(name2) & check3
					== var_types[[var_type]]] = 1
				}
				if(is.element(var_gold, variables)){
					print(paste(var_gold, 'is a variable', sep='_'))
					df[var_gold] = 0
					df[var_gold] = df$goldstein
					check1 = df$actor1type
					check2 = df$actor2type
					df[[var_gold]][check1 != toupper(name1) | check2 != toupper(name2)] = 0
				}
			}
		}
	}
	final_df = data.frame()
	print('Creating groupings...')
	#for(actor in all_actors){
	foreach(actor=all_actors, .combine=rbind) %dopar% { 
		print(paste('Processing', actor, sep=' '))
		check1 = substr(df$Actor1Code, 1, 3)
		check2 = substr(df$Actor2Code, 1, 3)
		actor_dataset = filter(df, (check1==actor & check2==actor) | (check1==actor & check2=='USA') | (check1=='USA' & check2==actor))
		actor_dataset$Actor1Code = actor_dataset$Actor2Code = actor_dataset$quad = 
		actor_dataset$goldstein = actor_dataset$actor1type = actor_dataset$actor2type = NULL
		tryCatch(
			{
				actor_dataset$country = actor
				am = melt(actor_dataset, id.vars=c('country', 'month','year'), na.rm=TRUE)
				actor_dataset = dcast(am, country+month+year~variable, value.var='value', 
				fun.aggregate=sum)
				final_df = rbind(final_df, actor_dataset)
				},
				error=function(cond){
					message(paste('Skipped',actor, sep=' '))
					message(cond)
				},
				finally = {
					message(paste('Processed', actor, sep=' '))
				}
			)
	}
return(final_df)
}
