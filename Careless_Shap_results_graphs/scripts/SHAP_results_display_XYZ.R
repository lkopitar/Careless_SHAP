# abs(results$filtered_results[results$filtered_results$variable_name=="res_psycsyn" & results$filtered_results$type == "careless misclass:no",]$contribution)+
#   abs(results$filtered_results[results$filtered_results$variable_name=="res_psycsyn" & results$filtered_results$type == "regular misclass:no",]$contribution)

results$filtered_results[results$filtered_results$variable_name=="res_psycsyn" & results$filtered_results$type == "careless misclass:no",]$contribution-
  results$filtered_results[results$filtered_results$variable_name=="res_psycsyn" & results$filtered_results$type == "careless misclass:no",]$ci

results$filtered_results[results$filtered_results$variable_name=="res_psycsyn" & results$filtered_results$type == "regular misclass:no",]$contribution+
  results$filtered_results[results$filtered_results$variable_name=="res_psycsyn" & results$filtered_results$type == "regular misclass:no",]$ci


abs(results$filtered_results[results$filtered_results$variable_name=="time_p5" & results$filtered_results$type == "careless misclass:no",]$contribution)+
  abs(results$filtered_results[results$filtered_results$variable_name=="time_p5" & results$filtered_results$type == "regular misclass:no",]$contribution)

results$filtered_results[results$filtered_results$variable_name=="time_p5" & results$filtered_results$type == "careless misclass:no",]$contribution+
  results$filtered_results[results$filtered_results$variable_name=="time_p5" & results$filtered_results$type == "careless misclass:no",]$ci

results$filtered_results[results$filtered_results$variable_name=="time_p5" & results$filtered_results$type == "regular misclass:no",]$contribution+
  results$filtered_results[results$filtered_results$variable_name=="time_p5" & results$filtered_results$type == "regular misclass:no",]$ci



abs(results$filtered_results[results$filtered_results$variable_name=="HE01_36" & results$filtered_results$type == "careless misclass:no",]$contribution)+
  abs(results$filtered_results[results$filtered_results$variable_name=="HE01_36" & results$filtered_results$type == "regular misclass:no",]$contribution)

results$filtered_results[results$filtered_results$variable_name=="HE01_36" & results$filtered_results$type == "careless misclass:no",]$contribution-
  results$filtered_results[results$filtered_results$variable_name=="HE01_36" & results$filtered_results$type == "careless misclass:no",]$ci

results$filtered_results[results$filtered_results$variable_name=="HE01_36" & results$filtered_results$type == "regular misclass:no",]$contribution-
  results$filtered_results[results$filtered_results$variable_name=="HE01_36" & results$filtered_results$type == "regular misclass:no",]$ci


abs(results$filtered_results[results$filtered_results$variable_name=="time_p3" & results$filtered_results$type == "careless misclass:no",]$contribution)+
  abs(results$filtered_results[results$filtered_results$variable_name=="time_p3" & results$filtered_results$type == "regular misclass:no",]$contribution)

results$filtered_results[results$filtered_results$variable_name=="time_p3" & results$filtered_results$type == "careless misclass:no",]$contribution-
  results$filtered_results[results$filtered_results$variable_name=="time_p3" & results$filtered_results$type == "careless misclass:no",]$ci

results$filtered_results[results$filtered_results$variable_name=="time_p3" & results$filtered_results$type == "regular misclass:no",]$contribution-
  results$filtered_results[results$filtered_results$variable_name=="time_p3" & results$filtered_results$type == "regular misclass:no",]$ci



results$filtered_results[results$filtered_results$variable_name=="time_p4" & results$filtered_results$type == "careless misclass:no",]$contribution-
  results$filtered_results[results$filtered_results$variable_name=="time_p4" & results$filtered_results$type == "careless misclass:no",]$ci

results$filtered_results[results$filtered_results$variable_name=="time_p4" & results$filtered_results$type == "careless misclass:yes",]$contribution+
  results$filtered_results[results$filtered_results$variable_name=="time_p4" & results$filtered_results$type == "careless misclass:yes",]$ci