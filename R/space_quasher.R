space_quasher = function(x){res = x; while(gregexpr("  ",x)[[1]][1] != -1) {res = gsub("  "," ",x);x = res};return(res)}
