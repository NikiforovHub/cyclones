l = nrow(cyclones_base_cleared)
bool = logical(l)
chain_len = 1
for (i in 1:l-1){
  if (cyclones_base_cleared$ID[i] == cyclones_base_cleared$ID[i+1]){
    chain_len = chain_len + 1
  }else{
    if (Chain_len > 2){
      bool[(i - chain_len + 1):i] == TRUE
    }
    chain_len = 1
  }
}