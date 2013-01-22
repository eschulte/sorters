#!/usr/bin/awk
{ for(i=1;i<=NF;i++){ printf "%s ", toupper(substr($i,1,1)) substr($i,2)} printf "\n" }
