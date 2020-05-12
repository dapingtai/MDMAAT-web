#!/bin/bash
#Program
# 指定欄位擷取
# 指定欄位插入
# Version : TEST
echo -e "Now using createproject2.0"

templatefile=/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/CP2BodyTest.R
# cat ${templatefile} | sed -n "4,171p" | sed -r "s/TEMPLATE/Test/g"


find2startline=$(grep -n "### TEMPLATE" ${templatefile} | awk -F':' '{print $1}')
find2startline=$((${find2startline} -1))
find2endline=$(grep -n "### Resetpw" ${templatefile} | awk -F':' '{print $1}')
find2endline=$((${find2endline} - 2))

find1endline=$((${find2startline} - 1))
find3startline=$((${find2endline} + 1))
find3endline=$(cat ${templatefile} | wc -l )

cat ${templatefile} | sed -n "1,${find1endline}p"
cat ${templatefile} | sed -n "${find2startline},${find2endline}p" | sed -r "s/TEMPLATE/Test/g"
cat ${templatefile} | sed -n "${find3startline},${find3endline}p"