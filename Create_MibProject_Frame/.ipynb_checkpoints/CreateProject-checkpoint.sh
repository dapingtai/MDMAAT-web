#!/bin/bash
#Program:
#     This program is creating data project for mib_project(shiny)
#Version v1.1
#Author Hung Chang (woror000@stat.sincia.edu.tw)
#How to use template List ?
#     Input paras in $1 text file list start(0) $2 text file list end (n)
#How to change template ?
#    You can change template(project.R/body.R/custom.css) in Template file 

echo "Now reading the ProjectTemplate......"
read -p "Choose template to make R (1.project.R/2.body.R/3.custom.css/4.All):" choosenum

templatelist="template_list.txt"
inputname=($(cat ${templatelist}))

#1.create project
function createproject(){
    templatefile=/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/Template/ProjectTemplate.R
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do
        # outputpath="/home/woror000/Project/Training/ShellScript/Mib_Project_Template/Output/${1}.R"
        outputpath="/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/Output/projects/${inputname[${i}]}.R"
        cat ${templatefile} | sed -r "s/TEMPLATE/${inputname[${i}]}/g" > ${outputpath}
    done
}

#2.create body
function createbody(){
    templatefile=/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/Template/BodyTemplate.R
    outputpath="/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/Output/body.R"
    
    find2startline=$(grep -n "### TEMPLATE" ${templatefile} | awk -F':' '{print $1}')
    find2startline=$((${find2startline} -1))
    find2endline=$(grep -n "### Resetpw" ${templatefile} | awk -F':' '{print $1}')
    find2endline=$((${find2endline} - 2))

    find1endline=$((${find2startline} - 1))
    find3startline=$((${find2endline} + 1))
    find3endline=$(cat ${templatefile} | wc -l )
    
    cat ${templatefile} | sed -n "1,${find1endline}p" > ${outputpath}
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do  
        cat ${templatefile} | sed -n "${find2startline},${find2endline}p" | sed -r "s/TEMPLATE/${inputname[${i}]}/g" >> ${outputpath}
    done
    cat ${templatefile} | sed -n "${find3startline},${find3endline}p" >> ${outputpath}
} 

#3.create css
function createcss(){
    templatefile=/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/Template/CustomTemplate.css
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do    
        outputpath="/home/woror000/Project/Training/ShellScript/Creat_MibProject_Frame/Output/custom.css"
        cat ${templatefile} | sed -r "s/TEMPLATE/${inputname[${i}]}/g" >> ${outputpath}
    done
}


if [ ${choosenum} == 1 ]; then
    createproject ${1} ${2}
elif [ ${choosenum} == 2 ]; then
    createbody ${1} ${2}
elif [ ${choosenum} == 3 ]; then
    createcss ${1} ${2} 
elif [ ${choosenum} == 4 ]; then
    createproject ${1} ${2}
    createbody ${1} ${2}
    createcss ${1} ${2}
else echo "Please input 1/2/3/4 to make R file"
fi
    
echo -e "\nCreate Success"
exit 0 