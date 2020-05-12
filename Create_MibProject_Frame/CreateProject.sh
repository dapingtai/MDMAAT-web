#!/bin/bash
#Program:
#     This program is creating data project for MDMAAT(shiny)
#Version v1.2
#Author Hung Chang (woror000@stat.sincia.edu.tw/zero102x@gmail.com)
#How to use template List ?
#     Input paras in $1 text file list start(0) $2 text file list end (n)
#How to change template ?
#    You can change template(project.R/body.R/sidebar.R/server.R/custom.css) in Template file 

echo "Now reading the ProjectTemplate......"
read -p "Choose template to make R (1.project.R/2.body.R/3.sidebar.R/4.server.R/5.custom.css/6.All):" choosenum

templatelist="template_list.txt"
inputname=($(cat ${templatelist}))
nowdir=($(pwd)) 

echo "Now your current dictionary is (${nowdir})"
echo "Your template is in (${nowdir}/Template)"
echo "Producing script output in (${nowdir}/Output)"
echo "Start writing..........."

#1.create project
function createproject(){
    templatefile=${nowdir}/Template/ProjectTemplate.R
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do
        outputpath="${nowdir}/Output/projects/${inputname[${i}]}.R"
        cat ${templatefile} | sed -r "s/TEMPLATE/${inputname[${i}]}/g" > ${outputpath}
    done
}

#2.create body
function createbody(){
    templatefile=${nowdir}/Template/BodyTemplate.R
    outputpath="${nowdir}/Output/body.R"
    
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

#3.create siderbar
function createsidebar(){
    templatefile=${nowdir}/Template/SidebarTemplate.R
    outputpath="${nowdir}/Output/sidebar.R"
    
    find2startline=$(grep -n "# SIDERBAR" ${templatefile} | awk -F':' '{print $1}')
    find1endline=$((${find2startline} +1))
    find2startline=$((${find2startline} +2))
    find2endline=$(grep -n "# PUTTOGETHER" ${templatefile} | awk -F':' '{print $1}')
    find2endline=$((${find2endline} - 2))
    
    find3startline=$((${find2endline} + 1))
    find3endline=$(grep -n "menuItemOutput('TEMPLATE')" ${templatefile} | awk -F':' '{print $1}')
    menuItemOutputline=$((${find3endline}))
    find3endline=$((${find3endline} - 1))
    
    find4startline=$((${menuItemOutputline} + 1))
    find4endline=$(cat ${templatefile} | wc -l )
    
    cat ${templatefile} | sed -n "1,${find1endline}p" > ${outputpath}
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do            
        cat ${templatefile} | sed -n "${find2startline},${find2endline}p" | sed -r "s/TEMPLATE/${inputname[${i}]}/g" >> ${outputpath}
    done
    cat ${templatefile} | sed -n "${find3startline},${find3endline}p" >> ${outputpath}
    
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do            
        cat ${templatefile} | sed -n "${menuItemOutputline}p" | sed -r "s/TEMPLATE/${inputname[${i}]}/g" >> ${outputpath}
    done
        cat ${templatefile} | sed -n "${find4startline},${find4endline}p" >> ${outputpath}
    
}

#4.create server.R
function createserver(){
    templatefile=${nowdir}/Template/ServerTemplate.R
    outputpath="${nowdir}/Output/server.R"
    
    find1endline=$(grep -n "### PROJECTS" ${templatefile} | awk -F':' '{print $1}')
    find1endline=$((${find1endline} +1))
    soureprojectline=$((${find1endline} +1))
    find2startline=$((${soureprojectline} +1))
    find2endline=$(cat ${templatefile} | wc -l )
    
    cat ${templatefile} | sed -n "1,${find1endline}p" > ${outputpath}
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do            
        cat ${templatefile} | sed -n "${soureprojectline}p" | sed -r "s/TEMPLATE/${inputname[${i}]}/g" >> ${outputpath}
    done
    cat ${templatefile} | sed -n "${find2startline},${find2endline}p" >> ${outputpath}
}

#5.create css
function createcss(){
    templatefile=${nowdir}/Template/CustomTemplate.css
    outputpath="${nowdir}/Output/custom.css"
    
    find2startline=$(grep -n "Custom css" ${templatefile} | awk -F':' '{print $1}')
    find1endline=$((${find2startline} - 1))
    find2startline=$((${find2startline} +2))
    find2endline=$(cat ${templatefile} | wc -l )
    
    cat ${templatefile} | sed -n "1,${find1endline}p" > ${outputpath}
    for(( i=${1}; i <= ${2}; i=i+1 ))
    do            
        cat ${templatefile} | sed -n "${find2startline},${find2endline}p" | sed -r "s/TEMPLATE/${inputname[${i}]}/g" >> ${outputpath}
    done
}

if [ ${choosenum} == 1 ]; then
    createproject ${1} ${2}
elif [ ${choosenum} == 2 ]; then
    createbody ${1} ${2}
elif [ ${choosenum} == 3 ]; then
    createsidebar ${1} ${2}
elif [ ${choosenum} == 4 ]; then
    createserver ${1} ${2} 
elif [ ${choosenum} == 5 ]; then
    createcss ${1} ${2}
elif [ ${choosenum} == 6 ]; then
    createproject ${1} ${2}
    createbody ${1} ${2}
    createsidebar ${1} ${2}
    createserver ${1} ${2}
    createcss ${1} ${2}
else echo "Please input 1/2/3/4/5/6 to make R file"
     echo -e "\nCreate Failed!!"
     exit 0
fi
    
echo -e "\nCreate Success!!"
exit 0 