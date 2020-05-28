# Create Project Scripts
Easy to make new project by shellscript

![image](https://github.com/dapingtai/MDMAAT-web/blob/master/Create_MibProject_Frame/CreateProjectShellScript.png)
# Instructions For Use
## Step1: Name your microarray project to use.[template_list.txt](https://github.com/dapingtai/MDMAAT-web/blob/master/Create_MibProject_Frame/template_list.txt)
```shell
$ vim /srv/shiny-server/MDMAAT/Create_MibProject_Frame/template_list.txt
```
## Step2: Run [CreateProject.sh](https://github.com/dapingtai/MDMAAT-web/blob/master/Create_MibProject_Frame/CreateProject.sh)
```shell
$ bash /srv/shiny-server/MDMAAT/Create_MibProject_Frame/CreateProject.sh 0 3
Now reading the ProjectTemplate......
Choose template to make R (1.project.R/2.body.R/3.sidebar.R/4.server.R/5.custom.css/6.All): 6
Your template is in (/srv/shiny-server/MDMAAT/Create_MibProject_Frame/Template)
Producing script output in (/srv/shiny-server/MDMAAT/MIB_Project_Git/Create_MibProject_Frame/Output)
Start writing...........

Create Success!!
```
Ps. bash /srv/shiny-server/MDMAAT/Create_MibProject_Frame/CreateProject.sh 0 [Your template_list.txt Line - 1]
## Step3: Merge created template to main floder
```shell
$ cp /srv/shiny-server/MDMAAT/Create_MibProject_Frame/Output/. /srv/shiny-server/MDMAAT/
```
