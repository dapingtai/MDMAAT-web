![GitHub](https://img.shields.io/github/license/dapingtai/MDMAAT-web)
![GitHub](https://img.shields.io/github/languages/top/dapingtai/MDMAAT-web)
![GitHub](https://img.shields.io/docker/image-size/zero102x/mdmaat-web)
# MDMAAT
- Microarry Data Management And Analysis Tool

   ---Watching your microarray data and starting easy analysis on website.---
# Overview Tool Function
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/MDMAAT_Function.jpg)

# Quick Start
```
docker pull zero102x/mdmaat-web
```
# Deployment
## Step1: Install Docker
- Linux - https://runnable.com/docker/install-docker-on-linux
- Window - https://docs.docker.com/docker-for-windows/install/
- Mac - https://docs.docker.com/docker-for-mac/install/

## Step2: Pull docker image ( Base image: Centos7 )
```shell
docker pull zero102x/mdmaat-web
```
## Step3: Run docker container 
```shell
docker run -dit -p 3838:3838 --name MDMAAT --privileged=true zero102x/mdmaat-web /usr/sbin/init
```
## Step4: Adjust server firewall
**Example in server Centos7**
```shell
firewall-cmd --permanent --zone=public --add-port=3838/tcp
firewall-cmd --reload
```
## Step5: Enter container
```shell
docker exec -it MDMAAT /bin/bash
```
#### Ps.If shiny-server unservice, try this command ####
```shell
systemctl start shiny-server
systemctl enable shiny-server
```
## Step6: Setting container DB
Our default is using localhost MariaDB in containter, if you want to change DB location, you can change host location in [login_info.text](https://github.com/dapingtai/MDMAAT-web/blob/master/login_info.text)

**Ps. Only Support MYSQL/MARIADB DO NOT USE OTHER DATABASE**
```shell
systemctl start mariadb
```
## Step7: Update MDMAAT-web version
```shell
cd /srv/shiny-server
git clone https://github.com/dapingtai/MDMAAT-web.git
```
# Default In Containter
- MariaDB(user="root", password="root")
- Databases in MariaDB(ArrayDB: "MicroarrayData", ShinyloginDB: "mib_shiny") 
- Shiny login(user="shiny", password="shiny")
- MDMAAT Floder location("/srv/shiny-server/MDMAAT")
# Create New Data In DataBase
### Quickly Creating (Using our shellscript)
View https://github.com/dapingtai/MDMAAT-web/tree/master/Create_MibProject_Frame
### Naming Principles For Table Name
- Microarray data: `{Your Platform abbreviated name}_{Your Data Name}_{DATA}`
- Microarray Expiration Date(Name/Date/Type): `{Your Platform abbrivated name}_{Your Data Name}_{EXP}`
### Abbreviate MicroArray Platform  
- Affy Human Genome U133 Plus 2.0 Array => AFFY
- Affy Mouse Genome 430 2.0 Array => M430
- Infinium Human Methylation 450K BeadChip => METH
- Infinium MethylationEPIC BeadChip => MEPIC
# Instructions For Using Website

## Data Processing
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/DataOverView.png)
## Step 1 Select Sample
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/SelectSample.png)
## Step 2 Choose Platform & View Feature
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/ChoosePlatform.png)
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/Features.png)
## Step 3 Select SampleA / SampleB
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/SelectGroupA.png)
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/SelectGroupB.png)
## Step 4 Choose Method & Start Analysis
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/SelectAnalysisMethod.png)
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/StartAnalysis.png)
## Step 5 Watching Resulte
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/Result.png)
### Data Visualization
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/Viewer1.png)
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/Viewer2.png)
## Step 6 Enrichment Analysis
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/EnrichmentControl.png)
![image](https://github.com/dapingtai/MDMAAT-web/blob/master/www/Instruction/EnrichmentAnalysis.png)
