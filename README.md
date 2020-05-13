![GitHub](https://img.shields.io/github/license/dapingtai/MDMAAT-web)
![GitHub](https://img.shields.io/github/languages/top/dapingtai/MDMAAT-web)
![GitHub](https://img.shields.io/docker/image-size/zero102x/mdmaat-web)
# MDMAAT
- Microarry Data Management And Analysis Tool

   ---Watching your microarray data and starting easy analysis on website.---
# Overview Tool Function
![image](https://github.com/dapingtai/MDMAAT/blob/master/www/MDMAAT_Function.jpg)

# Quick Start
```
docker pull zero102x/mdmaat-web
```
# Deployment
## Step1: Install Docker
- Linux - https://runnable.com/docker/install-docker-on-linux
- Window - https://docs.docker.com/docker-for-windows/install/
- Mac - https://docs.docker.com/docker-for-mac/install/

## Step2: Pull docker image
```
docker pull zero102x/mdmaat-web
```
## Step3: Run docker container
```
docker run -dit -p 3838:3838 --name MDMAAT --privileged=true zero102x/mdmaat-web /usr/sbin/init
```
## Step4: Adjust server firewall
**Example**
```
firewall-cmd --permanent --zone=public --add-port=3838/tcp
firewall-cmd --reload
```
## Step5: Enter container
```
docker exec -it MDMAAT /bin/bash
```
#### Ps.If shiny-server unservice, try this command ####
```
systemctl start shiny-server
systemctl enable shiny-server
```
## Step6: Setting container DB
Our default is using localhost DB(user="root", password="root"), if you want to change DB location, you can change host location in [login_info.text](https://github.com/dapingtai/MDMAAT-web/blob/master/login_info.text)

**Ps. Only Support MYSQL/MARIADB DO NOT USE OTHER DATABASE**
```
systemctl start mariadb
```
## Step7: Update MDMAAT-web version
```
cd /srv/shiny-server
git clone https://github.com/dapingtai/MDMAAT-web.git
```
# Instructions for use
