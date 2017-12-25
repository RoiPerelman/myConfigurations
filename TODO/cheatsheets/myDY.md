Roi's DYsheet
=============

## datafeeds-worker

* fish function to remove hash and change data feed to pending - `set_feed_as_pending 6183`

## deploy php

* manual php - web1-17.dynamicyield.com
    * check for one, than open 8 windows in tabs (shell - broadcast input)
    * `cd /var/www/dist/latest`
    * `.deploy_dy.sh`
    * `sudo service php-fpm restart`
    * check errors - `tail -f /var/log/php-fpm/www-error...` 

* new php servers - go into jenkins and build 
    * Project Deploy PHP Staging
    * Project Deploy PHP Production

## docker

* trigger feed indexing to local redis 63793 `http://localhost:8888/trigger_feed/6183`
* check status of feed indexing `http://localhost:8888/status` 