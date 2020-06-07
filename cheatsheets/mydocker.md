Roi's Dockersheet
=================

## docker client

* docker start `[instance name]`
* docker stop `[instance name]`
* docker rm `[instance name]`
* docker run --name `[instance name]` `[image name]
  * -v `[local path]:[instance path]`. volumes
  * -p `[local port]:[instance port]`. ports
  * --rm [docker stop + docker rm on instance]
  * -d [run as daemon in the background, deteched mode]
* docker images
* docker ps [process status]
  * -a [all]
  * -p [process id]
* docker rmi [remove image]

*docker stop (docker ps -a -q)*

*docker rm (docker ps -a -q)*

*docker inspect --format '{{ .NetworkSettings.IPAddress }}' (docker ps -q)*

## Dockerfile

* docker build -t `[image name]` -f `[Dockerfile name]` `[relative path]`

dockerfile example:
```ruby
From ruby:1.9.3

RUN apt-get update -qq && apt-get install -y build-essential nodejs npm nodejs-legacy mysql-client vim

WORKDIR /tmp
# add is like copy, only untars and can handle URL's
ADD Gemfile Gemfile
COPY Gemfile.lock Gemfile.lock
RUN bundle install

EXPOSE 3000

CMD ["rails","server"]
```

## docker-compose

* docker-compose -f [yml file path] up -d
* docker-compose down

```yml
version: '2'
services:
  db:
    image: library/mysql:5.6.22
    volumes:
      - /Users/roiperelman/work/docker-persistent-storage/mysql:/var/lib/mysql
    expose:
      - 3306
    ports:
      - '3306:3306'
    env_file:
      - 'docker.env'
  web:
    build: .
    command: ../docker-start.sh
    ports:
      - '3333:3000'
    env_file:
      - 'docker.env'
    # links:
    #   - db
```

## fast dockers

### nginx
```ruby
docker-compose -f /Users/roiperelman/Work/docker/nginx/nginx-compose.yml up -d
docker-compose -f /Users/roiperelman/Work/docker/nginx/nginx-compose.yml down
```
### mysql (adm client password is root -e MYSQL_ROOT_PASSWORD=root)
```ruby
docker run --name mysql-server -p 3306:3306 -v /Users/roiperelman/Work/docker/mysql/data:/var/lib/mysql --env-file '/Users/roiperelman/Work/adm-redesign/docker.env' -d mysql:5.6.22
docker run -it --link mysql-server:mysql --rm mysql:5.6.22 bash -c 'exec mysql -h"$MYSQL_PORT_3306_TCP_ADDR" -P"$MYSQL_PORT_3306_TCP_PORT" -u root -p"$MYSQL_ENV_MYSQL_ROOT_PASSWORD"'
```
### phpMyAdmin
`docker run --name phpMyAdmin -d --link mysql-server:db -p 9001:80 phpmyadmin/phpmyadmin`

### redis
`docker run --name redis-server -p 6379:6379 -v /Users/roiperelman/Work/docker/redis/data:/data -v /Users/roiperelman/Work/docker/redis/redis.conf:/usr/local/etc/redis/redis.conf -d redis redis-server /usr/local/etc/redis/redis.conf --appendonly yes`

### elasticsearch (`-v $PWD/config":/usr/share/elasticsearch/config`)
`docker run --name elasticsearch -p 9200:9200 -v /Users/roiperelman/Work/docker/elasticsearch/data:/usr/share/elasticsearch/data -d elasticsearch1.4.3 bash -c 'exec elasticsearch'`

### DY-Admin
`docker run --name dy-admin --link mysql-server:db --link redis-server:redis -p 3333:3000 --env-file '/Users/roiperelman/Work/adm-redesign/docker.env' -d dy-admin`

### elasticsearch (1.6.2 for datafeeds)
`docker run  --name elasticsearch-1.6.2 -p 9200:9200 -p 9300:9300 -v /Users/roiperelman/Work/docker/elasticsearch-1.6.2/data:/usr/share/elasticsearch/data -d library/elasticsearch:1.6.2`