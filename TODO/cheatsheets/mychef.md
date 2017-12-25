Roi's chefsheet
===============

## knife
* `knife status` - last time chef client ran
* `knife node list` - list all the nodes under knife
* `knife node show ${hostname} [--long]` - show all node attributes
* `knife node edit ${hostname}`
* `knife ssh "recipe:${recipe name} "${bash command}" -x ${user} -i ${ssh key path} -P` - lets you run bash command on all servers with the the recipe name.
  * `knife ssh "recipe:dy-webserve" "sudo /etc/init.d/chef-client stop" -x roi -i ~/.ssh/id_rsa_pass -P`
  * `knife ssh "recipes:dy-webserve\:\:recommendation-open-time" "sudo /etc/init.d/chef-client stop" -x roi -i ~/.ssh/id_rsa_pass -P`

## kitchen
- `kitchen list` - lists the platforms
- `kitchen converge ${platform}` - like vagrant up
