Roi's cheatsheet
================

- which / whereis to find path of service

- msgpack
``` python
import msgpack
msgpack.packb([1,2,3])
msgpack.unpackb(_)
```

`knife search "recipe:dy-webserve"`
`knife ssh "recipe:dy-webserve" "sudo service chef-client stop" -P`
`knife ssh -i ~/.ssh/id_rsa_pass -x roi "recipe:dy-webserve" "sudo /sbin/service chef-client stop" -P`
## to print pretty json 
pipe python -m json.tool 
or 
alias prettyjson='python -m json.tool'
