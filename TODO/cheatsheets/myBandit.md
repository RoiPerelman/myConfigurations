Roi's Bandit Game
=================

## Passwords
* connect with `bandit.labs.overthewire.org -p 2220 -l bandit${level}` and pass the password
* `0 - 1` - boJ9jbbUNNfktd78OOpsqOltutMc3MY1
* `1 - 2` - CV1DtqXWVFXTvM2F0k09SHz0YwRINYA9
* `2 - 3` - UmHadQclWmgdLOKQ3YNgjWxGoRMb5luK
* `3 - 4` - pIwrPrtPN36QITSp3EQaw936yaFoFgAB
* `4 - 5` - koReBOKuIDDepwhWk7jZC0RTdopnAYKh
  * `for i in {0..9}; do cat inhere/-file0$i; printf "\n-------\n-------\n"; done`
* `5 - 6` - DXjZPULLxYr17uwoI01bNLQbtFemEgo7
  * `find . -size 1033c`
* `6 - 7` - HKBPTKQnIay4Fw76bEy8PVxKEDQRKTzs
  * `find / -size 33c -user bandit7 -group bandit6`
* `7 - 8` - cvX2JJa4CFALtqS87jk27qwqGhBM9plV
  * `cat data.txt | grep millionth`
* `8 - 9` - UsvVyFSfZZWbi6wgC7dAFyFuR6jQQUhR
  * `sort data.txt | uniq -u` or `sort data.txt | uniq -c` to show the count of duplicate lines
* `9 - 10` - truKLdjsbJ5g7yyJ2X2R0o3a5HQJFuLk
  * `strings data.txt | grep ^==========`
* `10 - 11` - IFukwKGsFW8MOq3IRFqrxE1hxTNEbUPR
  * `base64 -d data.txt`
* `11 - 12` - 5Te8Y4drgCRfCx8ugdwuEX8KFC6k2EUu
  * `cat data.txt | tr 'n-za-mN-ZA-M^C'a-zA-Z'` - which rotates the data 13 letters
* `12 - 13` - 8ZjyCRiBWFYkneahHwxCv3wb2a1ORpYL
  * used `xxd -r data.txt`, `file ${filename}`, `gzip -d`, `zcat`, `tar -xvf`, `bzip2 -d`
* `13 - 14` - 4wcYUJFw0k0XLShlDzztnTBHiqxU3b3e
  * `ssh -i sshkey.private localhost -l bandit14`
* `14 - 15` - BfMYroe26WYalil77FoDi9qh59eK5xNr
  * `echo 4wcYUJFw0k0XLShlDzztnTBHiqxU3b3e | nc localhost 30000`
* `15 - 16` -
  * ``
* `16 - 17` -
  * ``
* `17 - 18` -
  * ``
* `18 - 19` -
  * ``