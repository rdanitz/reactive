#!/bin/sh

for i in */src/main/scala/*/*.asc
do
  src=$(echo $i | sed -e 's/.asc$//g')
  gpg --yes --quiet --decrypt --cipher-algo AES256 --passphrase-file secret.txt --output $src $i
done

for i in */src/main/scala/*/*/*.asc
do
  src=$(echo $i | sed -e 's/.asc$//g')
  gpg --yes --quiet --decrypt --cipher-algo AES256 --passphrase-file secret.txt --output $src $i
done
