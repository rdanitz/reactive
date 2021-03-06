#!/bin/sh

for i in */src/main/scala/*/*.scala
do
  gpg --yes --quiet --armor --symmetric --cipher-algo AES256 --passphrase-file secret.txt $i
done

for i in */src/main/scala/*/*/*.scala
do
  gpg --yes --quiet --armor --symmetric --cipher-algo AES256 --passphrase-file secret.txt $i
done
