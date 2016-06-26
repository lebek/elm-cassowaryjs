#!/bin/sh
elm make App.elm
sed -i '' 's/\/title>/\/title><script type="text\/javascript" src="c.js"><\/script>/g' index.html
