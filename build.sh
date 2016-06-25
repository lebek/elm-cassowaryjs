#!/bin/sh
elm make Cassowary.elm
sed -i '' 's/style>/style><script type="text\/javascript" src="c.js"><\/script>/g' index.html
