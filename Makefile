PROJECT = hello_world

DEPS = cowboy mongodb bson
dep_cowboy = pkg://cowboy master
dep_mongodb = https://github.com/mongodb/mongodb-erlang master
dep_bson = https://github.com/blt/bson master
include ./erlang.mk
