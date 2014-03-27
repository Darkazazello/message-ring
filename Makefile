PROJECT = hello_world

DEPS = cowboy mongodb bson jsx mongrel
dep_cowboy = pkg://cowboy master
dep_mongodb = https://github.com/mongodb/mongodb-erlang master
dep_bson = https://github.com/blt/bson master
dep_jsx = https://github.com/talentdeficit/jsx.git master
dep_mongrel = https://github.com/Darkazazello/mongrel.git master
include ./erlang.mk
