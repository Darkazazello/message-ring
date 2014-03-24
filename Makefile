PROJECT = hello_world

DEPS = cowboy mongodb bson mochijson2
dep_cowboy = pkg://cowboy master
dep_mongodb = https://github.com/mongodb/mongodb-erlang master
dep_bson = https://github.com/blt/bson master
dep_mochijson2 = https://github.com/bjnortier/mochijson2
include ./erlang.mk
