Hello world example
===================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/hello_world_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).
Create ring operation [http://localhost:8080/create?n=300](http://localhost:8080/create?n=300)
View active rings [http://localhost:8080/list](http://localhost:8080/list)
Push message to ring [http://localhost:8080/run?id=<id from active list>&m=100](http://localhost:8080/run?id=<id from active list>&m=100)
View log [http://localhost:8080/log]([http://localhost:8080/log)

Example output
--------------

``` bash
$ curl -i http://localhost:8080
HTTP/1.1 200 OK
connection: keep-alive
server: Cowboy
date: Fri, 28 Sep 2012 04:10:25 GMT
content-length: 12
content-type: application/json

[]
```
