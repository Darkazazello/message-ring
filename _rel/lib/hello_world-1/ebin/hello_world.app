%% Feel free to use, reuse and abuse the code in this file.

{application, hello_world, [
	{description, "Cowboy Hello World example."},
	{vsn, "1"},
	{modules, [hello_world_app, hello_world_sup, toppage_handler]},
	{registered, [hello_world_sup]},
	{applications, [
		kernel,
		stdlib,
		cowboy,
                mongodb,
                bson
	]},
	{mod, {hello_world_app, []}},
	{env, []}
]}.
