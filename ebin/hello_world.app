%% Feel free to use, reuse and abuse the code in this file.

{application, hello_world, [
	{description, "Cowboy Hello World example."},
	{vsn, "1"},
	{modules, [log_handler, hello_world_app, hello_world_sup, ring_handler_sup, ring_handler, main_sup, list_handler, toppage_handler, run_handler, create_handler]},
	{registered, [hello_world_sup,main_sup]},
	{applications, [
		kernel,
		stdlib,
		cowboy,
                mongodb,
                bson,
                jsx,
                mongrel
	]},
	{mod, {hello_world_app, []}},
	{env, []}
]}.
